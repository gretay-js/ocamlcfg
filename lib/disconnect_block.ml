(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = Cfg
module CL = Cfg_with_layout
module Int = Numbers.Int

let simplify_terminator (block : C.basic_block) =
  let t = block.terminator in
  match t.desc with
  | Branch successors ->
      (* Merge successors that go to the same label. Preserves the order of
         successors, except that successors sharing the same label target are
         grouped. *)
      (* CR-soon gyorsh: pairwise join of conditions is not canonical,
         because some joins are not representable as a condition. *)
      let labels_to_conds =
        List.fold_left
          (fun labels_to_conds (cond, label) ->
            let cond =
              match Int.Map.find_opt label labels_to_conds with
              | None -> [cond] (* Not seen this target yet *)
              | Some (joined_cond :: rest) ->
                  (* CR mshinwell: I think this needs a comment -- let's
                     discuss *)
                  Simplify_comparisons.disjunction cond joined_cond @ rest
              | Some [] ->
                  (* CR mshinwell: Why is this case impossible? *)
                  assert false
            in
            Int.Map.add label cond labels_to_conds)
          Int.Map.empty successors
      in
      let new_successors =
        Int.Map.bindings labels_to_conds
        |> List.map (fun (label, conds) ->
               List.map (fun cond -> (cond, label)) conds)
        |> List.concat
      in
      let cmp = List.compare_lengths new_successors successors in
      assert (cmp <= 0);
      if cmp < 0 then
        block.terminator <- { t with desc = Branch new_successors }
  | Switch labels -> (
      (* Convert simple [Switch] to branches. *)
      (* Find position k and label l such that label.(j) = l for all j =
         k..len-1. *)
      let len = Array.length labels in
      assert (len > 0);
      (* CR mshinwell: This should use [Misc.fatal_errorf] *)
      let l = labels.(len - 1) in
      let rec find_pos k =
        if k = 0 then 0
        else if labels.(k - 1) = l then find_pos (k - 1)
        else k
      in
      let k = find_pos (len - 1) in
      assert (k >= 0 && k < len);
      match k with
      | 0 ->
          (* All labels are the same and equal to l *)
          block.terminator <- { t with desc = Branch [(Always, l)] }
      | 1 ->
          let t0 = C.Test (Iinttest_imm (Iunsigned Clt, 1)) (* arg < 1 *) in
          let t1 = C.Test (Iinttest_imm (Iunsigned Cge, 1)) (* arg >= 1 *) in
          block.terminator <-
            { t with desc = Branch [(t0, labels.(0)); (t1, l)] }
      | 2 ->
          let t0 = C.Test (Iinttest_imm (Iunsigned Clt, 1)) (* arg < 1 *) in
          let t1 = C.Test (Iinttest_imm (Iunsigned Ceq, 1)) (* arg = 1 *) in
          let t2 = C.Test (Iinttest_imm (Iunsigned Cgt, 1)) (* arg > 1 *) in
          block.terminator <-
            { t with
              desc = Branch [(t0, labels.(0)); (t1, labels.(1)); (t2, l)]
            }
      | _ -> () )
  | _ -> ()

let update_predecessor's_terminators (cfg : C.t) ~pred_label
    ~being_disconnected ~target_label =
  let replace_label l =
    if l <> being_disconnected then l else target_label
  in
  let replace_successor (cond, l) = (cond, replace_label l) in
  let pred_block = Label.Tbl.find cfg.blocks pred_label in
  (* CR mshinwell: Consider adding [Cfg.map_successors]. *)
  ( match pred_block.terminator.desc with
  | Branch successors ->
      let new_successors = List.map replace_successor successors in
      pred_block.terminator <-
        { pred_block.terminator with desc = Branch new_successors }
  | Switch labels ->
      let new_labels = Array.map replace_label labels in
      pred_block.terminator <-
        { pred_block.terminator with desc = Switch new_labels }
  | Tailcall (Self _) ->
      if !C.verbose then
        Printf.printf
          "disconnect fallthrough %d: pred.terminator=Tailcall Self entry=%d\n"
          being_disconnected cfg.fun_tailrec_entry_point_label;
      assert (
        Label.equal being_disconnected cfg.fun_tailrec_entry_point_label );
      C.set_fun_tailrec_entry_point_label cfg target_label
  | Return | Raise _ | Tailcall (Func _) -> () );
  simplify_terminator pred_block

let disconnect cfg_with_layout label =
  let cfg = CL.cfg cfg_with_layout in
  let block = C.get_and_remove_block_exn cfg label in
  let has_predecessors = not (Label.Set.is_empty block.predecessors) in
  let has_more_than_one_successor =
    match Cfg.successor_labels cfg block with
    | [] | [_] -> false
    | _ :: _ -> true
  in
  if has_more_than_one_successor && has_predecessors then
    Misc.fatal_errorf
      "Cannot disconnect block %a: it has more than one successor and at \
       least one predecessor"
      Label.print label;
  (* Update successor blocks. *)
  List.iter
    (fun succ ->
      let succ_block = C.get_block_exn cfg succ in
      assert (Label.Set.mem label succ_block.predecessors);
      succ_block.predecessors <-
        Label.Set.union
          (Label.Set.remove label succ_block.predecessors)
          block.predecessors)
    (Cfg.successor_labels cfg block);
  (* Update predecessor blocks. *)
  ( match Cfg.successor_labels cfg block with
  | [target_label] ->
      Label.Set.iter
        (fun pred_label ->
          update_predecessor's_terminators cfg ~pred_label
            ~being_disconnected:label ~target_label)
        block.predecessors
  | [] | _ :: _ -> assert (Label.Set.is_empty block.predecessors) );
  (* Remove from layout and other data-structures that track labels. *)
  let layout = CL.layout cfg_with_layout in
  let new_layout = List.filter (fun l -> l <> label) layout in
  CL.set_layout cfg_with_layout new_layout;
  (* CR mshinwell: I don't think this property will necessarily hold in the
     future. *)
  (* If the dead block contains [Pushtrap], its handler becomes dead. Find
     all occurrences of the label in trap_labels and remove them. *)
  (* CR mshinwell: This next part doesn't seem to actually remove the trap
     handler blocks. (Also, should it remove trap handlers transitively?) *)
  CL.filter_trap_labels cfg_with_layout ~f:(fun ~pushtrap_lbl ->
      pushtrap_lbl <> label);
  CL.remove_from_trap_depths cfg_with_layout label;
  CL.remove_from_new_labels cfg_with_layout label;
  (* CR mshinwell: The next two lines should move to e.g.
     [Cfg.invalidate_block] together with an associated test for that,
     perhaps. When is such a test used? If it is not used, can we skip this
     procedure? *)
  block.terminator <- { block.terminator with desc = Branch [] };
  block.predecessors <- Label.Set.empty
