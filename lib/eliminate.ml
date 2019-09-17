(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*                         based on the work of                           *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* Simplify CFG *)
(* CR-soon gyorsh: needs more testing. *)

open Cfg
module M = Numbers.Int.Map

let simplify_terminator block =
  let t = block.terminator in
  match t.desc with
  | Branch successors ->
      (* Merge successors that go to the same label. Preserve order of
         successors, except successors that share the same label target are
         grouped. *)
      (* Map label to list of conditions that target it. *)
      (* CR-soon gyorsh: pairwise join of conditions is not canonical,
         because some joins are not representable as a condition. *)
      let map =
        List.fold_left
          (fun map (c, l) ->
            let s =
              match M.find_opt l map with
              | None -> [ c ] (* Not seen this target yet *)
              | Some (c1 :: rest) -> Simplify_comparisons.disjunction c c1 @ rest
              | Some [] -> assert false
            in
            M.add l s map)
          M.empty successors
      in
      let new_successors, map =
        List.fold_left
          (fun (res, map) (_, l) ->
            match M.find_opt l map with
            | None -> (res, map)
            | Some s ->
                let map = M.remove l map in
                let res =
                  List.fold_left (fun res c -> (c, l) :: res) res s
                in
                (res, map))
          ([], map) successors
      in
      assert (M.is_empty map);
      let new_len = List.length new_successors in
      let len = List.length successors in
      assert (new_len <= len);
      if new_len < len then
        block.terminator <- { t with desc = Branch new_successors }
  | Switch labels -> (
      (* Convert simple case to branches. *)
      (* Find position k and label l such that label.(j)=l for all
         j=k...len-1. *)
      let len = Array.length labels in
      assert (len > 0);
      let l = labels.(len - 1) in
      let rec find_pos k =
        if k = 0 then k
        else if labels.(k - 1) = l then find_pos (k - 1)
        else k
      in
      let k = find_pos (len - 1) in
      match k with
      | 0 ->
          (* All labels are the same and equal to l *)
          block.terminator <- { t with desc = Branch [ (Always, l) ] }
      | 1 ->
          let t0 = Test (Iinttest_imm (Iunsigned Clt, 1)) (* arg < 1 *) in
          let t1 = Test (Iinttest_imm (Iunsigned Cge, 1)) (* arg >= 1 *) in
          block.terminator <-
            { t with desc = Branch [ (t0, labels.(0)); (t1, l) ] }
      | 2 ->
          let t0 = Test (Iinttest_imm (Iunsigned Clt, 1)) (* arg < 1 *) in
          let t1 = Test (Iinttest_imm (Iunsigned Ceq, 1)) (* arg = 1 *) in
          let t2 = Test (Iinttest_imm (Iunsigned Cgt, 1)) (* arg > 1 *) in
          block.terminator <-
            {
              t with
              desc = Branch [ (t0, labels.(0)); (t1, labels.(1)); (t2, l) ];
            }
      | _ -> () )
  | _ -> ()

(* CR-soon gyorsh: eliminate transitively blocks that become dead from this
   one. *)
let eliminate_dead_block cfg dead_blocks label =
  let block = Hashtbl.find cfg.blocks label in
  Hashtbl.remove cfg.blocks label;

  (* Update successor blocks of the dead block *)
  List.iter
    (fun target ->
      let target_block = Hashtbl.find cfg.blocks target in
      (* Remove label from predecessors of target. *)
      target_block.predecessors <-
        LabelSet.remove label target_block.predecessors)
    (successor_labels block);

  (* Remove from layout and other data-structures that track labels. *)
  t.layout <- List.filter (fun l -> l <> label) t.layout;

  (* If the dead block contains Lpushtrap, its handler becomes dead. Find
     all occurrences of label as values of trap_labels and remove them,
     because is_trap_handler depends on it. *)
  Hashtbl.filter_map_inplace
    (fun _ lbl_pushtrap_block ->
      if label = lbl_pushtrap_block then None else Some lbl_pushtrap_block)
    t.trap_labels;

  (* Not necessary to remove it from trap_depths, because it will only be
     accessed if found in the cfg, but remove for consistency. *)
  Hashtbl.remove t.trap_depths label;

  (* Return updated list of eliminated blocks. CR-soon gyorsh: update this
     when transitively eliminate blocks. *)
  label :: dead_blocks

(* Must be called after predecessors are registered and split labels are
   registered. *)
let rec eliminate_dead_blocks cfg =
  (* if not t.preserve_orig_labels then
   *   failwith "Won't eliminate dead blocks when preserve_orig_labels is set."; *)
  let found =
    Hashtbl.fold
      (fun label block found ->
        if
          LabelSet.is_empty block.predecessors
          && (not (is_trap_handler t label))
          && t.cfg.entry_label <> label
        then label :: found
        else found)
      cfg.blocks []
  in
  let num_found = List.length found in
  if num_found > 0 then (
    let dead_blocks = List.fold_left (eliminate_dead_block cfg) [] found in
    let dead_blocks = List.sort_uniq Numbers.Int.compare dead_blocks in
    let num_eliminated = List.length dead_blocks in
    assert (num_eliminated >= num_found);
    if verbose then (
      Printf.printf
        "Found %d dead blocks in function %s, eliminated %d (transitively).\n"
        num_found cfg.fun_name num_eliminated;
      Printf.printf "Eliminated blocks are:";
      List.iter (fun lbl -> Printf.printf "\n%d" lbl) dead_blocks;
      Printf.printf "\n" );
    eliminate_dead_blocks cfg )

let dead_blocks cfg_builder =
  List.iter (eliminate_dead_blocks cfg)


type fallthrough_block = {
  label : label;
  target_label : label;
}

(* Disconnects fallthrough block by re-routing it predecessors to point
   directly to the successor block. *)
let disconnect_fallthrough_block t { label; target_label } =
  let block = Hashtbl.find t.cfg.blocks label in
  (* Update the successor block's predecessors set: first remove the current
     block and then add its predecessors. *)
  let target_block = Hashtbl.find t.cfg.blocks target_label in
  target_block.predecessors <-
    LabelSet.remove label target_block.predecessors;
  let update_pred pred_label =
    (* Update the predecessor block's terminators *)
    let replace_label l =
      if l = label then (
        target_block.predecessors <-
          LabelSet.add pred_label target_block.predecessors;
        target_label )
      else l
    in
    let replace_successor (cond, l) = (cond, replace_label l) in
    let pred_block = Hashtbl.find t.cfg.blocks pred_label in
    let t = pred_block.terminator in
    ( match t.desc with
    | Branch successors ->
        let new_successors = List.map replace_successor successors in
        pred_block.terminator <- { t with desc = Branch new_successors }
    | Switch labels ->
        let new_labels = Array.map replace_label labels in
        pred_block.terminator <- { t with desc = Switch new_labels }
    | _ -> () );
    simplify_terminator pred_block
  in
  LabelSet.iter update_pred block.predecessors;
  block.terminator <- { block.terminator with desc = Branch [] };
  block.predecessors <- LabelSet.empty

(* Find and disconnect fallthrough blocks until fixpoint. Does not eliminate
   dead blocks that result from it. Dead block elimination should run after
   it to delete these blocks.*)
let rec disconnect_fallthrough_blocks cfg =
  let found =
    Hashtbl.fold
      (fun label block found ->
        let successors_labels = successor_labels block in
        if
          cfg.entry_label <> label
          (* not entry block *)
          && List.length successors_labels = 1
          (* single successor *)
          && (not (is_trap_handler t label))
          && (* not trap label *)
             List.length block.body = 0
          (* empty body *)
        then (
          let target_label = List.hd successors_labels in
          if verbose then
            Printf.printf "block at %d has single successor %d\n" label
              target_label;
          { label; target_label } :: found )
        else found)
      cfg.blocks []
  in
  let len = List.length found in
  if len > 0 then (
    List.iter (disconnect_fallthrough_block t) found;
    if verbose then
      Printf.printf "Disconnected fallthrough blocks: %d\n" len;
    disconnect_fallthrough_blocks t )

let fallthrough_blocks t =
  (* Find and disconnect fallthrough blocks (i.e., blocks with empty body
     and a single successor) by rerouting their predecessors to point
     directly to their successors. Then eliminate dead blocks, which can
     create new fallthrough blocks by eliminated successors. Repeat until
     fixpoint. Termination is guaranteed because every step eliminates an
     edge or a node. The order matters for performance but not for the final
     result. *)
  cfg
  let rec loop () =
    let len = Hashtbl.length cfg.blocks in
    disconnect_fallthrough_blocks cfg;
    dead_blocks cfg;
    let new_len = Hashtbl.length cfg.blocks in
    if new_len < len && new_len > 0 then (
      if verbose then
        Printf.printf
          "Eliminated %d fallthrough blocks in %s: len=%d new_len=%d\n"
          (len - new_len) cfg.fun_name len new_len;
      loop () )
  in
  if verbose then print stdout t;
  Hashtbl.iter (fun _ b -> simplify_terminator b) t.cfg.blocks;
  loop ()
