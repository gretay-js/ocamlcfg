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
module C = Cfg
module Int = Numbers.Int

let block (block : C.basic_block) =
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
              | Some (joined_cond :: rest) -> (
                  (* Some disjunctions of floating point comparisons cannot
                     be representated as a single condition. *)
                  match
                    Simplify_comparisons.disjunction cond joined_cond
                  with
                  | Ok cond -> cond :: rest
                  | Cannot_simplify -> cond :: joined_cond :: rest )
              | Some [] ->
                  (* XCR mshinwell: Why is this case impossible?

                     gyorsh: we never add an empty list here to
                     labels_to_conds.*)
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
      if len < 1 then
        Misc.fatal_error "Malform terminator: switch with empty arms";
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

let run t = C.iter_blocks t ~f:(fun _ b -> block b)
