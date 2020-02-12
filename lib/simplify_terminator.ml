[@@@ocaml.warning "+a-30-40-41-42"]
module C = Cfg

(* Convert simple [Switch] to branches. *)
let simplify_switch (block : C.basic_block) labels =
  (* XCR xclerc: not sure it matters, but it feels weird to have an
   * optimization that is asymmetrical. From what I understand:
   * - `Switch [a, b, c, c, ..., c]` would be optimized;
   * - `Switch [a, a, ..., a, b, c]` would not be optimized.

     gyorsh: ah, yes, we can use arbitrary [n] for the case of two targets.
     Lcondbranch3 is asymmetrical by definition. fixed. *)
  let len = Array.length labels in
  if len < 1 then
    Misc.fatal_error "Malformed terminator: switch with empty arms";
  (* Count continuois repeated occurrences of labels *)
  let labels_with_counts =
    Array.fold_right
      (fun l acc ->
         if (List.compare_length_with acc 3) > 0 then acc
         else match acc with
           | [] -> [(l,1)]
           | (hd,n)::tl -> if hd = l then (hd,n+1)::tl else (l,1)::acc
      )
      labels
      []
  in
  match labels_with_counts with
  | [(l,1)] ->
    (* All labels are the same and equal to l *)
    block.terminator <- { block.terminator with desc = Branch (Always l) }
  | [(l0,n);(ln,k)] ->
    assert (labels.(0) = l0);
    assert (labels.(n) = ln);
    assert (len = n+k);
    let successors = C.Int_test
                       {
                         is_signed = false;
                         imm = Some n;
                         lt = l0;
                         eq = ln;
                         gt = ln;
                       }
    in
    block.terminator <- { block.terminator with desc = Branch successors }
  | [(l0,1);(l1,1);(l2,n)] ->
    assert (labels.(0) = l0);
    assert (labels.(1) = l1);
    assert (labels.(2) = l2);
    assert (len = n+2);
    let successors = C.Int_test
                       {
                         is_signed = false;
                         imm = Some 1;
                         lt = l0;
                         eq = l1;
                         gt = l2
                       }
    in
    block.terminator <- { block.terminator with desc = Branch successors}
  | _ -> ()

(* CR-soon gyorsh: merge (Lbranch | Lcondbranch | Lcondbranch3)+ into a
   single terminator when the argments are the same. Enables reordering
   of branch instructions and save cmp instructions. The main problem
   is that it involves boolean combination of conditionals of type
   Mach.test that can arise from a sequence of branches. When all
   conditions in the combination are integer comparisons, we can
   simplify them into a single condition, but it doesn't work for
   Ieventest and Ioddtest (which come from the primitive "is integer").
   The advantage is that it will enable us to reorder branch
   instructions to avoid generating jmp to fallthrough location in the
   new order. Also, for linear to cfg and back will be harder to
   generate exactly the same layout. Also, how do we map execution
   counts about branches onto this terminator? *)
let block cfg (block : C.basic_block) =
  match block.terminator.desc with
  | Branch (Always _) -> ()
  | Branch (Is_even _ | Is_true _ | Int_test _ | Float_test _) ->
      let labels = C.successor_labels cfg ~normal:true ~exn:false block in
      if Label.Set.cardinal labels = 1 then (
        let l = Label.Set.min_elt labels in
        block.terminator <- { block.terminator with desc = Branch (Always l) }
      )
  | Switch labels -> simplify_switch block labels
  | Tailcall (Self _ | Func _) | Raise _ | Return  -> ()

let run cfg = C.iter_blocks cfg ~f:(fun _ b -> block cfg b)


(* XCR xclerc: this code is generic, but it feels like is has been exercized
 * only/mainly with branches with two successors. The semantics of
 * the list of conditions is not very clear to me if e.g. we simplify,
 * fail to simplify, and then simplify again.

   gyorsh: the new representation of terminators does not need to be
   simplified in this way. It keeps disjoint conditions explicitly
   and allows redundancy in the target labels, i.e.,
   more than one conditions to go to the same label.
   The simplification is done in cfg_to_linear, where
   conditions that go to the same label are emitted as one opcode
   (conditions jump with the appropriate condition),
   whever possible, and everything is representable.
*)
