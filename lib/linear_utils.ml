[@@@ocaml.warning "+a-30-40-41-42"]

type labelled_insn =
  { label : Label.t;
    insn : Linear.instruction
  }

let labelled_insn_end = { label = -1; insn = Linear.end_instr }

(* XCR-someday xclerc: it might make sense to have this utility function
   upstreamed (e.g. next to `Linear.has_fallthrough`), as there is a feeling
   it might get out of sync.

   gyorsh: it would be hard to justify until the code that uses it
   is also upstreamed. In the meantime, following mshinwell's comment,
   making this match exhaustive will help to keep it in sync.
*)
let rec defines_label (i : Linear.instruction) =
  match i.desc with
  | Lend | Llabel _ ->
    (* CR xclerc for xclerc: does `Lprologue` have a label?

       gyorsh:
       The name is misleading, the intention is (has_label i) = true
       if and only if [i] produces a label definition.
       I've renamed this function to make the intent clearer.
       Lprologue does not produce it.
    *)
    true
  | Ladjust_trap_depth _ -> defines_label i.next
  | Lprologue
  | Lop _
  | Lreloadretaddr
  | Lreturn
  | Lbranch _
  | Lcondbranch _
  | Lcondbranch3 _
  | Lswitch _
  | Lentertrap
  | Lpushtrap _
  | Lpoptrap
  | Lraise _
    -> false  (* XCR mshinwell: Make this match exhaustive *)
