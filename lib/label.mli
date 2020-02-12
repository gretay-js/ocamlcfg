[@@@ocaml.warning "+a-30-40-41-42"]

type t = int

include Identifiable.S with type t := t
val equal : t -> t -> bool
val to_string : t -> string

(*
XCR xclerc: Is there a (remote) chance `Label.t` will ever be more complex
than a simple `int`? If not, a number of CRs are to be ignored: all the
ones related to the use of `Label.equal` instead of `=` or `<>`.

gyorsh: I converted all '=' that I could find to equal.
   A previous version distinguished original labels from new
   labels introduced in translation. It was needed for validation when
   integration with the compiler was via a driver rather than files.
   We may need it again if the cfg is included in the compiler.
   Also, there are special labels such as
   cfg entry and interprocedural exception handler.
*)

(*
XCR xclerc: a number of CRs are about checking whether a passed label is
indeed present in the associated CFG. To be honest, I am not certain
these sanity checks are worthwhile.

gyorsh: I addressed the ones that are not implied
   from local invariants of the function they appear in. thank you!
*)
