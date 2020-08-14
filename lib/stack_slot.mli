
(** Identifier for a stack slot. *)
type t = { loc: int; reg_class: int }

module Map : Map.S with type key := t
module Set : Set.S with type elt := t

val of_reg : Reg.t -> t option

val print : Format.formatter -> t -> unit
