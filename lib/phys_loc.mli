
(** Identifier for a physical location. *)
type t =
  | Reg of int
  | Stack of Stack_slot.t

module Set : Set.S with type elt := t
module Map : Map.S with type key := t

val of_reg : Reg.t -> t option

val print : Format.formatter -> t -> unit
