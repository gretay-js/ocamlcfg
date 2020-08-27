
(* Identifier for an instruction in the cfg. *)
type t =
  | Term of Label.t
  | Inst of Label.t * int

val at_terminator : Label.t -> t
val at_instruction : Label.t -> int -> t

val compare : t -> t -> int
val equal : t -> t -> bool

val parent : t -> Label.t

module Map : Map.S with type key := t
module Set : Set.S with type elt := t

(** Returns the instruction located at the ID. *)
val get_inst
  :  Cfg.t
  -> t
  -> [ `Basic of Cfg.basic Cfg.instruction | `Term of Cfg.terminator Cfg.instruction]

(** Returns a basic instruction located at the ID, throws Not_found if missing. *)
val get_basic : Cfg.t -> t -> Cfg.basic Cfg.instruction

(** Returns a terminator instruction located at the ID, throws Not_found if missing. *)
val get_terminator : Cfg.t -> t -> Cfg.terminator Cfg.instruction

(* Returns all the predecessors of an instruction. *)
val get_predecessors_of_inst : Cfg.t -> t -> t list

(* Returns all the successors of an instruction. *)
val get_successors_of_inst : Cfg.t -> t -> t list

(* Printer method *)
val print : Format.formatter -> t -> unit
