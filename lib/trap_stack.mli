(* XCR mshinwell: Please add file header and an explanation of what this
   module is doing (ideally with comments for each function below). *)
(* XCR xclerc: the header is missing. *)
[@@@ocaml.warning "+a-30-40-41-42"]
(* Mutable representation of a stack of handlers.

   Intended for computing a stack of trap handlers reachable a
   at each program location in a function.
   The top of the stack represents the current trap handler,
   which will be called if the current instruction raises.
*)


module type D =
sig
  type t
  val equal : t -> t -> bool
  (** for unification *)

  val to_string : t -> string
  (** for debug printing *)
end

module type S =
sig
  type d
  (** The type of elements pushed on the stack *)

  type t
  (** The stack of elements *)

  exception Unresolved

  val empty : t
  (** Returns the representation of an empty stack. *)

  val unknown : unit -> t
  (** Returns the representation of an unknown stack. *)

  val pop : t -> t
  val push : t -> d -> t

  (** Returns list representation of stack [t], with the head of the list
      representing the top of the stack. Raises [Unresolved] if [t]
      contains any [Unknown]. Does not terminate if [t] contains a cycle. *)
  val to_list_exn : t -> d list

  (* XCR mshinwell: Document which is the "top" of the stack. *)
  (** Returns the top of the stack [t], or [None] if [t] is empty. Raises
      [Unresolved] if the top of [t] cannot be resolved,
      i.e., top of [t] is Unknown or contains an unknown label. *)
  val top_exn : t -> d option

  val unify : t -> t -> unit
  (** [unify s1 s2] fail if s1 and s2 do not agree on the known elements,
      and resolves unknowns whenever possible, destructively modifying
      [s1] and [s2]. *)

  (* Debug printing *)

  val print : t -> unit

  val print_pair : string -> t -> t -> unit
end

module Make (D : D) : S with type d = D.t
(** Functor building stacks of *)
