type t = stack ref

and stack =
  | Emp
  | Unknown
  | Push of Label.t * t
  | Pop of t
  | Link of t

exception Unresolved

(** Returns list representation of stack [t], with the head of the list
    representing the top of the stack. Raises [Unresolved] if the canonical
    representation of [t] contains any [Pop] or [Link] or [Unknown]. Does not
    terminate if [t] contains a cycle. *)
val to_list_exn : t -> Label.t list

(** Returns the top of the stack [t], or [None] if [t] is empty. Raises
    [Unresolved] if [t] cannot be resolved. *)
val top_exn : t -> Label.t option

val unify : t -> t -> t
