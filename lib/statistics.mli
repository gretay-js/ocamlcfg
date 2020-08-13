
(** Enters a scope, writing all counters to [file]. *)
val record : file:string -> (unit -> 'a) -> 'a

(** Sets a counter to a value. *)
val set : group:string -> key:string -> int -> unit

(** Increments a counter *)
val inc : group:string -> key:string -> unit
