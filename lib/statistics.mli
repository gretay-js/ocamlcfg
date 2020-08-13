
(** Enters a scope, writing all counters to [file]. *)
val record : file:string -> (unit -> 'a) -> 'a

(** Increments a counter *)
val inc : group:string -> key:string -> unit
