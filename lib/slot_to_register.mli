(** Replace stack slots with registers if the value from the slot is available in them. *)

val run : Cfg.t -> unit
