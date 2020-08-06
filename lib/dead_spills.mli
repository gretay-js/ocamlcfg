(** Remove dead spills, based on liveness information. *)

val run : Cfg.t -> unit
