(* Debug printing *)
val print : string -> out_channel -> Cfg.t -> Layout.t -> unit

val print_basic : Format.formatter -> Cfg.basic Cfg.instruction -> unit

val debug_print : string -> out_channel -> Cfg_builder.t -> unit
