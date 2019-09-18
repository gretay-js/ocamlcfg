(* Debug printing *)
val print :
  string ->
  out_channel ->
  Cfg.t ->
  Layout.t ->
  linearize_basic:
    (Cfg.basic Cfg.instruction ->
    (* next, but not labeling the argument for easier fold *)
    Linear.instruction ->
    Linear.instruction) ->
  linearize_terminator:
    (Cfg.terminator Cfg.instruction -> Linear.instruction) ->
  unit

val print_terminator :
  Format.formatter -> Cfg.terminator Cfg.instruction -> unit
