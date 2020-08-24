type t

type meet_point =
  | Before of Inst_id.t
  | After of Inst_id.t
  | Anywhere of Inst_id.t
  | Edge of Label.t * Label.t

val of_cfg
  :  Cfg.t
  -> spill:Inst_id.t
  -> spills:Spill_reload_range.t
  -> reload:Inst_id.t
  -> reloads:Spill_reload_range.t
  -> t
(** Builds a graph with meeting points/edges from reaload/spill reach information. *)

val meet_points : t -> meet_point list
(** Returns a list of edges/nodes where spills and reloads meet. *)

val split_points : t -> Label.Set.t
(** Finds basic blocks which are not on meeting paths. *)

val print : Format.formatter -> t -> unit
(** Prints the graph. *)
