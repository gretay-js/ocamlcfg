type t

module Meet_point : sig
  type loc = Before | After | Anywhere

  type t =
    | Node of Inst_id.t * loc
    | Edge of Label.t * Label.t

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

val of_cfg
  :  Cfg.t
  -> spill:Inst_id.t
  -> spills:Spill_reload_range.t
  -> reload:Inst_id.t
  -> reloads:Spill_reload_range.t
  -> t
(** Builds a graph with meeting points/edges from reaload/spill reach information. *)

val meet_points : t -> Meet_point.t list
(** Returns a list of edges/nodes where spills and reloads meet. *)

val split_points : t -> Label.Set.t
(** Finds basic blocks which are not on meeting paths. *)

val sccs : t -> Label.Set.t list
(** Enumerate all strongly connected components. *)

val spill_reaches : t -> Label.t -> bool
(** Returns true if the spill reaches the block. *)

val reload_reaches : t -> Label.t -> bool
(** Returns true if the reload reaches the block. *)

val has_node : t -> Label.t -> bool
(** Check whether the node is on a path between the spill and the reload. *)

val print : Format.formatter -> t -> unit
(** Prints the graph. *)
