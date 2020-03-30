[@@@ocaml.warning "+a-30-40-41-42"]

(* CR-soon mshinwell: I'm unsure why this needs to be [private] when there
   are accessor functions below. xclerc: I guess you mean the type could just
   be abstract? mshinwell: Yes *)
type t =
  { cfg : Cfg.t;
    mutable layout : Label.t list;
        (** The order in which blocks should be emitted *)
    mutable new_labels : Label.Set.t;
        (** For validation, keeps track of labels created by [Linear_to_cfg] *)
    preserve_orig_labels : bool
        (** Set for validation, unset for optimization. *)
  }

val create :
  Cfg.t ->
  layout:Label.t list ->
  preserve_orig_labels:bool ->
  new_labels:Label.Set.t ->
  t

val cfg : t -> Cfg.t

val layout : t -> Label.t list

val preserve_orig_labels : t -> bool

val new_labels : t -> Label.Set.t

val set_layout : t -> Label.t list -> unit

(** Remove from cfg, layout, and other data-structures that track labels. *)
val remove_block : t -> Label.t -> unit

val is_trap_handler : t -> Label.t -> bool

val save_as_dot :
  t ->
  ?show_instr:bool ->
  ?show_exn:bool ->
  ?annotate_block:(Label.t -> string) ->
  ?annotate_succ:(Label.t -> Label.t -> string) ->
  string ->
  unit

val print : t -> out_channel -> string -> unit
