[@@@ocaml.warning "+a-30-40-41-42"]

(* XCR mshinwell: I'm unsure why this needs to be [private] when there are
   accessor functions below. *)
type t =
  { cfg : Cfg.t;
    (* XCR xclerc: my understanding is that `layout` is the list of
     * the label, in the order in which they appearted in the source
     * linear representation. I would add a comment with this piece
     * of information. *)
    mutable layout : Label.t list;
    (** the order in which blocks should be emitted *)
    mutable new_labels : Label.Set.t;
    (** for validation, keep track of labels created by linear_to_cfg *)
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

(* XCR xclerc: the name is a bit misleading, as this function is actually
 * creating a file. *)
val save_as_dot :
  t ->
  ?show_instr:bool ->
  ?show_exn:bool ->
  ?annotate_block:(Label.t -> string) ->
  ?annotate_succ:(Label.t -> Label.t -> string) ->
  string ->
  unit

val print : t -> out_channel -> string -> unit
