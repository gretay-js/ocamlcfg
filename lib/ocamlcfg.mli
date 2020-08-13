(** External interface to the ocamlcfg library. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module Label : sig
  include module type of struct
    include Label
  end
end

module Statistics : sig
  include module type of struct
    include Statistics
  end
end


module Cfg : sig
  include module type of struct
    include Cfg_intf.S
  end

  module Basic_block : sig
    (** The implementation of type [t] is a mutable structure. *)
    type t

    val start : t -> Label.t

    val body : t -> basic instruction list

    val terminator : t -> terminator instruction

    val is_exit : t -> bool
  end

  (** The implementation of type [t] is a mutable structure. *)
  type t

  val iter_blocks : t -> f:(Label.t -> Basic_block.t -> unit) -> unit

  val blocks : t -> Basic_block.t list

  val get_block : t -> Label.t -> Basic_block.t option

  val get_block_exn : t -> Label.t -> Basic_block.t

  (** [successor_labels] only returns non-exceptional edges. We need to pass
      [t] because the successor label of terminator (Tailcall Self) is
      recorded in [t], and not in the basic_block. *)
  val successor_labels : t -> Basic_block.t -> Label.t list

  val all_successor_labels : t -> Basic_block.t -> Label.t list

  val predecessor_labels : Basic_block.t -> Label.t list

  val fun_name : t -> string

  val entry_label : t -> Label.t

  val fun_tailrec_entry_point_label : t -> Label.t

  val destroyed_at_basic : basic -> int array

  val destroyed_at_terminator : terminator -> int array

  val print_terminator
    :  Format.formatter
    -> ?sep:string
    -> terminator instruction
    -> unit

  val print_basic : Format.formatter -> basic instruction -> unit

  val print : t -> Format.formatter -> string -> unit
end

module Cfg_with_layout : sig
  type t

  val cfg : t -> Cfg.t

  val layout : t -> Label.t list

  val set_layout : t -> Label.t list -> unit

  val save_as_dot :
    t ->
    ?show_instr:bool ->
    ?show_exn:bool ->
    ?annotate_block:(Label.t -> string) ->
    ?annotate_succ:(Label.t -> Label.t -> string) ->
    string ->
    unit

  val print : t -> Format.formatter -> string -> unit

  val preserve_orig_labels : t -> bool

  (** eliminate_* can call simplify_terminators *)
  val eliminate_dead_blocks : t -> unit

  (** eliminate fallthrough implies dead block elimination *)
  val eliminate_fallthrough_blocks : t -> unit

  val of_linear : Linear.fundecl -> preserve_orig_labels:bool -> t

  val to_linear : t -> Linear.instruction
end

module Passes : sig
  val add_extra_debug : Cfg.t -> file:string -> unit

  val simplify_terminators : Cfg.t -> unit

  val slot_to_register : Cfg.t -> unit

  val verify_liveness : Cfg.t -> unit
end

module Util : sig
  val verbose : bool ref

  val print_linear : Format.formatter -> Cfg_with_layout.t -> unit

  val print_assembly : Cfg.Basic_block.t list -> unit
end

module Inst_id : sig
  type t

  val at_terminator : Label.t -> t

  val at_instruction : Label.t -> int -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val parent : t -> Label.t

  module Map : Map.S with type key := t
  module Set : Set.S with type elt := t

  val get_inst
    :  Cfg.t
    -> t
    -> [ `Basic of Cfg.basic Cfg.instruction | `Term of Cfg.terminator Cfg.instruction]

  val get_predecessors_of_inst : Cfg.t -> t -> t list
end

module Analysis : sig
  include Data_flow_analysis_intf.S

  module type Cfg_semigroup_action_problem = sig
    module A : Semigroup_action

    type t

    val cfg : t -> Cfg.t

    val entry : t -> Label.t -> A.S.t

    val action : t -> Inst_id.t -> A.G.t
  end

  module Make_solver (P: Problem) : Solver
    with type t := P.t
     and module S := P.S
     and module Node := P.Node

  module Make_kill_gen_solver (P: Semigroup_action_problem) : Solver
    with type t := P.t
     and module S := P.A.S
     and module Node := P.Node

  module Make_forward_cfg_solver (P: Cfg_semigroup_action_problem) : Solver
    with type t := P.t
     and module S := P.A.S
     and module Node := Inst_id

  module Make_backward_cfg_solver (P: Cfg_semigroup_action_problem): Solver
    with type t := P.t
     and module S := P.A.S
     and module Node := Inst_id
end
