(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** External interface to the ocamlcfg library. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

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
  end

  (** The implementation of type [t] is a mutable structure. *)
  type t

  val iter_blocks : t -> f:(Label.t -> Basic_block.t -> unit) -> unit

  val get_block : t -> Label.t -> Basic_block.t option

  val successor_labels : t -> Basic_block.t -> Label.t list

  val fun_name : t -> string

  val entry_label : t -> Label.t

  val fun_tailrec_entry_point_label : t -> Label.t
end

module Cfg_with_layout : sig
  type t

  val cfg : t -> Cfg.t

  val layout : t -> Label.t list

  val set_layout : t -> Label.t list -> unit

  val print_dot : t -> ?show_instr:bool -> ?show_exn:bool -> string -> unit

  val print : t -> out_channel -> string -> unit

  val preserve_orig_labels : t -> bool

  (** eliminate_* can call simplify_terminators *)
  val eliminate_dead_blocks : t -> unit

  (** eliminate fallthrough implies dead block elimination *)
  val eliminate_fallthrough_blocks : t -> unit

  val of_linear : Linear.fundecl -> preserve_orig_labels:bool -> t

  val to_linear : t -> Linear.instruction

  (* CR mshinwell: Interface to determine if a block is a trap handler? *)
end

val verbose : bool ref

module Passes : sig
  val add_extra_debug : Cfg.t -> file:string -> unit

  val simplify_terminators : Cfg.t -> unit
end
