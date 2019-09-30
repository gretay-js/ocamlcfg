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
  include Cfg_intf.S

  (* CR mshinwell for gyorsh (per request): check that the other types
     (and modules) are not accessible. *)

  module Basic_block : sig
    (** The implementation of type [t] is a mutable structure. *)
    type t

    val start : t -> Label.t
    val body : t -> basic instruction list
    val terminator : t -> terminator instruction
    val predecessors : t -> Label.Set.t
  end

  (** The implementation of type [t] is a mutable structure. *)
  type t

  (* CR mshinwell: Put [print] here *)

  (* CR mshinwell: we need more methods here, e.g. find *)
  val iter_blocks : t -> f:(Label.t -> Basic_block.t -> unit) -> unit

  val fun_name : t -> string

  val entry_label : t -> Label.t

  val fun_tailrec_entry_point_label : t -> Label.t
end

module Cfg_with_layout : sig
  type t

  val cfg : t -> Cfg.t

  val layout : t -> Label.t list
  
  val set_layout : t -> layout:Label.t list -> unit

  (* CR mshinwell: Interface to determine if a block is a trap handler? *)
end

module Eliminate_fallthrough_blocks : sig
  val fallthrough_blocks : Cfg_with_layout.t -> unit
end

module Eliminate_dead_blocks : sig
  val dead_blocks : Cfg_with_layout.t -> unit
end