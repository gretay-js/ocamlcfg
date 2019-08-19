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
(* Control Flow Graph of a function. *)
type t

type label = Linear.label

module Layout : sig
  type t = label list
end

val from_linear : Linear.fundecl -> preserve_orig_labels:bool -> t

val to_linear : t -> extra_debug:bool -> Linear.instruction

val get_block : t -> label -> Cfg.block option

val get_layout : t -> Layout.t

val set_layout : t -> Layout.t -> t

val is_trap_handler : t -> label -> bool

val get_name : t -> string

val preserve_orig_labels : t -> bool

val id_to_label : t -> int -> label option

val entry_label : t -> label

val print : out_channel -> t -> unit

(* Mutates t inplace *)
val eliminate_dead_blocks : t -> unit

(* Mutates t inplace and also eliminate dead blocks *)
val eliminate_fallthrough_blocks : t -> unit

type labelled_insn = {
  label : label;
  insn : Linear.instruction;
}

val labelled_insn_end : labelled_insn

val linearize_terminator :
  ?extra_debug:string ->
  Cfg.terminator Cfg.instruction ->
  next:labelled_insn ->
  Linear.instruction

val basic_to_linear :
  ?extra_debug:string ->
  Cfg.basic Cfg.instruction ->
  Linear.instruction ->
  Linear.instruction
