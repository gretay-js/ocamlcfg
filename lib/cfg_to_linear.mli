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

(** Conversion from [Cfg] to [Linear] code. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val run : Cfg_with_layout.t -> extra_debug:bool -> Linear.instruction

(** Private functions for the [Print] module. *)

val basic_to_linear
   : ?extra_debug:string
  -> Cfg.basic Cfg.instruction
  -> next:Linear.instruction
  -> Linear.instruction

val linearize_terminator
   : Cfg.t
  -> ?extra_debug:string
  -> Cfg.terminator Cfg.instruction
  -> next:Linear_utils.labelled_insn
  -> Linear.instruction