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
val run : Cfg_builder.t -> extra_debug:bool -> Linear.instruction

(* The following functions are used by the Print. They should not be called
   from outside the library *)
val basic_to_linear :
  ?extra_debug:string ->
  Cfg.basic Cfg.instruction ->
  (* next, but not labeling the argument for easier fold *)
  Linear.instruction ->
  Linear.instruction

val linearize_terminator :
  Cfg.t ->
  ?extra_debug:string ->
  Cfg.terminator Cfg.instruction ->
  Cfg_builder.labelled_insn ->
  Linear.instruction
