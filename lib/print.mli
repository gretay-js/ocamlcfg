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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Debug printing. *)

(* CR mshinwell: I would try to move these functions into the relevant
   files (e.g. debug_print -> Cfg_with_layout.print) *)

val print_layout : string -> out_channel -> Cfg.t -> Label.t list -> unit

val print_basic : Format.formatter -> Cfg.basic Cfg.instruction -> unit

val debug_print : string -> out_channel -> Cfg_with_layout.t -> unit

val dot_format : bool ref

val dot_show_instr : bool ref

val verbose : bool ref