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

(** Insertion of extra debugging information used to correlate between
    machine instructions, [Linear] and [Cfg] code. *)

(** Adds the id of each cfg instruction into debug info of each instruction,
    encoded as line number into the [file]. *)
val add : Cfg.t -> file:string -> unit
