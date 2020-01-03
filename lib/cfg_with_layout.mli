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

type t = private
  { cfg : Cfg.t;
    mutable layout : Label.t list;
    mutable new_labels : Label.Set.t;
    (* Set for validation, unset for optimization. *)
    preserve_orig_labels : bool
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

val remove_from_new_labels : t -> Label.t -> unit

val is_trap_handler : t -> Label.t -> bool

val print_dot :
  t ->
  ?show_instr:bool ->
  ?show_exn:bool ->
  ?annotate_block:(Label.t -> string) ->
  ?annotate_succ:(Label.t -> Label.t -> string) ->
  string ->
  unit

val print : t -> out_channel -> string -> unit
