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
(* Control Flow Graph of a function with its layout. *)

type t = {
  (* The graph itself *)
  cfg : Cfg.t;
  (* Original layout: linear order of blocks. *)
  mutable layout : Layout.t;
  (* Map labels to trap depths. Required for linearize. *)
  trap_depths : (Cfg.label, int) Hashtbl.t;
  (* Maps trap handler block label [L] to the label of the block where the
     "Lpushtrap L" reference it. Used for dead block elimination. This
     mapping is one to one, but the reverse is not, because a block might
     contain multiple Lpushtrap, which is not a terminator. *)
  trap_labels : (Cfg.label, Cfg.label) Hashtbl.t;
  (* Map id of instruction to label of the block that contains the
     instruction. Used for mapping perf data back to linear IR. *)
  mutable id_to_label : Cfg.label Numbers.Int.Map.t;
  (* Set for validation, unset for optimization. *)
  mutable preserve_orig_labels : bool;
  (* Labels added by cfg construction, except entry. Used for testing of the
     mapping back to Linear IR. *)
  mutable new_labels : Cfg.LabelSet.t;
}

val is_trap_handler : t -> Cfg.label -> bool

val id_to_label : t -> int -> Cfg.label option

type labelled_insn = {
  label : Cfg.label;
  insn : Linear.instruction;
}

val labelled_insn_end : labelled_insn

val verbose : bool ref
