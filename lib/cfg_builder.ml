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
[@@@ocaml.warning "+a-4-30-40-41-42-44-45"]

open Cfg

module Layout = struct
  type t = label list
end

let verbose = ref true

let dot_format = ref true

let dot_show_instr = ref true

type t = {
  (* The graph itself *)
  cfg : Cfg.t;
  (* Original layout: linear order of blocks. *)
  mutable layout : Layout.t;
  (* Map labels to trap depths. Required for linearize. *)
  trap_depths : (label, int) Hashtbl.t;
  (* Maps trap handler block label [L] to the label of the block where the
     "Lpushtrap L" reference it. Used for dead block elimination. This
     mapping is one to one, but the reverse is not, because a block might
     contain multiple Lpushtrap, which is not a terminator. *)
  trap_labels : (label, label) Hashtbl.t;
  (* Map id of instruction to label of the block that contains the
     instruction. Used for mapping perf data back to linear IR. *)
  mutable id_to_label : label Numbers.Int.Map.t;
  (* Set for validation, unset for optimization. *)
  mutable preserve_orig_labels : bool;
  (* Labels added by cfg construction, except entry. Used for testing of the
     mapping back to Linear IR. *)
  mutable new_labels : LabelSet.t;
}

let id_to_label t id =
  match Numbers.Int.Map.find_opt id t.id_to_label with
  | None ->
      Printf.fprintf stderr "id_to_label map: \n";
      Numbers.Int.Map.iter
        (fun id lbl -> Printf.printf "(%d,%d) " id lbl)
        t.id_to_label;
      Printf.fprintf stderr "\n";
      failwith (Printf.sprintf "Cannot find label for id %d in map\n" id)
  | Some lbl ->
      if !verbose then
        Printf.printf "Found label %d for id %d in map\n" lbl id;
      Some lbl

let is_trap_handler t label = Hashtbl.mem t.trap_labels label

(* CR-soon gyorsh: implement CFG traversal *)
(* CR-soon gyorsh: abstraction of cfg updates that transparently and
   efficiently keeps predecessors and successors in sync. For example,
   change successors relation should automatically updates the predecessors
   relation without recomputing them from scratch. *)

(* CR-soon gyorsh: Optimize terminators. Implement switch as jump table or
   as a sequence of branches depending on perf counters and layout. It
   should be implemented as a separate transformation on the cfg, that needs
   to be informated by the layout, but it should not be done while emitting
   linear. This way we can keep to_linear as simple as possible to ensure
   basic invariants are preserved, while other optimizations can be turned
   on and off.*)

type labelled_insn = {
  label : label;
  insn : Linear.instruction;
}

let labelled_insn_end = { label = -1; insn = Linear.end_instr }
