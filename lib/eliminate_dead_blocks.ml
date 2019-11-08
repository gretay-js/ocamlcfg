(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*                         based on the work of                           *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: Is this file based on Xavier's work? *)

(* CR gyorsh: needs more testing. mshinwell: We should do the testing before
   the release---what more do we need to do? *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = Cfg
module CL = Cfg_with_layout
module Int = Numbers.Int

(* CR-soon gyorsh: eliminate transitively blocks that become dead from this
   one. *)
let eliminate_dead_block cfg_with_layout dead_blocks label_of_dead_block =
  Disconnect_block.disconnect cfg_with_layout label_of_dead_block;
  (* CR-soon gyorsh: update this when we transitively eliminate blocks. *)
  label_of_dead_block :: dead_blocks

let block_is_dead cfg_with_layout (block : C.basic_block) =
  let cfg = CL.cfg cfg_with_layout in
  Label.Set.is_empty block.predecessors
  && (not (CL.is_trap_handler cfg_with_layout block.start))
  && C.entry_label cfg <> block.start

let rec eliminate_dead_blocks cfg_with_layout =
  if CL.preserve_orig_labels cfg_with_layout then
    Misc.fatal_error
      "Won't eliminate dead blocks when [preserve_orig_labels] is set";
  let found_dead =
    Label.Tbl.fold
      (fun label block found ->
        if block_is_dead cfg_with_layout block then label :: found else found)
      (CL.cfg cfg_with_layout).blocks []
  in
  let num_found_dead = List.length found_dead in
  if num_found_dead > 0 then (
    let dead_blocks =
      List.fold_left (eliminate_dead_block cfg_with_layout) [] found_dead
      |> Int.Set.of_list
    in
    let num_eliminated = Int.Set.cardinal dead_blocks in
    assert (num_eliminated >= num_found_dead);
    if !C.verbose then (
      (* CR mshinwell: It's unclear why this says "transitively". *)
      Printf.printf
        "Found %d dead blocks in function %s, eliminated %d (transitively).\n"
        num_found_dead (CL.cfg cfg_with_layout).fun_name num_eliminated;
      Printf.printf "Eliminated blocks are:";
      Int.Set.iter (fun lbl -> Printf.printf "\n%d" lbl) dead_blocks;
      Printf.printf "\n" );
    (* CR mshinwell: Add comment saying why this always terminates *)
    eliminate_dead_blocks cfg_with_layout )

let run cfg_with_layout = eliminate_dead_blocks cfg_with_layout
