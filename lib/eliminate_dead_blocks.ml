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
(* CR-soon gyorsh: Eliminate dead cycles. *)
[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = Cfg
module CL = Cfg_with_layout

let block_is_dead cfg_with_layout (block : C.basic_block) =
  let cfg = CL.cfg cfg_with_layout in
  Label.Set.is_empty block.predecessors
  (* CR-soon gyorsh: Predecessors already contains all live handlers. Remove
     is_trap_handler check when CFG is updated to use trap stacks instead of
     pushtrap/poptrap instructions in CFG. *)
  && (not block.is_trap_handler)
  (* CR xclerc: I would rather used `Label.equal` (defensive against a change
     of `Label.t`). *)
  && cfg.entry_label <> block.start

(* CR-someday xclerc: not to say the implementation should change any time
   soon, but since it was mentioned the other day: with support for generic
   data flow analysis, a trivial analysis would identify all live/dead
   blocks in one go and the function below would no longer be recursive. *)
let rec eliminate_dead_blocks cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  if CL.preserve_orig_labels cfg_with_layout then
    Misc.fatal_error
      "Won't eliminate dead blocks when [preserve_orig_labels] is set";
  let found_dead =
    Label.Tbl.fold
      (fun label block found ->
        if block_is_dead cfg_with_layout block then label :: found else found)
      cfg.blocks []
  in
  let num_found_dead = List.length found_dead in
  if num_found_dead > 0 then (
    List.iter (Disconnect_block.disconnect cfg_with_layout) found_dead;
    if !C.verbose then (
      Printf.printf "Found and eliminated %d dead blocks in function %s.\n"
        num_found_dead cfg.fun_name;
      Printf.printf "Eliminated blocks are:";
      List.iter (Printf.printf "\n%d") found_dead;
      Printf.printf "\n" );
    (* Termination: the number of remaining blocks is strictly smaller in
       each recursive call. *)
    eliminate_dead_blocks cfg_with_layout )

let run cfg_with_layout = eliminate_dead_blocks cfg_with_layout
