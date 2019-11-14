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

module C = Cfg
module CL = Cfg_with_layout

let is_fallthrough_block cfg_with_layout (block : C.basic_block) =
  let cfg = CL.cfg cfg_with_layout in
  if cfg.entry_label = block.start || block.is_trap_handler then None
  else
    match C.successor_labels ~normal:true ~exn:false cfg block with
    | [] | _ :: _ :: _ -> None
    | [target_label] -> (
        match block.body with
        | [] -> Some target_label
        | _ -> None )

(* CR mshinwell: The logic below looks similar in structure to
   [Eliminate_dead_blocks]. I think it would be worth trying to factor that
   out (into a functor) -- this would presumably form the starting point for
   a generic traversal / rewrite mechanism in the future. *)

(* Find and disconnect fallthrough blocks until fixpoint. Does not eliminate
   dead blocks that result from it. Dead block elimination should run after
   this to delete those blocks. *)
let rec disconnect_fallthrough_blocks cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  let found =
    Label.Tbl.fold
      (fun label block found ->
        match is_fallthrough_block cfg_with_layout block with
        | None -> found
        | Some target_label ->
            if !C.verbose then
              Printf.printf "block at %d has single successor %d\n" label
                target_label;
            Disconnect_block.disconnect cfg_with_layout label;
            label :: found)
      cfg.blocks []
  in
  let len = List.length found in
  if len > 0 then (
    if !C.verbose then
      Printf.printf "Disconnected fallthrough blocks: %d\n" len;
    (* CR mshinwell: We have two fixpoint loops: here and the one below in
       [fallthrough_blocks]. Can we have just one? *)
    disconnect_fallthrough_blocks cfg_with_layout )

let run cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  (* CR mshinwell: I'm not sure the claim about elimination of dead blocks
     (at least as implemented in [Eliminate_dead_blocks]) is true. *)
  (* Find and disconnect fallthrough blocks (i.e., blocks with empty body and
     a single successor) by rerouting their predecessors to point directly to
     their successors. Then eliminate dead blocks, which can create new
     fallthrough blocks by eliminated successors. Repeat until fixpoint.
     Termination is guaranteed because every step eliminates an edge or a
     node. The order matters for performance but not for the final result. *)
  let rec loop () =
    let len = Label.Tbl.length cfg.blocks in
    disconnect_fallthrough_blocks cfg_with_layout;
    Eliminate_dead_blocks.run cfg_with_layout;
    let new_len = Label.Tbl.length cfg.blocks in
    if new_len < len && new_len > 0 then (
      if !C.verbose then
        Printf.printf
          "Eliminated %d fallthrough blocks in %s: len=%d new_len=%d\n"
          (len - new_len) cfg.fun_name len new_len;
      loop () )
  in
  if !C.verbose then CL.print_dot cfg_with_layout "before_elim_ft";
  (* CR mshinwell: Why is this simplify_terminator call here? We've already
     called this on at least some of the affected blocks from
     [Disconnect_block]. If this is needed, maybe it should be a separate
     pass (and we should move [simplify_terminator] out of [Disconnect_block]
     into its own file, most likely). *)
  (* Uncomment if needed Label.Tbl.iter (fun _ b -> simplify_terminator b)
     cfg.blocks; *)
  loop ()
