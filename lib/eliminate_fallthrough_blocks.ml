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

(* CR gyorsh: needs more testing.
   mshinwell: We should do the testing before the release---what more do
   we need to do? *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = Cfg
module CL = Cfg_with_layout

let is_fallthrough_block cfg_with_layout (block : C.basic_block) =
  let cfg = CL.cfg cfg_with_layout in
  let successor_labels = C.successor_labels cfg block in
  match successor_labels with
  | [] | _::_::_ -> None
  | [target_label] ->
    match block.body with
    | _::_ -> None
    | [] ->
      if cfg.entry_label <> block.start
        && (not (CL.is_trap_handler cfg_with_layout block.start))
      then Some target_label
      else None

(* CR mshinwell: The logic below looks similar in structure to
   [Eliminate_dead_blocks].  I think it would be worth trying to factor
   that out (into a functor) -- this would presumably form the starting
   point for a generic traversal / rewrite mechanism in the future. *)

(* Find and disconnect fallthrough blocks until fixpoint. Does not eliminate
   dead blocks that result from it. Dead block elimination should run after
   this to delete those blocks. *)
let rec disconnect_fallthrough_blocks cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  let found =
    Label.Tbl.fold (fun label block found ->
        match is_fallthrough_block cfg_with_layout block with
        | None -> found
        | Some target_label ->
          if !Print.verbose then begin
            Printf.printf "block at %d has single successor %d\n" label
              target_label
          end;
          Disconnect_block.disconnect cfg_with_layout label;
          label :: found)
      cfg.blocks []
  in
  let len = List.length found in
  if len > 0 then begin
    if !Print.verbose then begin
      Printf.printf "Disconnected fallthrough blocks: %d\n" len
    end;
    (* CR mshinwell: We have two fixpoint loops: here and the one below in
       [fallthrough_blocks].  Can we have just one? *)
    disconnect_fallthrough_blocks cfg_with_layout
  end

let fallthrough_blocks cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  (* CR mshinwell: I'm not sure the claim about elimination of dead blocks
     (at least as implemented in [Eliminate_dead_blocks]) is true. *)
  (* Find and disconnect fallthrough blocks (i.e., blocks with empty body
     and a single successor) by rerouting their predecessors to point
     directly to their successors. Then eliminate dead blocks, which can
     create new fallthrough blocks by eliminated successors. Repeat until
     fixpoint. Termination is guaranteed because every step eliminates an
     edge or a node. The order matters for performance but not for the final
     result. *)
  let rec loop () =
    let len = Label.Tbl.length cfg.blocks in
    disconnect_fallthrough_blocks cfg_with_layout;
    Eliminate_dead_blocks.dead_blocks cfg_with_layout;
    let new_len = Label.Tbl.length cfg.blocks in
    if new_len < len && new_len > 0 then begin
      if !Print.verbose then begin
        Printf.printf
          "Eliminated %d fallthrough blocks in %s: len=%d new_len=%d\n"
          (len - new_len) cfg.fun_name len new_len
      end;
      loop ();
    end
  in
  if !Print.verbose then begin
    Print.debug_print "before_elim_ft" stdout cfg_with_layout
  end;
  (* CR mshinwell: Why is this simplify_terminator call here?  We've already
     called this on at least some of the affected blocks from
     [Disconnect_block].  If this is needed, maybe it should be a separate
     pass (and we should move [simplify_terminator] out of
     [Disconnect_block] into its own file, most likely). *)
  (* Uncomment if needed
  Label.Tbl.iter (fun _ b -> simplify_terminator b) cfg.blocks;
  *)
  loop ()
