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

type fallthrough_block = {
  label : label;
  target_label : label;
}

(* Disconnects fallthrough block by re-routing it predecessors to point
   directly to the successor block. *)
let disconnect_fallthrough_block t { label; target_label } =
  let block = Hashtbl.find t.cfg.blocks label in
  (* Update the successor block's predecessors set: first remove the current
     block and then add its predecessors. *)
  let target_block = Hashtbl.find t.cfg.blocks target_label in
  target_block.predecessors <-
    LabelSet.remove label target_block.predecessors;
  let update_pred pred_label =
    (* Update the predecessor block's terminators *)
    let replace_label l =
      if l = label then (
        target_block.predecessors <-
          LabelSet.add pred_label target_block.predecessors;
        target_label )
      else l
    in
    let replace_successor (cond, l) = (cond, replace_label l) in
    let pred_block = Hashtbl.find t.cfg.blocks pred_label in
    let t = pred_block.terminator in
    ( match t.desc with
    | Branch successors ->
        let new_successors = List.map replace_successor successors in
        pred_block.terminator <- { t with desc = Branch new_successors }
    | Switch labels ->
        let new_labels = Array.map replace_label labels in
        pred_block.terminator <- { t with desc = Switch new_labels }
    | _ -> () );
    simplify_terminator pred_block
  in
  LabelSet.iter update_pred block.predecessors;
  block.terminator <- { block.terminator with desc = Branch [] };
  block.predecessors <- LabelSet.empty

(* Find and disconnect fallthrough blocks until fixpoint. Does not eliminate
   dead blocks that result from it. Dead block elimination should run after
   it to delete these blocks.*)
let rec disconnect_fallthrough_blocks t =
  let found =
    Hashtbl.fold
      (fun label block found ->
        let successors_labels = successor_labels block in
        if
          t.cfg.entry_label <> label
          (* not entry block *)
          && List.length successors_labels = 1
          (* single successor *)
          && (not (is_trap_handler t label))
          && (* not trap label *)
             List.length block.body = 0
          (* empty body *)
        then (
          let target_label = List.hd successors_labels in
          if verbose then
            Printf.printf "block at %d has single successor %d\n" label
              target_label;
          { label; target_label } :: found )
        else found)
      t.cfg.blocks []
  in
  let len = List.length found in
  if len > 0 then (
    List.iter (disconnect_fallthrough_block t) found;
    if verbose then
      Printf.printf "Disconnected fallthrough blocks: %d\n" len;
    disconnect_fallthrough_blocks t )

let eliminate_fallthrough_blocks t =
  (* Find and disconnect fallthrough blocks (i.e., blocks with empty body
     and a single successor) by rerouting their predecessors to point
     directly to their successors. Then eliminate dead blocks, which can
     create new fallthrough blocks by eliminated successors. Repeat until
     fixpoint. Termination is guaranteed because every step eliminates an
     edge or a node. The order matters for performance but not for the final
     result. *)
  let rec loop () =
    let len = Hashtbl.length t.cfg.blocks in
    disconnect_fallthrough_blocks t;
    eliminate_dead_blocks t;
    let new_len = Hashtbl.length t.cfg.blocks in
    if new_len < len && new_len > 0 then (
      if verbose then
        Printf.printf
          "Eliminated %d fallthrough blocks in %s: len=%d new_len=%d\n"
          (len - new_len) t.cfg.fun_name len new_len;
      loop () )
  in
  if verbose then print stdout t;
  Hashtbl.iter (fun _ b -> simplify_terminator b) t.cfg.blocks;
  loop ()
