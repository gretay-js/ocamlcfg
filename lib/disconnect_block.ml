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

let update_predecessor's_terminators (cfg : C.t) ~pred_label
    ~being_disconnected ~target_label =
  let replace_label l = if l = being_disconnected then target_label else l in
  let pred_block = Label.Tbl.find cfg.blocks pred_label in
  Cfg.replace_successor_labels cfg ~normal:true ~exn:true pred_block
    ~f:replace_label;
  Simplify_terminator.run pred_block

let disconnect cfg_with_layout label =
  let cfg = CL.cfg cfg_with_layout in
  let block = C.get_and_remove_block_exn cfg label in
  let has_predecessors = not (Label.Set.is_empty block.predecessors) in
  let has_more_than_one_successor =
    List.length (Cfg.successor_labels ~normal:true ~exn:true cfg block) > 1
  in

  if has_more_than_one_successor && has_predecessors then
    Misc.fatal_errorf
      "Cannot disconnect block %a: it has more than one successor and at \
       least one predecessor"
      Label.print label;
  (* Update successor blocks. *)
  List.iter
    (fun succ ->
      let succ_block = C.get_block_exn cfg succ in
      assert (Label.Set.mem label succ_block.predecessors);
      succ_block.predecessors <-
        Label.Set.union
          (Label.Set.remove label succ_block.predecessors)
          block.predecessors)
    (Cfg.successor_labels ~normal:true ~exn:true cfg block);
  (* Update predecessor blocks. *)
  ( match Cfg.successor_labels ~normal:true ~exn:true cfg block with
  | [target_label] ->
      Label.Set.iter
        (fun pred_label ->
          update_predecessor's_terminators cfg ~pred_label
            ~being_disconnected:label ~target_label)
        block.predecessors
  | [] | _ :: _ -> assert (Label.Set.is_empty block.predecessors) );
  (* Remove from layout and other data-structures that track labels. *)
  let layout = CL.layout cfg_with_layout in
  let new_layout = List.filter (fun l -> l <> label) layout in
  CL.set_layout cfg_with_layout new_layout;
  CL.remove_from_new_labels cfg_with_layout label;

  (* CR-soon gyorsh: how to remove this label from block.trap_stacks of other
     blocks? For now, trap stacks can be ignored after construction of cfg
     from linear, and if do not eliminate trap handler, but in the new cfg,
     trap stacks will be used instead of push/pop trap, and if trap handlers
     can be eliminated then trap stacks would need to be updated. *)

  (* CR mshinwell: The next two lines should move to e.g.
     [Cfg.invalidate_block] together with an associated test for that,
     perhaps. When is such a test used? If it is not used, can we skip this
     procedure?

     gyorsh: not sure what to do with this CR. It's a valid block but
     detached. Once it's removed from all the data structures, it will be
     unreachable by ocaml. Are you suggesting that it should be kept around,
     maybe for debugging? *)
  block.exns <- Label.Set.empty;
  block.terminator <- { block.terminator with desc = Branch [] };
  block.predecessors <- Label.Set.empty
