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

let verbose = ref false

let dot_format = ref true

let dot_show_instr = ref true

let linearize_basic insn next =
  Cfg_to_linear.basic_to_linear ?extra_debug:None insn ~next

let linearize_terminator cfg_builder terminator =
  Cfg_to_linear.linearize_terminator cfg_builder ?extra_debug:None
    terminator ~next:Linear_utils.labelled_insn_end

let print_block t ppf label (b : C.basic_block) =
  Format.fprintf ppf "\n%d:\n" label;
  let i = List.fold_right linearize_basic b.body Linear.end_instr in
  Printlinear.instr ppf i;
  Format.fprintf ppf "%d: " b.terminator.id;
  C.print_terminator ppf b.terminator;
  ( try
      let t = linearize_terminator t b.terminator in
      Printlinear.instr ppf t
    with _ -> () );
  Format.fprintf ppf "\npredecessors:";
  Label.Set.iter (fun l -> Format.fprintf ppf " %d" l) b.predecessors;
  Format.fprintf ppf "\nsuccessors:";
  List.iter (fun l -> Format.fprintf ppf " %d" l) (C.successor_labels t b)

let print oc cfg layout =
  let ppf = Format.formatter_of_out_channel oc in
  Printf.fprintf oc "\n%s\n" (C.fun_name cfg);
  Printf.fprintf oc "layout.length=%d\n" (List.length layout);
  Printf.fprintf oc "blocks.length=%d\n" (Label.Tbl.length cfg.blocks);
  List.iter
    (fun label ->
      let b = Label.Tbl.find cfg.blocks label in
      print_block cfg ppf label b)
    layout

let print_dot oc cfg layout =
  let name l = Printf.sprintf "\".L%d\"" l in
  let ppf = Format.formatter_of_out_channel oc in
  let print_block_dot label (block : C.basic_block) index =
    let show_index = Option.value index ~default:(-1) in
    Printf.fprintf oc "\n%s [shape=box label=\".L%d:I%d:S%d" (name label)
      label show_index (List.length block.body);
    if !dot_show_instr then begin
      (* This doesn't work because of special characters like { } that need
         to be escaped. Should use sexp to print. *)
      (* Printf.fprintf oc "\\n";
       * let i = List.fold_right linearize_basic block.body Linear.end_instr in
       * Printlinear.instr ppf i;
       * Format.pp_print_flush ppf () *)
      Printf.fprintf oc "\npreds:";
      Label.Set.iter (Printf.fprintf oc " %d") block.predecessors;
      Printf.fprintf oc "\\n(%d)" block.terminator.id;
      C.print_terminator ppf block.terminator;
      Format.pp_print_flush ppf ()
    end;
    Printf.fprintf oc "\"]\n";
    List.iter
      (fun l -> Printf.fprintf oc "%s->%s\n" (name label) (name l))
      (C.successor_labels cfg block)
  in
  Printf.fprintf oc "strict digraph \"%s\" {\n" cfg.fun_name;
  (* print all the blocks, even if they don't appear in the layout *)
  List.iteri (fun index label ->
      let block = Label.Tbl.find cfg.blocks label in
      print_block_dot label block (Some index))
    layout;
  assert (List.length layout <= Label.Tbl.length cfg.blocks);
  if List.length layout < Label.Tbl.length cfg.blocks then begin
    Label.Tbl.iter (fun label block ->
        match List.find_opt (fun lbl -> label = lbl) layout with
        | None -> print_block_dot label block None
        | _ -> ())
      cfg.blocks
  end;
  Printf.fprintf oc "}\n"

let print_layout msg oc cfg layout =
  if !dot_format then (
    let filename =
      Printf.sprintf "%s%s%s.dot"
        (X86_proc.string_of_symbol "" (C.fun_name cfg))
        (if msg = "" then "" else ".")
        msg
    in
    Printf.printf "Writing cfg for %s to %s\n" msg filename;
    let dotc = open_out filename in
    print_dot dotc cfg layout;
    close_out dotc )
  else (
    Printf.printf "cfg for %s\n" msg;
    print oc cfg layout )

let debug_print msg oc cfg_with_layout =
  print_layout msg oc (Cfg_with_layout.cfg cfg_with_layout)
    (Cfg_with_layout.layout cfg_with_layout)

let print_basic ppf basic =
  let i = linearize_basic basic Linear.end_instr in
  Printlinear.instr ppf i
