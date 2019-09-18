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
type label = Linear.label

module LabelSet = Set.Make (struct
  type t = label

  let compare (x : t) y = compare x y
end)

(* CR-soon gyorsh: store label after separately and update after reordering. *)
type func_call_operation =
  | Indirect of { label_after : label }
  | Immediate of {
      func : string;
      label_after : label;
    }

type tail_call_operation =
  | Self of { label_after : label }
  | Func of func_call_operation

type prim_call_operation =
  | External of {
      func : string;
      alloc : bool;
      label_after : label;
    }
  | Alloc of {
      bytes : int;
      label_after_call_gc : label option;
      spacetime_index : int;
    }
  | Checkbound of {
      immediate : int option;
      label_after_error : label option;
      spacetime_index : int;
    }

type operation =
  | Move
  | Spill
  | Reload
  | Const_int of nativeint
  | Const_float of int64
  | Const_symbol of string
  | Stackoffset of int
  | Load of Cmm.memory_chunk * Arch.addressing_mode
  | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Intop of Mach.integer_operation
  | Intop_imm of Mach.integer_operation * int
  | Negf
  | Absf
  | Addf
  | Subf
  | Mulf
  | Divf
  | Floatofint
  | Intoffloat
  | Specific of Arch.specific_operation
  | Name_for_debugger of {
      ident : Ident.t;
      which_parameter : int option;
      provenance : unit option;
      is_assignment : bool;
    }

type call_operation =
  | P of prim_call_operation
  | F of func_call_operation

type condition =
  | Always
  | Test of Mach.test

type successor = condition * label

(* CR-soon gyorsh: Switch has successors but currently no way to attach
   User_data to them. Can be fixed by translating Switch to Branch. *)

(* basic block *)
type block = {
  start : label;
  mutable body : basic instruction list;
  mutable terminator : terminator instruction;
  mutable predecessors : LabelSet.t;
}

and 'a instruction = {
  desc : 'a;
  arg : Reg.t array;
  res : Reg.t array;
  dbg : Debuginfo.t;
  live : Reg.Set.t;
  trap_depth : int;
  id : int;
}

and basic =
  | Op of operation
  | Call of call_operation
  | Reloadretaddr
  | Entertrap
  | Pushtrap of { lbl_handler : label }
  | Poptrap
  | Prologue

and terminator =
  | Branch of successor list
  | Switch of label array
  | Return
  | Raise of Cmm.raise_kind
  | Tailcall of tail_call_operation

(* Control Flow Graph of a function. *)
type t = {
  (* Map labels to blocks *)
  blocks : (label, block) Hashtbl.t;
  (* Function name, used for printing messages *)
  fun_name : string;
  (* Must be first in all layouts of this cfg. *)
  entry_label : label;
  (* Without prologue, this is the same as entry_label. Otherwise, prologue
     falls through to tailrec_entry. *)
  mutable fun_tailrec_entry_point_label : label;
}

let successors t block =
  match block.terminator.desc with
  | Branch successors -> successors
  | Return -> []
  | Raise _ -> []
  | Tailcall (Self _) -> [ (Always, t.fun_tailrec_entry_point_label) ]
  | Tailcall _ -> []
  | Switch labels ->
      Array.mapi
        (fun i label -> (Test (Iinttest_imm (Iunsigned Ceq, i)), label))
        labels
      |> Array.to_list

let successor_labels t block =
  let _, labels = List.split (successors t block) in
  labels

let print_terminator ppf ti =
  Format.fprintf ppf "\n";
  match ti.desc with
  | Branch successors ->
      Format.fprintf ppf "Branch with %d successors:\n"
        (List.length successors);
      List.iter
        (fun (c, l) ->
          match c with
          | Always -> Format.fprintf ppf "goto %d\n" l
          | Test c ->
              Format.fprintf ppf "if %a then goto %d\n" (Printmach.test c)
                ti.arg l)
        successors
  | Switch labels ->
      Format.fprintf ppf "switch %a of\n" Printmach.reg ti.arg.(0);
      for i = 0 to Array.length labels - 1 do
        Format.fprintf ppf "case %d: goto %d\n" i labels.(i)
      done
  | Return -> Format.fprintf ppf "Return\n"
  | Raise _ -> Format.fprintf ppf "Raise\n"
  | Tailcall (Self _) -> Format.fprintf ppf "Tailcall self\n"
  | Tailcall _ -> Format.fprintf ppf "Tailcall\n"

let print_block t ppf label b ~linearize_basic ~linearize_terminator =
  Format.fprintf ppf "\n%d:\n" label;
  let i = List.fold_right linearize_basic b.body Linear.end_instr in
  Printlinear.instr ppf i;
  Format.fprintf ppf "%d: " b.terminator.id;
  print_terminator ppf b.terminator;
  ( try
      let t = linearize_terminator b.terminator in
      Printlinear.instr ppf t
    with _ -> () );
  Format.fprintf ppf "\npredecessors:";
  LabelSet.iter (fun l -> Format.fprintf ppf " %d" l) b.predecessors;
  Format.fprintf ppf "\nsuccessors:";
  List.iter (fun l -> Format.fprintf ppf " %d" l) (successor_labels t b)

(* CR-soon gyorsh: add dot format output *)
let print oc cfg layout ~linearize_basic ~linearize_terminator =
  let ppf = Format.formatter_of_out_channel oc in
  Printf.fprintf oc "\n%s\n" cfg.fun_name;
  Printf.fprintf oc "layout.length=%d\n" (List.length layout);
  Printf.fprintf oc "blocks.length=%d\n" (Hashtbl.length cfg.blocks);
  List.iter
    (fun label ->
      let b = Hashtbl.find cfg.blocks label in
      print_block cfg ppf label b ~linearize_basic ~linearize_terminator)
    layout

let print_dot oc cfg layout ~linearize_basic:_ ~linearize_terminator:_
    ~dot_show_instr =
  let name l = Printf.sprintf "\".L%d\"" l in
  let ppf = Format.formatter_of_out_channel oc in
  let print_block_dot label block index =
    let show_index = Option.value index ~default:(-1) in
    Printf.fprintf oc "\n%s [shape=box label=\".L%d:I%d:S%d" (name label)
      label show_index (List.length block.body);

    if dot_show_instr then (
      (* This doesn't work because of special characters like { } that need
         to be escaped. Should use sexp to print, like in Eric's instr_freq. *)
      (* Printf.fprintf oc "\\n";
       * let i = List.fold_right linearize_basic block.body Linear.end_instr in
       * Printlinear.instr ppf i;
       * Format.pp_print_flush ppf () *)
      Printf.fprintf oc "\npreds:";
      LabelSet.iter (Printf.fprintf oc " %d") block.predecessors;
      Printf.fprintf oc "\\n(%d)" block.terminator.id;
      print_terminator ppf block.terminator;
      Format.pp_print_flush ppf () );
    Printf.fprintf oc "\"]\n";

    List.iter
      (fun l -> Printf.fprintf oc "%s->%s\n" (name label) (name l))
      (successor_labels cfg block)
  in
  Printf.fprintf oc "strict digraph \"%s\" {\n" cfg.fun_name;

  (* print all the blocks, even if they don't appear in the layout *)
  List.iteri
    (fun index label ->
      let block = Hashtbl.find cfg.blocks label in
      print_block_dot label block (Some index))
    layout;

  assert (List.length layout <= Hashtbl.length cfg.blocks);
  if List.length layout < Hashtbl.length cfg.blocks then
    Hashtbl.iter
      (fun label block ->
        match List.find_opt (fun lbl -> label = lbl) layout with
        | None -> print_block_dot label block None
        | _ -> ())
      cfg.blocks;
  Printf.fprintf oc "}\n"

let print msg oc cfg layout ~linearize_basic ~linearize_terminator
    ~dot_format ~dot_show_instr =
  if dot_format then (
    let filename =
      Printf.sprintf "%s%s%s.dot"
        (X86_proc.string_of_symbol "" cfg.fun_name)
        (if msg = "" then "" else ".")
        msg
    in
    Printf.printf "Writing cfg for %s to %s\n" msg filename;
    let dotc = open_out filename in
    print_dot dotc cfg layout ~linearize_basic ~linearize_terminator
      ~dot_show_instr;
    close_out dotc )
  else (
    Printf.printf "cfg for %s\n" msg;
    print oc cfg layout ~linearize_basic ~linearize_terminator )
