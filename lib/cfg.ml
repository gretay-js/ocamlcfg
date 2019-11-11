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

[@@@ocaml.warning "+a-4-30-40-41-42"]

let verbose = ref false

(* CR-soon gyorsh: store label after separately and update after reordering. *)
include Cfg_intf.S

(* CR-soon gyorsh: Switch has successors but currently no way to attach
   User_data to them. Can be fixed by translating Switch to Branch. *)

type basic_block =
  { start : Label.t;
    mutable body : basic instruction list;
    mutable terminator : terminator instruction;
    mutable traps : Label.Set.t;
    mutable predecessors : Label.Set.t;
    mutable trap_depth : int;
    mutable is_trap_handler : bool;
    mutable can_raise : bool
  }

type t =
  { blocks : basic_block Label.Tbl.t;
    fun_name : string;
    entry_label : Label.t;
    mutable fun_tailrec_entry_point_label : Label.t;
    mutable id_to_label : Label.t Numbers.Int.Map.t
  }

let create ~fun_name ~fun_tailrec_entry_point_label =
  { fun_name;
    entry_label = 1;
    blocks = Label.Tbl.create 31;
    fun_tailrec_entry_point_label;
    id_to_label = Numbers.Int.Map.empty
  }

let successors t block =
  match block.terminator.desc with
  | Branch successors -> successors
  | Tailcall (Self _) -> [(Always, t.fun_tailrec_entry_point_label)]
  | Switch labels ->
      Array.mapi
        (fun i label -> (Test (Iinttest_imm (Iunsigned Ceq, i)), label))
        labels
      |> Array.to_list
  | Return | Raise _ | Tailcall _ -> []

let successor_labels t ~normal ~exn block =
  List.concat
    [ ( if normal then
        let _, labels = List.split (successors t block) in
        labels
      else [] );
      (if exn then Label.Set.elements block.exns else []) ]

let mem_block t label = Label.Tbl.mem t.blocks label

let get_and_remove_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found -> Misc.fatal_errorf "Block %d not found" label
  | block ->
      Label.Tbl.remove t.blocks label;
      block

let get_block t label = Label.Tbl.find_opt t.blocks label

let get_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found -> Misc.fatal_errorf "Block %d not found" label
  | block -> block

let fun_name t = t.fun_name

let entry_label t = t.entry_label

let fun_tailrec_entry_point_label t = t.fun_tailrec_entry_point_label

let set_fun_tailrec_entry_point_label t label =
  t.fun_tailrec_entry_point_label <- label

let iter_blocks t ~f = Label.Tbl.iter f t.blocks

let id_to_label t id =
  match Numbers.Int.Map.find_opt id t.id_to_label with
  | None ->
      Misc.fatal_errorf "Cannot find label for ID %d in [id_to_label]:@ %a"
        id
        (Numbers.Int.Map.print Label.print)
        t.id_to_label
  | Some lbl ->
      if !verbose then
        Printf.printf "Found label %d for id %d in map\n" lbl id;
      Some lbl

(* CR-soon gyorsh: if a block is eliminated after id_to_label is computed, we
   might return a label that doesn't exist. Except that we would never ask
   for that id because there won't be a corresponding debug line recorded in
   perf data for it. *)
let compute_id_to_label t =
  let fold_block label block id_to_label =
    let id_to_label =
      List.fold_left
        (fun id_to_label (i : _ instruction) ->
          Numbers.Int.Map.add i.id label id_to_label)
        id_to_label block.body
    in
    Numbers.Int.Map.add block.terminator.id label id_to_label
  in
  t.id_to_label <- Label.Tbl.fold fold_block t.blocks Numbers.Int.Map.empty

(* Printing for debug *)

(* The next 2 functions are copied almost as is from asmcomp/printmach.ml
   because there is no interface to call them. Eventually this won't be
   needed when we change cfg to have its own types rather than referring back
   to mach and cmm. *)
(* CR-someday gyorsh: better printing, using the dreaded format? *)
let intcomp (comp : Mach.integer_comparison) =
  match comp with
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.integer_comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.integer_comparison c)

let intop (op : Mach.integer_operation) =
  match op with
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh -> " *h "
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior -> " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Icomp cmp -> intcomp cmp
  | _ -> assert false

let print_op oc = function
  | Move -> Printf.fprintf oc "mov"
  | Spill -> Printf.fprintf oc "spill"
  | Reload -> Printf.fprintf oc "reload"
  | Const_int n -> Printf.fprintf oc "const_int %nd" n
  | Const_float f -> Printf.fprintf oc "const_float %Ld" f
  | Const_symbol s -> Printf.fprintf oc "const_symbol %s" s
  | Stackoffset n -> Printf.fprintf oc "stackoffset %d" n
  | Load _ -> Printf.fprintf oc "load"
  | Store _ -> Printf.fprintf oc "store"
  | Intop op -> Printf.fprintf oc "intop %s" (intop op)
  | Intop_imm (op, n) -> Printf.fprintf oc "intop %s %d" (intop op) n
  | Negf -> Printf.fprintf oc "negf"
  | Absf -> Printf.fprintf oc "absf"
  | Addf -> Printf.fprintf oc "addf"
  | Subf -> Printf.fprintf oc "subf"
  | Mulf -> Printf.fprintf oc "mulf"
  | Divf -> Printf.fprintf oc "divf"
  | Floatofint -> Printf.fprintf oc "floattoint"
  | Intoffloat -> Printf.fprintf oc "intoffloat"
  | Specific _ -> Printf.fprintf oc "specific"
  | Name_for_debugger _ -> Printf.fprintf oc "name_for_debugger"

let print_call oc = function
  | P prim_call -> (
      match prim_call with
      | External { func_symbol : string; _ } ->
          Printf.fprintf oc "external %s" func_symbol
      | Alloc { bytes : int; _ } -> Printf.fprintf oc "alloc %d" bytes
      | Checkbound _ -> Printf.fprintf oc "checkbound" )
  | F func_call -> (
      match func_call with
      | Indirect _ -> Printf.fprintf oc "indirect"
      | Direct { func_symbol : string; _ } ->
          Printf.fprintf oc "direct %s" func_symbol )

(* CR-someday: implement desc printing, and args/res/dbg, etc, properly, with
   regs, use the dreaded Format. *)
let print_basic oc i =
  Printf.fprintf oc "%d:\n" i.id;
  match i.desc with
  | Op op ->
      Printf.fprintf oc "Op ";
      print_op oc op;
      Printf.fprintf oc "\n"
  | Call call ->
      Printf.fprintf oc "Call ";
      print_call oc call;
      Printf.fprintf oc "\n"
  | Reloadretaddr -> Printf.fprintf oc "Reloadretaddr\n"
  | Entertrap -> Printf.fprintf oc "Entertrap\n"
  | Pushtrap { lbl_handler } ->
      Printf.fprintf oc "Pushtrap handler=%d\n" lbl_handler
  | Poptrap -> Printf.fprintf oc "Poptrap\n"
  | Prologue -> Printf.fprintf oc "Prologue\n"

let print_terminator oc ti =
  Printf.fprintf oc "%d:\n" ti.id;
  match ti.desc with
  | Branch successors ->
      Printf.fprintf oc "Branch with %d successors:\n"
        (List.length successors);
      List.iter
        (fun (c, l) ->
          match c with
          | Always -> Printf.fprintf oc "goto %d\n" l
          | Test c ->
              let ppf = Format.formatter_of_out_channel oc in
              Format.fprintf ppf "if %a then goto %d\n" (Printmach.test c)
                ti.arg l)
        successors
  | Switch labels ->
      Printf.fprintf oc "switch\n";
      for i = 0 to Array.length labels - 1 do
        Printf.fprintf oc "case %d: goto %d\n" i labels.(i)
      done
  | Return -> Printf.fprintf oc "Return\n"
  | Raise _ -> Printf.fprintf oc "Raise\n"
  | Tailcall (Self _) -> Printf.fprintf oc "Tailcall self\n"
  | Tailcall _ -> Printf.fprintf oc "Tailcall\n"
