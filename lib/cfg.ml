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

let verbose = ref true

(* CR-soon gyorsh: store label after separately and update after reordering. *)
include Cfg_intf.S

(* CR-soon gyorsh: Switch has successors but currently no way to attach
   User_data to them. Can be fixed by translating Switch to Branch. *)

type basic_block =
  { start : Label.t;
    mutable body : basic instruction list;
    mutable terminator : terminator instruction;
    mutable predecessors : Label.Set.t;
    trap_depth : int;
    mutable exns : Label.Set.t;
    mutable can_raise : bool;
    mutable can_raise_interproc : bool;
    mutable is_trap_handler : bool
  }

type t =
  { blocks : basic_block Label.Tbl.t;
    fun_name : string;
    entry_label : Label.t;
    mutable fun_tailrec_entry_point_label : Label.t
  }

let create ~fun_name ~fun_tailrec_entry_point_label =
  { fun_name;
    entry_label = 1;
    blocks = Label.Tbl.create 31;
    fun_tailrec_entry_point_label
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

let replace_successor_labels_normal t block ~f =
  match block.terminator.desc with
  | Branch successors ->
      let replace_successor (cond, l) = (cond, f l) in
      let new_successors = List.map replace_successor successors in
      block.terminator <-
        { block.terminator with desc = Branch new_successors }
  | Switch labels ->
      let new_labels = Array.map f labels in
      block.terminator <- { block.terminator with desc = Switch new_labels }
  | Tailcall (Self _) ->
      t.fun_tailrec_entry_point_label <- f t.fun_tailrec_entry_point_label
  | Return | Raise _ | Tailcall (Func _) -> ()

let replace_successor_labels t ~normal ~exn block ~f =
  if normal then replace_successor_labels_normal t block ~f;
  if exn then block.exns <- Label.Set.map f block.exns

let successor_labels_normal t block = snd (List.split (successors t block))

let successor_labels t ~normal ~exn block =
  (* CR gyorsh: all normal successors labels should be distinct by
     construction but the conditions that differentiate them might not be
     representable or during a transformation we may temporarily violate this
     invariant, so we do not rely on it here. *)
  match (normal, exn) with
  | false, false -> []
  | true, false -> successor_labels_normal t block
  | false, true -> Label.Set.elements block.exns
  | true, true ->
      Label.Set.elements
        (Label.Set.union block.exns
           (Label.Set.of_list (successor_labels_normal t block)))

let predecessors block = Label.Set.elements block.predecessors

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

let can_raise t =
  let res = ref false in
  iter_blocks t ~f:(fun _ b -> if b.can_raise_interproc then res := true);
  !res

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

let print_test (c : Mach.test) =
  match c with
  | Itruetest -> "true"
  | Ifalsetest -> "false"
  | Iinttest ic -> intcomp ic
  | Iinttest_imm (ic, n) -> intcomp ic ^ Int.to_string n
  | Ifloattest fc -> Printcmm.float_comparison fc
  | Ioddtest -> "odd"
  | Ieventest -> "even"

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
  Printf.fprintf oc "%d: " i.id;
  match i.desc with
  | Op op -> print_op oc op
  | Call call ->
      Printf.fprintf oc "Call ";
      print_call oc call
  | Reloadretaddr -> Printf.fprintf oc "Reloadretaddr"
  | Pushtrap { lbl_handler } ->
      Printf.fprintf oc "Pushtrap handler=%d" lbl_handler
  | Poptrap -> Printf.fprintf oc "Poptrap"
  | Prologue -> Printf.fprintf oc "Prologue"

let print_terminator oc ?(sep = "\n") ti =
  Printf.fprintf oc "%d: " ti.id;
  match ti.desc with
  | Branch successors ->
      Printf.fprintf oc "Branch with %d successors:%s"
        (List.length successors) sep;
      List.iter
        (fun (c, l) ->
          match c with
          | Always -> Printf.fprintf oc "goto %d%s" l sep
          | Test c ->
              Printf.fprintf oc "if %s then goto %d%s" (print_test c) l sep)
        successors
  | Switch labels ->
      Printf.fprintf oc "switch%s" sep;
      for i = 0 to Array.length labels - 1 do
        Printf.fprintf oc "case %d: goto %d%s" i labels.(i) sep
      done
  | Return -> Printf.fprintf oc "Return%s" sep
  | Raise _ -> Printf.fprintf oc "Raise%s" sep
  | Tailcall (Self _) -> Printf.fprintf oc "Tailcall self%s" sep
  | Tailcall _ -> Printf.fprintf oc "Tailcall%s" sep
