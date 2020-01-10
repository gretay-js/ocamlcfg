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

type labelled_insn =
  { label : Label.t;
    insn : Linear.instruction
  }

let labelled_insn_end = { label = -1; insn = Linear.end_instr }

let rec has_label (i : Linear.instruction) =
  match i.desc with
  | Lend | Llabel _ -> true
  | Ladjust_trap_depth _ -> has_label i.next
  | _ -> false

let to_basic (op : Mach.operation) =
  let open Cfg in
  match op with
  | Icall_ind { label_after } ->
    Call (F (Indirect { label_after }))
  | Icall_imm { func; label_after } ->
    Call (F (Direct { func_symbol = func; label_after }))
  | Iextcall { func; alloc; label_after } ->
    Call
      (P (External { func_symbol = func; alloc; label_after }))
  | Iintop (Icheckbound { label_after_error; spacetime_index }) ->
    Call
      (P
         (Checkbound
            { immediate = None;
              label_after_error;
              spacetime_index
            }))
  | Iintop op -> Op (Intop op)
  | Iintop_imm
      (Icheckbound { label_after_error; spacetime_index }, i) ->
    Call
      (P
         (Checkbound
            { immediate = Some i;
              label_after_error;
              spacetime_index
            }))
  | Iintop_imm (op, i) -> Op (Intop_imm (op, i))
  | Ialloc { bytes; label_after_call_gc; spacetime_index } ->
    Call
      (P (Alloc { bytes; label_after_call_gc; spacetime_index }))
  | Istackoffset i -> Op (Stackoffset i)
  | Iload (c, a) -> Op (Load (c, a))
  | Istore (c, a, b) -> Op (Store (c, a, b))
  | Imove -> Op Move
  | Ispill -> Op Spill
  | Ireload -> Op Reload
  | Iconst_int n -> Op (Const_int n)
  | Iconst_float n -> Op (Const_float n)
  | Iconst_symbol n -> Op (Const_symbol n)
  | Inegf -> Op Negf
  | Iabsf -> Op Absf
  | Iaddf -> Op Addf
  | Isubf -> Op Subf
  | Imulf -> Op Mulf
  | Idivf -> Op Divf
  | Ifloatofint -> Op Floatofint
  | Iintoffloat -> Op Intoffloat
  | Ispecific op -> Op (Specific op)
  | Iname_for_debugger
      { ident; which_parameter; provenance; is_assignment } ->
    Op
      (Name_for_debugger
         { ident; which_parameter; provenance; is_assignment })
  | Itailcall_ind _ | Itailcall_imm _ -> assert false
