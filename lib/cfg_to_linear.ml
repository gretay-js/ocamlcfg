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
module L = Linear

let to_linear_instr ?extra_debug ?(like : _ Cfg.instruction option) desc ~next
      : L.instruction =
  let dbg =
    match like with
    | None -> Debuginfo.none
    | Some like ->
      match extra_debug with
      | None -> like.dbg
      | Some file -> Extra_debug.add_discriminator like.dbg file like.id
  in
  let arg, res, live =
    match like with
    | None -> [| |], [| |], Reg.Set.empty
    | Some like -> like.arg, like.res, like.live
  in
  { desc;
    next;
    arg;
    res;
    dbg;
    live;
  }

let from_basic (basic : C.basic) : L.instruction_desc =
  match basic with
  | Prologue -> Lprologue
  | Reloadretaddr -> Lreloadretaddr
  | Entertrap -> Lentertrap
  | Pushtrap { lbl_handler; } -> Lpushtrap { lbl_handler; }
  | Poptrap -> Lpoptrap
  | Call (F (Indirect { label_after; })) ->
    Lop (Icall_ind { label_after; })
  | Call (F (Direct { func_symbol; label_after; })) ->
    Lop (Icall_imm { func = func_symbol; label_after; })
  | Call (P (External { func_symbol; alloc; label_after; })) ->
    Lop (Iextcall { func = func_symbol; alloc; label_after; })
  | Call (P (Checkbound
      { immediate = None; label_after_error; spacetime_index; })) ->
    Lop (Iintop (Icheckbound { label_after_error; spacetime_index; }))
  | Call (P (Checkbound
      { immediate = Some i; label_after_error; spacetime_index; })) ->
    Lop (Iintop_imm (Icheckbound { label_after_error; spacetime_index; }, i))
  | Call (P (Alloc { bytes; label_after_call_gc; spacetime_index; })) ->
    Lop (Ialloc { bytes; label_after_call_gc; spacetime_index; })
  | Op op ->
    let op : Mach.operation =
      match op with
      | Move -> Imove
      | Spill -> Ispill
      | Reload -> Ireload
      | Const_int n -> Iconst_int n
      | Const_float n -> Iconst_float n
      | Const_symbol n -> Iconst_symbol n
      | Stackoffset n -> Istackoffset n
      | Load (c, m) -> Iload (c, m)
      | Store (c, m, b) -> Istore (c, m, b)
      | Intop op -> Iintop op
      | Intop_imm (op, i) -> Iintop_imm (op, i)
      | Negf -> Inegf
      | Absf -> Iabsf
      | Addf -> Iaddf
      | Subf -> Isubf
      | Mulf -> Imulf
      | Divf -> Idivf
      | Floatofint -> Ifloatofint
      | Intoffloat -> Iintoffloat
      | Specific op -> Ispecific op
      | Name_for_debugger
          { ident; which_parameter; provenance; is_assignment; } ->
        Iname_for_debugger
          { ident; which_parameter; provenance; is_assignment; }
    in
    Lop op

let basic_to_linear ?extra_debug (i : _ C.instruction) ~next =
  let desc = from_basic i.desc in
  to_linear_instr ?extra_debug ~like:i desc ~next

let linearize_terminator cfg ?extra_debug
      (terminator : C.terminator C.instruction)
      ~(next : Linear_utils.labelled_insn) =
  let desc_list =
    match terminator.desc with
    | Return -> [L.Lreturn]
    | Raise kind -> [L.Lraise kind]
    | Tailcall (Func (Indirect { label_after; })) ->
      [L.Lop (Itailcall_ind { label_after; })]
    | Tailcall (Func (Direct { func_symbol; label_after; })) ->
      [L.Lop (Itailcall_imm { func = func_symbol; label_after; })]
    | Tailcall (Self { label_after; }) ->
      [L.Lop (Itailcall_imm { func = Cfg.fun_name cfg; label_after; })]
    | Switch labels -> [L.Lswitch labels]
    | Branch successors ->
      match successors with
      | [Always, label] ->
        if next.label = label then [] else [L.Lbranch label]
      | [Test cond_p, label_p; Test cond_q, label_q] ->
        if cond_p <> L.invert_test cond_q then begin
          Misc.fatal_errorf "Malformed branch (conditions are not inverses \
              of each other):@ %a"
            C.print_terminator terminator
        end;
        if label_p = next.label && label_q = next.label then []
        else if label_p <> next.label && label_q <> next.label then
          (* CR-soon gyorsh: if both label are not fall through, then
             arrangement should depend on perf data and possibly the relative
             position of the target labels and the current block: whether the
             jumps are forward or back. This information can be obtained from
             layout but it needs to be made accessible here. *)
          [L.Lcondbranch (cond_p, label_p); Lbranch label_q]
        else if label_p = next.label then
          [L.Lcondbranch (cond_q, label_q)]
        else if label_q = next.label then
          [L.Lcondbranch (cond_p, label_p)]
        else assert false
      | [Test (Iinttest_imm (Iunsigned Clt, 1)), label0;
         Test (Iinttest_imm (Iunsigned Ceq, 1)), label1;
         Test (Iinttest_imm (Iunsigned Cgt, 1)), label2;
        ] ->
        let find l = if next.label = l then None else Some l in
        [L.Lcondbranch3 (find label0, find label1, find label2)]
      | _ ->
        let reason =
          match successors with
          | [] -> " (no successors)"
          | [Test _, _] -> " (successors are non-exhaustive)"
          | _ -> ""
        in
        Misc.fatal_errorf "Malformed branch%s:@ %a"
          reason
          C.print_terminator terminator
  in
  List.fold_left (fun next desc ->
      to_linear_instr ?extra_debug ~like:terminator desc ~next)
    next.insn
    (List.rev desc_list)

let need_starting_label (cfg_with_layout : CL.t)
      (block : C.basic_block) ~(pred_block : C.basic_block) =
  match Label.Set.elements block.predecessors with
  | [] | _::_::_ -> true
  | [pred] when pred <> pred_block.start -> true
  | [_] ->
    (* This block has a single predecessor which appears in the layout
       immediately prior to this block. *)
    if CL.is_trap_handler cfg_with_layout block.start then begin
      (* CR-someday mshinwell: This may need thinking about in conjunction
         with Flambda 2.0, in case we generate a direct jump to an exception
         handler, but still have that handler involved in a push-trap
         operation. *)
      Misc.fatal_errorf "Fallthrough from %d to trap handler %d"
        pred_block.start block.start
    end;
    (* No need for the label, unless the predecessor's terminator is [Switch]
       when the label is needed for the jump table. *)
    (* CR-soon gyorsh: is this correct with label_after for calls? *)
    match pred_block.terminator.desc with
    | Switch _ -> true
    | Branch _ ->
      (* If the label came from the original [Linear] code, preserve it for
         checking that the conversion from [Linear] to [Cfg] and back is the
         identity; and for various assertions in reorder. *)
      let new_labels = CL.new_labels cfg_with_layout in
      CL.preserve_orig_labels cfg_with_layout
        && not (Label.Set.mem block.start new_labels)
    | _ -> assert false

let adjust_trap_depth cfg_with_layout body (block : C.basic_block)
      ~(pred_block : C.basic_block) =
  let block_trap_depth =
    Label.Tbl.find (CL.trap_depths cfg_with_layout) block.start
  in
  let pred_trap_depth = pred_block.terminator.trap_depth in
  if block_trap_depth = pred_trap_depth then body
  else
    let delta_traps = block_trap_depth - pred_trap_depth in
    to_linear_instr (Ladjust_trap_depth { delta_traps; }) ~next:body

(* CR-soon gyorsh: handle duplicate labels in new layout: print the same
   block more than once. *)
let run cfg_with_layout ~extra_debug =
  let cfg = CL.cfg cfg_with_layout in
  let extra_debug =
    if extra_debug then Some (Extra_debug.get_linear_file (C.fun_name cfg))
    else None
  in
  let layout = Array.of_list (CL.layout cfg_with_layout) in
  let len = Array.length layout in
  let next = ref Linear_utils.labelled_insn_end in
  for i = len - 1 downto 0 do
    let label = layout.(i) in
    if not (Label.Tbl.mem cfg.blocks label) then begin
      Misc.fatal_errorf "Unknown block labelled %d\n" label
    end;
    let block = Label.Tbl.find cfg.blocks label in
    assert (label = block.start);
    let body =
      let terminator =
        linearize_terminator cfg ?extra_debug block.terminator ~next:!next
      in
      List.fold_left (fun next i -> basic_to_linear ?extra_debug i ~next)
        terminator
        (List.rev block.body)
    in
    let insn =
      if i = 0 then body (* Entry block of the function. Don't add label. *)
      else
        let pred = layout.(i - 1) in
        let pred_block = Label.Tbl.find cfg.blocks pred in
        let body =
          if not (need_starting_label cfg_with_layout block ~pred_block)
          then body
          else to_linear_instr (Llabel block.start) ~next:body
        in
        adjust_trap_depth cfg_with_layout body block ~pred_block
    in
    next := { label; insn; }
  done;
  !next.insn
