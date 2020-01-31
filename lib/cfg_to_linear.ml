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

module CL = Cfg_with_layout
module L = Linear

let to_linear_instr ?(like : _ Cfg.instruction option) desc ~next :
    L.instruction =
  let arg, res, dbg, live =
    match like with
    | None -> ([||], [||], Debuginfo.none, Reg.Set.empty)
    | Some like -> (like.arg, like.res, like.dbg, like.live)
  in
  { desc; next; arg; res; dbg; live }

let from_basic (basic : Cfg.basic) : L.instruction_desc =
  match basic with
  | Prologue -> Lprologue
  | Reloadretaddr -> Lreloadretaddr
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Call (F (Indirect { label_after })) -> Lop (Icall_ind { label_after })
  | Call (F (Direct { func_symbol; label_after })) ->
      Lop (Icall_imm { func = func_symbol; label_after })
  | Call (P (External { func_symbol; alloc; label_after })) ->
      Lop (Iextcall { func = func_symbol; alloc; label_after })
  | Call
      (P
        (Checkbound { immediate = None; label_after_error; spacetime_index }))
    ->
      Lop (Iintop (Icheckbound { label_after_error; spacetime_index }))
  | Call
      (P
        (Checkbound
          { immediate = Some i; label_after_error; spacetime_index })) ->
      Lop
        (Iintop_imm (Icheckbound { label_after_error; spacetime_index }, i))
  | Call (P (Alloc { bytes; label_after_call_gc; spacetime_index })) ->
      Lop (Ialloc { bytes; label_after_call_gc; spacetime_index })
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
            { ident; which_parameter; provenance; is_assignment } ->
            Iname_for_debugger
              { ident; which_parameter; provenance; is_assignment }
      in
      Lop op

let basic_to_linear (i : _ Cfg.instruction) ~next =
  let desc = from_basic i.desc in
  to_linear_instr ~like:i desc ~next

let linearize_terminator cfg (terminator : Cfg.terminator Cfg.instruction)
    ~(next : Linear_utils.labelled_insn) =
  let desc_list =
    match terminator.desc with
    | Return -> [L.Lreturn]
    | Raise kind -> [L.Lraise kind]
    | Tailcall (Func (Indirect { label_after })) ->
        [L.Lop (Itailcall_ind { label_after })]
    | Tailcall (Func (Direct { func_symbol; label_after })) ->
        [L.Lop (Itailcall_imm { func = func_symbol; label_after })]
    | Tailcall (Self { label_after }) ->
        [L.Lop (Itailcall_imm { func = Cfg.fun_name cfg; label_after })]
    | Switch labels -> [L.Lswitch labels]
    | Branch successors -> (
        match successors with
        | [(Always, label)] ->
            if next.label = label then [] else [L.Lbranch label]
        | [(Test cond_p, label_p); (Test cond_q, label_q)] ->
            if cond_p <> L.invert_test cond_q then (
              Cfg.print_terminator stderr terminator;
              Misc.fatal_error
                "Malformed branch: conditions are not inverses of each \
                 other." );
            (* CR-someday xclerc: consider switching the ifs below to
             * match eq label_p next.label, eq label_q next.label with
             * | true, true -> []
             * | false, false -> ...
             * *)
            if label_p = next.label && label_q = next.label then []
            else if label_p <> next.label && label_q <> next.label then
              (* CR-soon gyorsh: if both label are not fall through, then
                 arrangement should depend on perf data and possibly the
                 relative position of the target labels and the current
                 block: whether the jumps are forward or back. This
                 information can be obtained from layout but it needs to be
                 made accessible here. *)
              [L.Lcondbranch (cond_p, label_p); Lbranch label_q]
            else if label_p = next.label then
              [L.Lcondbranch (cond_q, label_q)]
            else if label_q = next.label then
              [L.Lcondbranch (cond_p, label_p)]
            else assert false
        | [ (Test (Iinttest_imm (Iunsigned Clt, 1)), label0);
            (Test (Iinttest_imm (Iunsigned Ceq, 1)), label1);
            (Test (Iinttest_imm (Iunsigned Cgt, 1)), label2) ] ->
          let find l = if next.label = l (* CR xclerc: use `Label.equal`? *) then None else Some l in
            [L.Lcondbranch3 (find label0, find label1, find label2)]
        | _ ->
            let reason =
              match successors with
              | [] -> "no successors"
              | [(Test _, _)] -> "successors are non-exhaustive"
              | _ -> ""
            in
            Cfg.print_terminator stderr terminator;
            Misc.fatal_errorf "Malformed branch %s" reason )
  in
  List.fold_left
    (fun next desc -> to_linear_instr ~like:terminator desc ~next)
    next.insn (List.rev desc_list)

let need_starting_label (cfg_with_layout : CL.t) (block : Cfg.basic_block)
    ~(prev_block : Cfg.basic_block) =
  if block.is_trap_handler then true
  else
    match Label.Set.elements block.predecessors with
    | [] | _ :: _ :: _ -> true
    | [pred] when pred <> prev_block.start -> true
    | [_] -> (
        (* This block has a single predecessor which appears in the layout
           immediately prior to this block. *)
        (* No need for the label, unless the predecessor's terminator is
           [Switch] when the label is needed for the jump table. *)
        (* CR-soon gyorsh: is this correct with label_after for calls? *)
        match prev_block.terminator.desc with
        | Switch _ -> true
        | Branch _ ->
            (* If the label came from the original [Linear] code, preserve it
               for checking that the conversion from [Linear] to [Cfg] and
               back is the identity; and for various assertions in reorder. *)
            let new_labels = CL.new_labels cfg_with_layout in
            CL.preserve_orig_labels cfg_with_layout
            && not (Label.Set.mem block.start new_labels)
        | _ -> (* CR xclerc: rather enumerate the constructors? *)
          assert false )

let adjust_trap_depth body (block : Cfg.basic_block)
    ~(prev_block : Cfg.basic_block) =
  let block_trap_depth = block.trap_depth in
  let prev_trap_depth = prev_block.terminator.trap_depth in
  if block_trap_depth = prev_trap_depth then body
  else
    let delta_traps = block_trap_depth - prev_trap_depth in
    to_linear_instr (Ladjust_trap_depth { delta_traps }) ~next:body

(* CR-soon gyorsh: handle duplicate labels in new layout: print the same
   block more than once. *)
let run cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  let layout = Array.of_list (CL.layout cfg_with_layout) in
  let len = Array.length layout in
  let next = ref Linear_utils.labelled_insn_end in
  for i = len - 1 downto 0 do
    let label = layout.(i) in
    if not (Label.Tbl.mem cfg.blocks label) then
      Misc.fatal_errorf "Unknown block labelled %d\n" label;
    let block = Label.Tbl.find cfg.blocks label in
    assert (label = block.start);
    let body =
      let terminator =
        linearize_terminator cfg block.terminator ~next:!next
      in
      List.fold_left
        (fun next i -> basic_to_linear i ~next)
        terminator (List.rev block.body)
    in
    let insn =
      if i = 0 then body (* Entry block of the function. Don't add label. *)
      else
        let body =
          if block.is_trap_handler then to_linear_instr Lentertrap ~next:body
          else body
        in
        let prev = layout.(i - 1) in
        let prev_block = Label.Tbl.find cfg.blocks prev in
        let body =
          if not (need_starting_label cfg_with_layout block ~prev_block) then
            body
          else to_linear_instr (Llabel block.start) ~next:body
        in
        adjust_trap_depth body block ~prev_block
    in
    next := { label; insn }
  done;
  !next.insn

(** debug print block as assembly *)
let print_assembly (blocks : Cfg.basic_block list) =
  (* create a fake cfg just for printing these blocks *)
  let layout = List.map (fun (b : Cfg.basic_block) -> b.start) blocks in
  let fun_name = "_fun_start_" in
  let fun_tailrec_entry_point_label = 0 in
  let cfg = Cfg.create ~fun_name ~fun_tailrec_entry_point_label in
  List.iter
    (fun (block : Cfg.basic_block) ->
      Label.Tbl.add cfg.blocks block.start block)
    blocks;
  let cl =
    Cfg_with_layout.create cfg ~layout ~new_labels:Label.Set.empty
      ~preserve_orig_labels:true
  in
  let fun_body = run cl in
  let fundecl =
    { Linear.fun_name;
      fun_body;
      fun_fast = false;
      fun_dbg = Debuginfo.none;
      fun_spacetime_shape = None;
      fun_num_stack_slots = Array.make Proc.num_register_classes 0;
      fun_frame_required = false;
      fun_prologue_required = false;
      fun_contains_calls = false;
      fun_tailrec_entry_point_label
    }
  in
  X86_proc.reset_asm_code ();
  Emit.fundecl fundecl;
  X86_proc.generate_code
    (Some (X86_gas.generate_asm !Emitaux.output_channel))
