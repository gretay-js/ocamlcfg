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
[@@@ocaml.warning "+a-4-30-40-41-42-44-45"]

open Linear
open Cfg_builder
open Cfg

let verbose = false

(* Set desc and next from inputs and the rest is empty *)
let make_simple_linear desc next =
  {
    desc;
    next;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
  }

(* Set desc and next from inputs and copy the rest from i *)
let to_linear_instr ?extra_debug ~i desc next =
  let dbg =
    match extra_debug with
    | None -> i.dbg
    | Some file -> Extra_debug.add_discriminator i.dbg file i.id
  in
  { desc; next; arg = i.arg; res = i.res; dbg; live = i.live }

let from_basic = function
  | Prologue -> Lprologue
  | Reloadretaddr -> Lreloadretaddr
  | Entertrap -> Lentertrap
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Call (F (Indirect { label_after })) -> Lop (Icall_ind { label_after })
  | Call (F (Immediate { func; label_after })) ->
      Lop (Icall_imm { func; label_after })
  | Call (P (External { func; alloc; label_after })) ->
      Lop (Iextcall { func; alloc; label_after })
  | Call
      (P
        (Checkbound
          { immediate = None; label_after_error; spacetime_index })) ->
      Lop (Iintop (Icheckbound { label_after_error; spacetime_index }))
  | Call
      (P
        (Checkbound
          { immediate = Some i; label_after_error; spacetime_index })) ->
      Lop
        (Iintop_imm (Icheckbound { label_after_error; spacetime_index }, i))
  | Call (P (Alloc { bytes; label_after_call_gc; spacetime_index })) ->
      Lop (Ialloc { bytes; label_after_call_gc; spacetime_index })
  | Op op -> (
      match op with
      | Move -> Lop Imove
      | Spill -> Lop Ispill
      | Reload -> Lop Ireload
      | Const_int n -> Lop (Iconst_int n)
      | Const_float n -> Lop (Iconst_float n)
      | Const_symbol n -> Lop (Iconst_symbol n)
      | Stackoffset n -> Lop (Istackoffset n)
      | Load (c, m) -> Lop (Iload (c, m))
      | Store (c, m, b) -> Lop (Istore (c, m, b))
      | Intop op -> Lop (Iintop op)
      | Intop_imm (op, i) -> Lop (Iintop_imm (op, i))
      | Negf -> Lop Inegf
      | Absf -> Lop Iabsf
      | Addf -> Lop Iaddf
      | Subf -> Lop Isubf
      | Mulf -> Lop Imulf
      | Divf -> Lop Idivf
      | Floatofint -> Lop Ifloatofint
      | Intoffloat -> Lop Iintoffloat
      | Specific op -> Lop (Ispecific op)
      | Name_for_debugger
          { ident; which_parameter; provenance; is_assignment } ->
          Lop
            (Iname_for_debugger
               { ident; which_parameter; provenance; is_assignment }) )

let basic_to_linear ?extra_debug i next =
  let desc = from_basic i.desc in
  to_linear_instr desc next ~i ?extra_debug

let linearize_terminator ?extra_debug terminator ~next =
  let desc_list =
    match terminator.desc with
    | Return -> [ Lreturn ]
    | Raise kind -> [ Lraise kind ]
    | Tailcall (Indirect { label_after }) ->
        [ Lop (Itailcall_ind { label_after }) ]
    | Tailcall (Immediate { func; label_after }) ->
        [ Lop (Itailcall_imm { func; label_after }) ]
    | Switch labels -> [ Lswitch labels ]
    | Branch successors -> (
        match successors with
        | [] ->
            if verbose then Printf.printf "next label is %d\n" next.label;
            failwith "Branch without successors"
        | [ (Always, label) ] ->
            if next.label = label then [] else [ Lbranch label ]
        | [ (Test _, _) ] -> failwith "Successors not exhastive"
        | [ (Test cond_p, label_p); (Test cond_q, label_q) ] ->
            if not (cond_p = invert_test cond_q) then (
              Printf.fprintf stderr
                "Cannot linearize branch with non-invert:\n";
              Print.print_terminator
                (Format.formatter_of_out_channel stderr)
                terminator;
              failwith "Illegal successors"
              (* [Lcondbranch(cond_p,label_p); Lcondbranch(cond_q,label_q);
                 ] *) )
            else if label_p = next.label && label_q = next.label then []
            else if label_p <> next.label && label_q <> next.label then
              (* CR-soon gyorsh: if both label are not fall through, then
                 arrangement should depend on perf data and possibly the
                 relative position of the target labels and the current
                 block: whether the jumps are forward or back. This
                 information can be obtained from layout but it needs to be
                 made accessible here. *)
              [ Lcondbranch (cond_p, label_p); Lbranch label_q ]
            else if label_p = next.label then
              [ Lcondbranch (cond_q, label_q) ]
            else if label_q = next.label then
              [ Lcondbranch (cond_p, label_p) ]
            else assert false
        | [
         (Test (Iinttest_imm (Iunsigned Clt, 1)), label0);
         (Test (Iinttest_imm (Iunsigned Ceq, 1)), label1);
         (Test (Iinttest_imm (Iunsigned Cgt, 1)), label2);
        ] ->
            let find_label l = if next.label = l then None else Some l in
            [
              Lcondbranch3
                (find_label label0, find_label label1, find_label label2);
            ]
        | _ -> assert false )
  in
  List.fold_right
    (to_linear_instr ?extra_debug ~i:terminator)
    desc_list next.insn

let need_label (t : Cfg_builder.t) block pred_block =
  (* Can we drop the start label for this block or not? *)
  if block.predecessors = LabelSet.singleton pred_block.start then (
    (* This block has a single predecessor which appears in the layout
       immediately prior to this block. *)
    if is_trap_handler t block.start then
      failwith
        (Printf.sprintf "Fallthrough from %d to trap handler %d\n"
           pred_block.start block.start);

    (* No need for the label, unless the predecessor's terminator is switch
       and then the label is needed for the jump table. *)
    (* CR-soon gyorsh: is this correct with label_after for calls? *)
    match pred_block.terminator.desc with
    | Switch _ -> true
    | Branch _ ->
        (* If label is original, preserve it for checking that linear to cfg
           and back is identity, and for various assertions in reorder. *)
        if
          LabelSet.mem block.start t.new_labels
          || not t.preserve_orig_labels
        then false
        else true
    | _ -> assert false )
  else true

let adjust_trap t body block pred_block =
  (* Adjust trap depth *)
  let block_trap_depth = Hashtbl.find t.trap_depths block.start in
  let pred_trap_depth = pred_block.terminator.trap_depth in
  if block_trap_depth != pred_trap_depth then
    let delta_traps = block_trap_depth - pred_trap_depth in
    make_simple_linear (Ladjust_trap_depth { delta_traps }) body
  else body

(* CR-soon gyorsh: handle duplicate labels in new layout: print the same
   block more than once. *)
let run t ~extra_debug =
  let extra_debug =
    if extra_debug then Some (Extra_debug.get_linear_file t.cfg.fun_name)
    else None
  in
  let layout = Array.of_list t.layout in
  let len = Array.length layout in
  let next = ref labelled_insn_end in
  for i = len - 1 downto 0 do
    let label = layout.(i) in
    if not (Hashtbl.mem t.cfg.blocks label) then
      failwith (Printf.sprintf "Unknown block labelled %d\n" label);
    let block = Hashtbl.find t.cfg.blocks label in
    assert (label = block.start);
    let terminator =
      linearize_terminator ?extra_debug block.terminator ~next:!next
    in
    let body =
      List.fold_right (basic_to_linear ?extra_debug) block.body terminator
    in
    let insn =
      if i = 0 then body (* Entry block of the function. Don't add label. *)
      else
        let pred = layout.(i - 1) in
        let pred_block = Hashtbl.find t.cfg.blocks pred in
        let body =
          if need_label t block pred_block then
            make_simple_linear (Llabel block.start) body
          else body
        in
        adjust_trap t body block pred_block
    in
    next := { label; insn }
  done;
  !next.insn

let debug_print oc t =
  let extra_debug = None in
  Print.print oc t.cfg t.layout
    ~linearize_basic:(basic_to_linear ?extra_debug)
    ~linearize_terminator:
      (linearize_terminator ?extra_debug ~next:labelled_insn_end)
