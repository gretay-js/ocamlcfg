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

let verbose = false

let entry_id = 1

let last_linear_id = ref entry_id

let get_new_linear_id () =
  let id = !last_linear_id in
  last_linear_id := id + 1;
  id

(* All labels have id 0 because cfg operations can create new labels,
   whereas ids of basic block instructions do not change. *)
(* New terminators introduced by block reordering can also get id=0. *)
(* CR-soon gyorsh: make id into an abstract type to distinguish special
   cases of new ids explicitly. *)

(* From 4.08, LPrologue is added to fun_body, so there is no need to make an
   id for fun_dbg, and no need to use prolog_id instead of entry_id in
   add_linear_discriminators. *)
(* let prolog_id = 1 *)

let create_empty_instruction ?(trap_depth = 0) desc =
  {
    desc;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    trap_depth;
    id = get_new_linear_id ();
  }

let create_empty_block t start =
  let block =
    {
      start;
      body = [];
      terminator = create_empty_instruction (Branch []);
      predecessors = LabelSet.empty;
    }
  in
  if Hashtbl.mem t.cfg.blocks start then
    failwith (Printf.sprintf "Cannot create block, label exists: %d" start);
  t.layout <- start :: t.layout;
  block

let register t block =
  if Hashtbl.mem t.cfg.blocks block.start then
    failwith
      (Printf.sprintf "Cannot register block, label exists: %d" block.start);

  (* Printf.printf "registering block %d\n" block.start *)
  (* Body is constructed in reverse, fix it now: *)
  block.body <- List.rev block.body;
  assert (not (block.terminator.id = 0 && List.length block.body = 0));
  Hashtbl.add t.cfg.blocks block.start block

let register_predecessors t =
  Hashtbl.iter
    (fun label block ->
      let targets = successor_labels block in
      List.iter
        (fun target ->
          let target_block = Hashtbl.find t.cfg.blocks target in
          (* Add label to predecessors of target *)
          target_block.predecessors <-
            LabelSet.add label target_block.predecessors)
        targets)
    t.cfg.blocks

let is_trap_handler t label = Hashtbl.mem t.trap_labels label

let register_split_labels t =
  List.fold_right
    (fun label new_labels_layout ->
      if LabelSet.mem label t.new_labels then
        (* Add a new label to accumulated layout *)
        label :: new_labels_layout
      else (
        (* Original label found *)
        if new_labels_layout <> [] then
          (* The original label was followed by some new ones, which we have
             gathers in new_labels_layout. Tuck on original label and
             register the split layout. *)
          Hashtbl.add t.split_labels label (label :: new_labels_layout);
        [] ))
    t.layout []
  |> ignore

let create_instr desc ~trap_depth (i : Linear.instruction) =
  {
    desc;
    arg = i.arg;
    res = i.res;
    dbg = i.dbg;
    live = i.live;
    trap_depth;
    id = get_new_linear_id ();
  }

let get_or_make_label t (i : Linear.instruction) =
  match i.desc with
  | Llabel label -> { label; insn = i }
  (* | Lbranch _ | Lcondbranch (_,_) | Lcondbranch3(_,_,_)
   *   -> Misc.fatal_errorf "Unexpected branch instead of label @;%a"
   *                  Printlinear.instr i; *)
  | Lend -> failwith "Unexpected end of function instead of label"
  | _ ->
      let label = Cmm.new_label () in
      t.new_labels <- LabelSet.add label t.new_labels;
      { label; insn = Linear.instr_cons (Llabel label) [||] [||] i }

(* Is [i] an existing label? *)
let rec has_label (i : Linear.instruction) =
  match i.desc with
  | Lend | Llabel _ -> true
  | Ladjust_trap_depth _ -> has_label i.next
  | _ -> failwith "Unexpected instruction after terminator"

let mark_trap_label t ~lbl_handler ~lbl_pushtrap_block =
  if Hashtbl.mem t.trap_labels lbl_handler then
    failwith
      (Printf.sprintf
         "Trap hanlder label already exists: Lpushtrap %d from block label \
          %d\n"
         lbl_handler lbl_pushtrap_block);
  Hashtbl.add t.trap_labels lbl_handler lbl_pushtrap_block

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

let record_trap_depth_at_label t label ~trap_depth =
  match Hashtbl.find t.trap_depths label with
  | exception Not_found -> Hashtbl.add t.trap_depths label trap_depth
  | existing_trap_depth ->
      if trap_depth <> existing_trap_depth then
        failwith
          (Printf.sprintf
             "Conflicting trap depths for label %d: already have %d but \
              the following instruction has depth %d"
             label existing_trap_depth trap_depth)

let rec create_blocks t i block ~trap_depth =
  let add_terminator desc =
    block.terminator <- create_instr desc ~trap_depth i;
    register t block
  in
  match i.desc with
  | Lend ->
      (* End of the function. Make sure the previous block is registered. *)
      if not (Hashtbl.mem t.cfg.blocks block.start) then
        failwith
          (Printf.sprintf
             "End of function without terminator for block %d\n" block.start)
  | Llabel start ->
      if verbose then
        Printf.printf "Llabel start=%d, block.start=%d\n" start block.start;

      (* Add the previos block, if it did not have an explicit terminator. *)
      if not (Hashtbl.mem t.cfg.blocks block.start) then (
        (* Previous block falls through. Add start as explicit successor. *)
        let fallthrough = Branch [ (Always, start) ] in
        (* fallthrough terminator gets linear id = 0, like all the labels.
           We create new labels, but preserve linearids from the original
           program, for mapping perf data to cfg. This works for labels
           because they don't correspond to new instructions, but for
           fallthrough after reorder there may be a jump instruction that
           doesn't have linearid. *)
        block.terminator <- create_empty_instruction fallthrough ~trap_depth;
        register t block );

      (* Start a new block *)
      (* CR-soon gyorsh: check for multpile consecutive labels *)
      record_trap_depth_at_label t start ~trap_depth;
      let new_block = create_empty_block t start in
      create_blocks t i.next new_block ~trap_depth
  | Lop (Itailcall_ind { label_after }) ->
      let desc = Tailcall (Indirect { label_after }) in
      assert (has_label i.next);
      add_terminator desc;
      create_blocks t i.next block ~trap_depth
  | Lop (Itailcall_imm { func; label_after }) ->
      let desc = Tailcall (Immediate { func; label_after }) in
      assert (has_label i.next);
      add_terminator desc;
      create_blocks t i.next block ~trap_depth
  | Lreturn ->
      assert (has_label i.next);
      if trap_depth <> 0 then failwith "Trap depth must be zero at Lreturn";
      add_terminator Return;
      create_blocks t i.next block ~trap_depth
  | Lraise kind ->
      assert (has_label i.next);
      add_terminator (Raise kind);
      create_blocks t i.next block ~trap_depth
  | Lbranch lbl ->
      if verbose then Printf.printf "Lbranch %d\n" lbl;
      let successors = [ (Always, lbl) ] in
      assert (has_label i.next);
      record_trap_depth_at_label t lbl ~trap_depth;
      add_terminator (Branch successors);
      create_blocks t i.next block ~trap_depth
  | Lcondbranch (cond, lbl) ->
      (* CR-soon gyorsh: merge (Lbranch | Lcondbranch | Lcondbranch3)+ into
         a single terminator when the argments are the same. Enables
         reordering of branch instructions and save cmp instructions. The
         main problem is that it involves boolean combination of
         conditionals of type Mach.test that can arise from a sequence of
         branches. When all conditions in the combination are integer
         comparisons, we can simplify them into a single condition, but it
         doesn't work for Ieventest and Ioddtest (which come from the
         primitive "is integer"). The advantage is that it will enable us to
         reorder branch instructions to avoid generating jmp to fallthrough
         location in the new order. Also, for linear to cfg and back will be
         harder to generate exactly the same layout. Also, how do we map
         execution counts about branches onto this terminator? *)
      let fallthrough = get_or_make_label t i.next in
      let successors =
        [ (Test cond, lbl); (Test (invert_test cond), fallthrough.label) ]
      in
      add_terminator (Branch successors);
      record_trap_depth_at_label t lbl ~trap_depth;
      record_trap_depth_at_label t fallthrough.label ~trap_depth;
      create_blocks t fallthrough.insn block ~trap_depth
  | Lcondbranch3 (lbl0, lbl1, lbl2) ->
      let fallthrough = get_or_make_label t i.next in
      let get_dest lbl =
        let res =
          match lbl with
          | None -> fallthrough.label
          | Some lbl -> lbl
        in
        record_trap_depth_at_label t res ~trap_depth;
        res
      in
      let s0 = (Test (Iinttest_imm (Iunsigned Clt, 1)), get_dest lbl0) in
      let s1 = (Test (Iinttest_imm (Iunsigned Ceq, 1)), get_dest lbl1) in
      let s2 = (Test (Iinttest_imm (Iunsigned Cgt, 1)), get_dest lbl2) in
      add_terminator (Branch [ s0; s1; s2 ]);
      create_blocks t fallthrough.insn block ~trap_depth
  | Lswitch labels ->
      (* CR-soon gyorsh: get rid of switches entirely and re-generate them
         based on optimization and perf data? *)
      add_terminator (Switch labels);
      Array.iter (record_trap_depth_at_label t ~trap_depth) labels;
      assert (has_label i.next);
      create_blocks t i.next block ~trap_depth
  | Ladjust_trap_depth { delta_traps } ->
      (* We do not emit any executable code for this insn, only moves the
         virtual stack pointer. We do not have an insn in cfg because the
         required adjustment can change when blocks are reordered,
         regenerate it when converting back to linear. We use delta_traps
         only to compute trap_depths of other instructions.*)
      let trap_depth = trap_depth + delta_traps in
      if trap_depth < 0 then
        failwith
          (Printf.sprintf
             "Ladjust_trap_depth %d moves the trap depth below zero: %d"
             delta_traps trap_depth);
      create_blocks t i.next block ~trap_depth
  | Lpushtrap { lbl_handler } ->
      mark_trap_label t ~lbl_handler ~lbl_pushtrap_block:block.start;
      record_trap_depth_at_label t lbl_handler ~trap_depth;
      let desc = Pushtrap { lbl_handler } in
      block.body <- create_instr desc ~trap_depth i :: block.body;
      let trap_depth = trap_depth + 1 in
      create_blocks t i.next block ~trap_depth
  | Lpoptrap ->
      let desc = Poptrap in
      block.body <- create_instr desc ~trap_depth i :: block.body;
      let trap_depth = trap_depth - 1 in
      if trap_depth < 0 then
        failwith "Lpoptrap moves the trap depth below zero";
      create_blocks t i.next block ~trap_depth
  | d ->
      let desc =
        match d with
        | Lprologue -> Prologue
        | Lentertrap -> Entertrap
        | Lreloadretaddr -> Reloadretaddr
        | Lop op -> (
            match op with
            | Icall_ind { label_after } ->
                Call (F (Indirect { label_after }))
            | Icall_imm { func; label_after } ->
                Call (F (Immediate { func; label_after }))
            | Iextcall { func; alloc; label_after } ->
                Call (P (External { func; alloc; label_after }))
            | Iintop op -> (
                match op with
                | Icheckbound { label_after_error; spacetime_index } ->
                    Call
                      (P
                         (Checkbound
                            {
                              immediate = None;
                              label_after_error;
                              spacetime_index;
                            }))
                | _ -> Op (Intop op) )
            | Iintop_imm (op, i) -> (
                match op with
                | Icheckbound { label_after_error; spacetime_index } ->
                    Call
                      (P
                         (Checkbound
                            {
                              immediate = Some i;
                              label_after_error;
                              spacetime_index;
                            }))
                | _ -> Op (Intop_imm (op, i)) )
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
            | Itailcall_ind _ | Itailcall_imm _ -> assert false )
        | Lend | Lreturn | Llabel _ | Lbranch _
        | Lcondbranch (_, _)
        | Lcondbranch3 (_, _, _)
        | Lswitch _ | Lraise _ | Ladjust_trap_depth _ | Lpoptrap
        | Lpushtrap _ ->
            assert false
      in
      block.body <- create_instr desc i ~trap_depth :: block.body;
      create_blocks t i.next block ~trap_depth

let make_empty_cfg name ~preserve_orig_labels =
  let cfg =
    {
      fun_name = name;
      entry_label = 0;
      blocks : (label, block) Hashtbl.t = Hashtbl.create 31;
    }
  in
  {
    cfg;
    trap_labels : (label, label) Hashtbl.t = Hashtbl.create 7;
    trap_depths : (label, int) Hashtbl.t = Hashtbl.create 31;
    layout = [];
    preserve_orig_labels;
  }

let run (f : Linear.fundecl) ~preserve_orig_labels =
  let t = make_empty_cfg f.fun_name ~preserve_orig_labels in
  (* CR-soon gyorsh: label of the function entry must not conflict with
     existing labels. Relies on the invariant: Cmm.new_label() is int > 99.
     An alternative is to create a new type for label here, but it is less
     efficient because label is used as a key to Hashtble. *)
  let entry_block = create_empty_block t t.cfg.entry_label in
  last_linear_id := entry_id;
  create_blocks t f.fun_body entry_block ~trap_depth:0;

  (* Register predecessors now rather than during cfg construction, because
     of forward jumps: the blocks do not exist when the jump that reference
     them is processed. CR-soon gyorsh: combine with dead block elimination. *)
  register_predecessors t;

  (* Layout was constructed in reverse, fix it now: *)
  t.layout <- List.rev t.layout;
  t
