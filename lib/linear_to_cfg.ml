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
module L = Linear
module T = Trap_stack

type t =
  { cfg : Cfg.t;
    mutable layout : Label.t list;
    (* Labels added by cfg construction, except entry. Used for testing of
       the mapping back to Linear IR. *)
    mutable new_labels : Label.Set.t;
    (* All traps pushed in this function. Used to check that all trap
       handlers start with Entertrap, to make sure that is_trap_handler set
       for these blocks, which is needed to convert them back to linear (and
       restore [Lentertrap]). *)
    (* CR mshinwell: The above comment refers to Entertrap as a CFG
       construction, but it doesn't seem to exist *)
    mutable trap_handlers : Label.Set.t;
    (* Maps labels of blocks to trap handler stacks at the beginning of the
       block *)
    (* CR-soon gyorsh: The need for this is because we need to record
       trap_stack for a block that hasn't been created yet. If we change
       create_empty_block to get_or_create, then we don't need this hashtbl
       and will be able to store the trap_stacks directly within their nodes. *)
    trap_stacks : T.t Label.Tbl.t;
    (* Maps labels to trap stacks that can be raised in that block. This
       won't be needed after block splitting, as it will be uniquely
       determined by the top of the trap stack. *)
    exns : T.t list Label.Tbl.t
  }

let create cfg =
  { cfg;
    layout = [];
    new_labels = Label.Set.empty;
    trap_handlers = Label.Set.empty;
    trap_stacks = Label.Tbl.create 31;
    exns = Label.Tbl.create 31
  }

(* CR-soon gyorsh: implement CFG traversal *)

(* CR-soon gyorsh: abstraction of cfg updates that transparently and
   efficiently keeps predecessors and successors in sync. For example, change
   successors relation should automatically updates the predecessors relation
   without recomputing them from scratch. *)

(* CR-soon gyorsh: Optimize terminators. Implement switch as jump table or as
   a sequence of branches depending on perf counters and layout. It should be
   implemented as a separate transformation on the cfg, that needs to be
   informated by the layout, but it should not be done while emitting linear.
   This way we can keep to_linear as simple as possible to ensure basic
   invariants are preserved, while other optimizations can be turned on and
   off. *)

let entry_id = 1

let last_linear_id = ref entry_id

let get_new_linear_id () =
  let id = !last_linear_id in
  last_linear_id := id + 1;
  id

let create_empty_instruction ?(trap_depth = 0) desc : _ C.instruction =
  { desc;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    trap_depth;
    id = get_new_linear_id ()
  }

let create_instruction desc ~trap_depth (i : Linear.instruction) :
    _ C.instruction =
  { desc;
    arg = i.arg;
    res = i.res;
    dbg = i.dbg;
    live = i.live;
    trap_depth;
    id = get_new_linear_id ()
  }

let record_traps t label traps =
  match Label.Tbl.find_opt t.trap_stacks label with
  | None -> Label.Tbl.add t.trap_stacks label traps
  | Some existing_traps ->
      if !C.verbose then
        T.print_pair
          (Printf.sprintf "Unify at %d" label)
          traps existing_traps;
      T.unify traps existing_traps;
      if !C.verbose then (
        Printf.printf "after: ";
        T.print existing_traps )

let record_exn t (block : C.basic_block) traps =
  block.can_raise <- true;
  match Label.Tbl.find_opt t.exns block.start with
  | None -> Label.Tbl.add t.exns block.start [traps]
  | Some ls -> Label.Tbl.replace t.exns block.start (traps :: ls)

let create_empty_block t start ~trap_depth ~traps =
  let block : C.basic_block =
    { start;
      body = [];
      terminator = create_empty_instruction (C.Branch []);
      exns = Label.Set.empty;
      predecessors = Label.Set.empty;
      trap_depth;
      is_trap_handler = false;
      can_raise = false;
      can_raise_interproc = false
    }
  in
  record_traps t start traps;
  if C.mem_block t.cfg start then
    Misc.fatal_errorf "A block with starting label %d is already registered"
      start;
  t.layout <- start :: t.layout;
  block

let register_block t (block : C.basic_block) traps =
  if Label.Tbl.mem t.cfg.blocks block.start then
    Misc.fatal_errorf "A block with starting label %d is already registered"
      block.start;
  if !C.verbose then Printf.printf "registering block %d\n" block.start;
  (* Body is constructed in reverse, fix it now: *)
  block.body <- List.rev block.body;
  (* Update trap stacks of successor blocks. *)
  (* CR gyorsh: do we need to update traps of exns successors? what do we put
     there? we probably need to pop traps before propagating to the handler,
     but can it be unified at the handler? is it not the whole point that a
     handler can be reached with different trap stacks dynamically? Should it
     be a union stacks rather than unify them? Then, we need set of stacks
     everywhere and how do we unify two sets?
     As discussed:
     - think about and/or update comment re. first sentence
     - unique trap depth per handler. *)
  List.iter
    (fun label -> record_traps t label traps)
    (C.successor_labels t.cfg ~normal:true ~exn:false block);
  Label.Tbl.add t.cfg.blocks block.start block

let can_raise_basic (i : C.basic) =
  (* CR mshinwell: Make match exhaustive
     In fact, I would try enabling warning 4 everywhere (just remove it
     from the @@@ocaml.warning stanza at the top of each file), to catch
     more of these cases.  Warning 4 can be overkill sometimes but for this
     library it's probably ok. *)
  match i with
  | Call _ -> true
  (* CR xclerc for xclerc: double check that Op (... int div)  cannot raise. *)
  (* CR xclerc for xclerc: double check that Op (Specific ...) cannot raise *)
  | _ -> false

let can_raise_terminator (i : C.terminator) =
  (* CR mshinwell: Make match exhaustive *)
  match i with
  | Raise _ | Tailcall _ -> true
  | _ -> false

(* CR mshinwell: This function should be renamed, since it sounds like an
   invariant-checking function at the moment, but actually updates
   [block.exns]. *)
let check_trap t label (block : C.basic_block) =
  match Label.Tbl.find_opt t.trap_stacks label with
  | None -> ()
  | Some traps -> (
      try
        if !C.verbose then (
          Printf.printf "%s: check_trap at %d: " t.cfg.fun_name label;
          T.print traps );
        let trap_stack = T.to_list_exn traps in
        let d = List.length trap_stack in
        if not (block.trap_depth = d) then
          Misc.fatal_errorf
            "Malformed linear IR: mismatch trap_depth=%d,but trap_stack \
             length=%d"
            block.trap_depth d;
        (* All exns in this block must be based off the trap_stack above,
           which was successfully resolved. *)
        (* CR mshinwell: Clarify what "resolved" means *)
        match Label.Tbl.find_opt t.exns label with
        | None -> ()
        | Some exns ->
            let f acc l =
              (* CR mshinwell: We can check something about
                 [block.trap_depth] based on the return value from
                 [T.top_exn], no? *)
              (* CR mshinwell: Call this variable [trap_stack] or something
                 rather than [l]. *)
              match T.top_exn l with
              | None ->
                  block.can_raise_interproc <- true;
                  acc
              | Some l -> Label.Set.add l acc
            in
            block.exns <- List.fold_left f Label.Set.empty exns;
            if !C.verbose then (
              Printf.printf "%s: %d exn stacks at %d: " t.cfg.fun_name
                (List.length exns) label;
              List.iter T.print exns;
              Printf.printf "%s: %d exns at %d: " t.cfg.fun_name
                (Label.Set.cardinal block.exns)
                label;
              Label.Set.iter (Printf.printf "%d ") block.exns;
              Printf.printf "\n" )
      with T.Unresolved ->
        (* must be dead block or flow from exception handler only *)
        if !C.verbose then
          Printf.printf
            "unknown trap stack at label %d, the block must be dead, or \
             there is a bug in trap stacks."
            label )

let check_traps t =
  (* check that all blocks referred to in pushtraps are marked as
     trap_handlers *)
  Label.Set.iter
    (fun label ->
      let trap_block = C.get_block_exn t.cfg label in
      (* CR mshinwell: This again references the mythical Entertrap *)
      if not trap_block.is_trap_handler then
        Misc.fatal_errorf "Label %d used in pushtrap but has no Entertrap."
          label)
    t.trap_handlers;
  (* check that trap stacks of all blocks are resolved, unless the block has
     no predecessors, and compute block.exn successors using t.exn. *)
  C.iter_blocks t.cfg ~f:(check_trap t);
  (* after all exn successors are computed, check that if a block can_raise,
     then it has a registered exn successor or interproc exn. *)
  let f _ (block : C.basic_block) =
    let n = Label.Set.cardinal block.exns in
    assert (n >= 0); (* CR xclerc: what is it supposed to check? *)
    assert ((not block.can_raise_interproc) || block.can_raise);
    assert ((not block.can_raise) || n > 0 || block.can_raise_interproc)
  in
  C.iter_blocks t.cfg ~f

let register_predecessors_for_all_blocks t =
  Label.Tbl.iter
    (fun label block ->
      let targets = C.successor_labels ~normal:true ~exn:true t.cfg block in
      List.iter
        (fun target ->
          let target_block = Label.Tbl.find t.cfg.blocks target in
          target_block.predecessors <-
            Label.Set.add label target_block.predecessors)
        targets)
    t.cfg.blocks

let get_or_make_label t (insn : Linear.instruction) :
    Linear_utils.labelled_insn =
  match insn.desc with
  | Llabel label -> { label; insn }
  | Lend -> Misc.fatal_error "Unexpected end of function instead of label"
  | _ ->
      let label = Cmm.new_label () in
      t.new_labels <- Label.Set.add label t.new_labels;
      let insn = Linear.instr_cons (Llabel label) [||] [||] insn in
      { label; insn }

let block_is_registered t (block : C.basic_block) =
  Label.Tbl.mem t.cfg.blocks block.start

let add_terminator t (block : C.basic_block) (i : L.instruction)
    (desc : C.terminator) ~trap_depth ~traps =
  ( match desc with
    (* all terminators are followed by a label, except branches we created for
       fallthroughs in Linear. *)
    (* CR gyorsh for mshinwell: you asked in the previous review round:
       "What exactly determines which ones of these must be followed by a label?"
       and I put in the comment above, but then you removed the case for Branch,
       I think, not sure why, and it is needed. I put it back. *)
    | Branch _ -> ()
    | Switch _ | Return | Raise _ | Tailcall _ ->
      if not (Linear_utils.has_label i.next) then
        Misc.fatal_errorf "Linear instruction not followed by label:@ %a"
          Printlinear.instr
          { i with next = Linear.end_instr } );
  (* CR-soon gyorsh: simplify terminator *)
  block.terminator <- create_instruction desc ~trap_depth i;
  if can_raise_terminator desc then record_exn t block traps;
  register_block t block traps

(** [traps] represents the trap stack, with head being the top. [trap_depths]
    is the depth of the trap stack. *)
let rec create_blocks t (i : L.instruction) (block : C.basic_block)
    ~trap_depth ~traps =
  (* [traps] is constructed incrementally, because Ladjust_trap does not give
     enough information to compute it upfront, but trap_depth is directly
     computed. *)
  match i.desc with
  | Lend ->
      if not (block_is_registered t block) then
        Misc.fatal_errorf "End of function without terminator for block %d"
          block.start
  | Llabel start ->
      if !C.verbose then
        Printf.printf "Llabel start=%d, block.start=%d\n" start block.start;
      (* Labels always cause a new block to be created. If the block prior to
         the label falls through, an explicit successor edge is added. *)
      if not (block_is_registered t block) then (
        let fallthrough : C.terminator = Branch [(Always, start)] in
        block.terminator <- create_empty_instruction fallthrough ~trap_depth;
        register_block t block traps );
      (* CR-soon gyorsh: check for multiple consecutive labels *)
      let new_block = create_empty_block t start ~trap_depth ~traps in
      create_blocks t i.next new_block ~trap_depth ~traps
  | Lop (Itailcall_ind { label_after }) ->
      let desc = C.Tailcall (Func (Indirect { label_after })) in
      add_terminator t block i desc ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Lop (Itailcall_imm { func = func_symbol; label_after }) ->
      let desc =
        if String.equal func_symbol (C.fun_name t.cfg) then
          C.Tailcall (Self { label_after })
        else C.Tailcall (Func (Direct { func_symbol; label_after }))
      in
      add_terminator t block i desc ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Lreturn ->
      if trap_depth <> 0 then
        Misc.fatal_errorf "Trap depth must be zero at Lreturn";
      add_terminator t block i Return ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Lraise kind ->
      (* CR gyorsh: Why does the compiler not generate adjust after
         raise? raise pops the trap handler stack and then the next block may
         have a different try depth. Also, why do we not need to update
         trap_depths and traps here like for pop?
         mshinwell: I don't think there's anything special about a raise for
         Linearize; it may still add a trap adjustment.  Think about this some
         more...
         "raise" does two things:
         1. Moves the stack pointer back to the place it was in the most
            recent trap
         2. Jumps to the handler.
         Step 1 is actually the "pop trap" operation.
         So the stack on the handler identified by the label on the top of
         the trap stack must equal the current trap stack minus the top
         frame. *)
      add_terminator t block i (Raise kind) ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Lbranch lbl ->
      if !C.verbose then Printf.printf "Lbranch %d\n" lbl;
      add_terminator t block i (Branch [(Always, lbl)]) ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Lcondbranch (cond, lbl) ->
      (* CR-soon gyorsh: merge (Lbranch | Lcondbranch | Lcondbranch3)+ into a
         single terminator when the argments are the same. Enables reordering
         of branch instructions and save cmp instructions. The main problem
         is that it involves boolean combination of conditionals of type
         Mach.test that can arise from a sequence of branches. When all
         conditions in the combination are integer comparisons, we can
         simplify them into a single condition, but it doesn't work for
         Ieventest and Ioddtest (which come from the primitive "is integer").
         The advantage is that it will enable us to reorder branch
         instructions to avoid generating jmp to fallthrough location in the
         new order. Also, for linear to cfg and back will be harder to
         generate exactly the same layout. Also, how do we map execution
         counts about branches onto this terminator? *)
      let fallthrough = get_or_make_label t i.next in
      add_terminator t block i
        (Branch
           [(Test cond, lbl); (Test (L.invert_test cond), fallthrough.label)])
        ~trap_depth ~traps;
      create_blocks t fallthrough.insn block ~trap_depth ~traps
  | Lcondbranch3 (lbl0, lbl1, lbl2) ->
      let fallthrough = get_or_make_label t i.next in
      let get_dest lbl = Option.value lbl ~default:fallthrough.label in
      let s0 = (C.Test (Iinttest_imm (Iunsigned Clt, 1)), get_dest lbl0) in
      let s1 = (C.Test (Iinttest_imm (Iunsigned Ceq, 1)), get_dest lbl1) in
      let s2 = (C.Test (Iinttest_imm (Iunsigned Cgt, 1)), get_dest lbl2) in
      add_terminator t block i (Branch [s0; s1; s2]) ~trap_depth ~traps;
      create_blocks t fallthrough.insn block ~trap_depth ~traps
  | Lswitch labels ->
      (* CR-soon gyorsh: get rid of switches entirely and re-generate them
         based on optimization and perf data? *)
      add_terminator t block i (Switch labels) ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Ladjust_trap_depth { delta_traps } ->
      (* We do not emit any executable code for this insn; it only moves the
         virtual stack pointer in the emitter. We do not have a corresponding
         insn in [Cfg] because the required adjustment can change when blocks
         are reordered. Instead we regenerate the instructions when
         converting back to linear. We use [delta_traps] only to compute
         [trap_depth]s of other instructions. *)
      let trap_depth = trap_depth + delta_traps in
      if trap_depth < 0 then
        Misc.fatal_errorf
          "Ladjust_trap_depth %d moves the trap depth below zero: %d"
          delta_traps trap_depth;
      let traps = T.unknown () in
      create_blocks t i.next block ~trap_depth ~traps
  | Lpushtrap { lbl_handler } ->
      t.trap_handlers <- Label.Set.add lbl_handler t.trap_handlers;
      record_traps t lbl_handler traps;
      let desc = C.Pushtrap { lbl_handler } in
      block.body <- create_instruction desc ~trap_depth i :: block.body;
      let trap_depth = trap_depth + 1 in
      let traps = T.push traps lbl_handler in
      create_blocks t i.next block ~trap_depth ~traps
  | Lpoptrap ->
      let desc = C.Poptrap in
      block.body <- create_instruction desc ~trap_depth i :: block.body;
      let trap_depth = trap_depth - 1 in
      if trap_depth < 0 then
        Misc.fatal_error "Lpoptrap moves the trap depth below zero";
      if !C.verbose then (
        Printf.printf "before pop: ";
        T.print traps );
      let traps = T.pop traps in
      if !C.verbose then (
        Printf.printf "after pop: ";
        T.print traps );
      create_blocks t i.next block ~trap_depth ~traps
  | Lentertrap ->
      (* Must be the first instruction in the block. *)
      assert (List.length block.body = 0);
      block.is_trap_handler <- true;
      create_blocks t i.next block ~trap_depth ~traps
  | desc ->
      let desc : C.basic =
        match desc with
        | Lprologue -> Prologue
        | Lreloadretaddr -> Reloadretaddr
        | Lop op -> (
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
            | Itailcall_ind _ | Itailcall_imm _ -> assert false )
        | Lend | Lreturn | Llabel _ | Lbranch _
        | Lcondbranch (_, _)
        | Lcondbranch3 (_, _, _)
        | Lswitch _ | Lraise _ | Ladjust_trap_depth _ | Lpoptrap
        | Lpushtrap _ | Lentertrap ->
            assert false
      in
      block.body <- create_instruction desc i ~trap_depth :: block.body;
      if can_raise_basic desc then record_exn t block traps;
      create_blocks t i.next block ~trap_depth ~traps

let run (f : Linear.fundecl) ~preserve_orig_labels =
  let t =
    let cfg =
      Cfg.create ~fun_name:f.fun_name
        ~fun_tailrec_entry_point_label:f.fun_tailrec_entry_point_label
    in
    create cfg
  in
  (* CR-soon gyorsh: label of the function entry must not conflict with
     existing labels. Relies on the invariant: Cmm.new_label() is int > 99.
     An alternative is to create a new type for label here, but it is less
     efficient because label is used as a key to Label.Tbl. mshinwell: I
     don't think a new type needs to cause any change in efficiency. A custom
     hashtable can be made with the appropriate hashing and equality
     functions. *)
  let traps = T.emp in
  let trap_depth = 0 in
  let entry_block =
    create_empty_block t t.cfg.entry_label ~trap_depth ~traps
  in
  last_linear_id := entry_id;
  create_blocks t f.fun_body entry_block ~trap_depth ~traps;
  (* Register predecessors now rather than during cfg construction, because
     of forward jumps: the blocks do not exist when the jump that reference
     them is processed. *)
  check_traps t;
  register_predecessors_for_all_blocks t;
  (* Layout was constructed in reverse, fix it now: *)
  Cfg_with_layout.create t.cfg ~layout:(List.rev t.layout)
    ~preserve_orig_labels ~new_labels:t.new_labels
