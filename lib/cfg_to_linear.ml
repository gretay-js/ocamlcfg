[@@@ocaml.warning "+a-30-40-41-42"]

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

let mk_int_test ~lt ~eq ~gt :  Cmm.integer_comparison =
  match (eq,lt,gt) with
  | (true,false,false) -> Ceq
  | (false,true,false) -> Clt
  | (false,false,true) -> Cgt
  | (false,true,true) -> Cne
  | (true,true,false) -> Cle
  | (true,false,true) -> Cge
  | (true,true,true) -> assert false
  | (false, false, false) -> assert false


(* Certain "unordered" outcomes of float comparisions
   are not expressible as a single Cmm.float_comparison operator,
   or a disjunction of disjoint Cmm.float_comparison operators.

   For these cases, we emit a jump with a comparison operator
   that is not disjoint from previously emitted comparisons
   for this block, and therefore must appear after them.
*)
type float_test = { must_be_last: bool;
                    c : Cmm.float_comparison list;
                  }

let mk_float_test ~lt ~eq ~gt ~uo =
  let any c =
    {
      must_be_last = false;
      c;
    }
  in
  let must_be_last c =
    {
      must_be_last = true;
      c;
    }
  in
  match (eq, lt, gt, uo) with
  | (true,  false, false, false) ->  any [CFeq]
  | (false, true,  false, false ) -> any [CFlt]
  | (false, false, true,  false ) -> any [CFgt]
  | (true,  true,  false, false ) -> any [CFle]
  | (true,  false, true,  false ) -> any [CFge]
  | (false, true,  true,  true ) -> any [CFneq]
  | (true,  false, true,  true ) -> any [CFnlt]
  | (true,  true,  false, true ) -> any [CFngt]
  | (false, false, true,  true ) -> any [CFnle]
  | (false, true,  false, true ) -> any [CFnge]
  | (true,  true,  true,  true ) -> assert false (* unconditional jump *)
  | (false, false, false, false) -> assert false (* no successors *)
  | (true, true, true, false) -> any [CFle;CFgt]
    (* CR-soon gyorsh: how to choose between equivalent representations:
       [CFle;CFgt]
       [CFlt;CFge]
       [CFlt;CFeq;CFgt]
    *)
  | (false, true, true, false) -> any [CFlt;CFgt]
  | (false, false, false, true) -> must_be_last [CFnlt]
  | (true,  false, false, true) -> must_be_last [CFnlt]
  (* XCR mshinwell: Maybe rename Unordered -> Unrepresentable?
     And just double-check these again.

     gyorsh: everything is representable now that we can emit several
     conditional jumps if needed,
     and control the order in which they are emitted.
  *)


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
        (* XCR-someday xclerc: consider switching the ifs below to
         * match eq label_p next.label, eq label_q next.label with
         * | true, true -> []
         * | false, false -> ...
         * *)
        (* CR-soon gyorsh: refactor, a lot of redundant code
           for different cases *)
        (* CR-soon gyorsh: for successor labels that are not fallthrough,
           order of branch instructions should depend on perf data and
           possibly the relative position of the target labels
           and the current block: whether the jumps are forward or back.
           This information can be obtained from the layout. *)
        let branch_or_fallthrough lbl =
          if Label.equal next.label lbl then [] else [L.Lbranch lbl]
        in
        let emit_bool (c1,l1) (c2,l2) =
          match Label.equal l1 next.label, Label.equal l2 next.label with
          | true, true -> []
          | false, true -> [L.Lcondbranch (c1, l1)]
          | true, false -> [L.Lcondbranch (c2, l2)]
          | false, false ->
            if Label.equal l1 l2 then
              [L.Lbranch l1]
            else
              [L.Lcondbranch (c1, l1);
               L.Lcondbranch (c2, l2)]
        in
        match successors with
        | Always label -> branch_or_fallthrough label
        | Is_even {ifso;ifnot} ->
          emit_bool (Ieventest, ifso) (Ioddtest, ifnot)
        | Is_true {ifso;ifnot} ->
          emit_bool (Itruetest, ifso) (Ifalsetest, ifnot)
        | Int_test {lt;eq;gt;imm=Some 1;is_signed=false} ->
          (* XCR xclerc: use `Label.equal`? *)
          let find l = if Label.equal next.label l
            then None else Some l
          in
          [L.Lcondbranch3 (find lt, find eq, find gt)]
        | Float_test {lt;eq;gt;uo;} ->
          let successor_labels =
            Label.Set.singleton lt
            |> Label.Set.add gt
            |> Label.Set.add eq
            |> Label.Set.add uo
          in
          (match Label.Set.cardinal successor_labels with
           | 0 -> assert false
           | 1 -> branch_or_fallthrough (Label.Set.min_elt successor_labels)
           | 2 | 3 | 4 ->
             let emit ~is_last lbl =
               let x = mk_float_test
                         ~lt:(Label.equal lt lbl)
                         ~eq:(Label.equal eq lbl)
                         ~gt:(Label.equal gt lbl)
                         ~uo:(Label.equal uo lbl)
               in
               if x.must_be_last && not is_last then
                 Misc.fatal_error "Illegal branch on floating point \
                                   comparison with unordered";
               List.map (fun cond ->
                 L.Lcondbranch (Ifloattest cond, lbl))
                 x.c
             in
             (* If one of the successor is a fallthrough label,
                do not emit a jump for it.
                Otherwise, the last jump is unconditional.  *)
             let last =
               if Label.Set.mem next.label successor_labels then
                 next.label
               else
                 (* Emit jump to [uo] label last,
                    because in some cases its condition is not disjoint.  *)
                 uo
             in
             let init, target_labels =
               let init = branch_or_fallthrough last in
               let target_labels = (Label.Set.remove last successor_labels) in
               (* If [uo] does need a condition jump, emit it last,
                  because in some cases its condition is not disjoint. *)
               if Label.Set.mem uo target_labels then
                 (emit ~is_last:true uo)@init,
                 (Label.Set.remove uo successor_labels)
               else
                 init, target_labels
             in
             Label.Set.fold (fun lbl acc -> (emit ~is_last:false lbl)@acc)
               target_labels
               init
           | _ -> assert false)
        | Int_test {lt;eq;gt;imm;is_signed} ->
          let successor_labels =
            Label.Set.singleton lt
            |> Label.Set.add gt
            |> Label.Set.add eq
          in
          (match Label.Set.cardinal successor_labels with
           | 0 -> assert false
           | 1 -> branch_or_fallthrough (Label.Set.min_elt successor_labels)
           | 2 | 3 ->
             (* If fallthrough label is a successor, do not emit a jump for it.
                Otherwise, the last jump could be unconditional. *)
             let last =
               if Label.Set.mem next.label successor_labels then
                 next.label
               else
                 Label.Set.min_elt successor_labels
             in
             let init = branch_or_fallthrough last in
             Label.Set.fold (fun lbl acc ->
                 let cond = mk_int_test
                              ~lt:(Label.equal lt lbl)
                              ~eq:(Label.equal eq lbl)
                              ~gt:(Label.equal gt lbl)
                 in
                 let comp =
                   match is_signed with
                   | true -> Mach.Isigned cond
                   | false -> Mach.Iunsigned cond
                 in
                 let test =
                   match imm with
                   | None -> Mach.Iinttest comp
                   | Some n -> Iinttest_imm (comp, n)
                 in
                 L.Lcondbranch (test, lbl)::acc)
               (Label.Set.remove last successor_labels)
               init
           | _ -> assert false)
      )
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
        | Return | Raise _ | Tailcall _
          -> (* XCR xclerc: rather enumerate the constructors? *)
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


