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
(* Based on Linearize *)
[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  { cfg : Cfg.t;
    mutable layout : Label.t list;
    mutable trap_depth : int;
    mutable id : int;
  }

let create cfg =
  { cfg;
    layout = [];
    trap_depth = 0;
    id = 0;
  }

let new_label () = Cmm.new_label ()

let create_empty_instruction ?(trap_depth = 0) id desc : _ C.instruction =
  { desc;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    trap_depth;
    id
  }

let create_instruction id desc ~trap_depth (i : Linear.instruction) :
    _ C.instruction =
  { desc;
    arg = i.arg;
    res = i.res;
    dbg = i.dbg;
    live = i.live;
    trap_depth;
    id
  }

let create_empty_block t start ~trap_depth =
  let block : C.basic_block =
    { start;
      body = [];
      terminator = create_empty_instruction t (C.Branch []);
      exns = Label.Set.empty;
      predecessors = Label.Set.empty;
      trap_depth;
      is_trap_handler = false;
      can_raise = false;
      can_raise_interproc = false
    }
  in
  if C.mem_block t.cfg start then
    Misc.fatal_errorf "A block with starting label %d is already registered"
      start;
  block

let create_block t ?(trap_depth=0) start body terminator =
  let block : C.basic_block =
    { start;
      body;
      terminator;
      exns = Label.Set.empty;
      predecessors = Label.Set.empty;
      trap_depth;
      is_trap_handler = false;
      can_raise = false;
      can_raise_interproc = false
    }
  in
  if C.mem_block t.cfg start then
    Misc.fatal_errorf "A block with starting label %d is already registered"
      start;
  t.layout <- start :: t.layout;
  block


let register_block t (block : C.basic_block) =
  if Label.Tbl.mem t.cfg.blocks block.start then
    Misc.fatal_errorf "A block with starting label %d is already registered"
      block.start;
  if !C.verbose then Printf.printf "registering block %d\n" block.start;
  (* Body is constructed in reverse, fix it now: *)
  block.body <- List.rev block.body;
  Label.Tbl.add t.cfg.blocks block.start block;
  { t with layout = block.start :: t.layout }

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

let block_is_registered t (block : C.basic_block) =
  Label.Tbl.mem t.cfg.blocks block.start

let add_terminator t (block : C.basic_block)
      (desc : C.terminator) (i : L.instruction) ~trap_depth =
  block.terminator <- create_instruction desc i ~trap_depth;
  register_block t block

let rec create_blocks t i start body n =
  let get_start_blocks t i n =
    let start = new_label () in
    create_blocks t i start [] n;
    start
  in
  let start_blocks t i n =
    create_blocks t i (new_label ()) [] n
  in
    match i.Mach.desc with
    | Iend ->
      let ti = create_terminator t Cfg.Branch [(Always,n)] i in
      register_block t start body ti
    | Iop (Itailcall_ind { label_after }) ->
      let desc = Cfg.Tailcall (Func (Indirect { label_after })) in
      let ti = create_terminator t desc i in
      register_block t start body ti;
      start_blocks t i.next n
    | Iop (Itailcall_imm { func = func_symbol; label_after }) ->
      let desc =
        if String.equal func_symbol (Cfg.fun_name t.cfg) then
          Cfg.Tailcall (Self { label_after })
        else Cfg.Tailcall (Func (Direct { func_symbol; label_after }))
      in
      let ti = create_terminator t desc i in
      register_block t start body ti;
      start_blocks t i.next n
    | Ireturn ->
      let body =
        if t.contains_calls then
          (create_basic t Cfg.Lreloadretaddr i) :: body
        else body in
      let ti = create_terminator t Cfg.Return i in
      register_block t start body ti;
      start_blocks t i.next n
    | Iraise k ->
      let ti = create_terminator t (Cfg.Raise k) i in
      register_block t start body ti;
      start_blocks t i.next n
    | Iop op ->
      let bi = create_basic t (Linear_utils.to_basic op) i  in
      create_blocks t start (bi::body) i.next n
    | Iifthenelse (test, ifso, ifnot) ->
      lbl_end = get_start_blocks t i.next n in
      lbl_ifso = get_start_blocks t ifso lbl_end;
      lbl_ifnot = get_start_blocks t ifnot lbl_end;
      let desc = Cfg.Branch [(test,lbl_ifso);(Linear.invert test, lbl_ifnot)] in
      let ti = create_terminator t desc in
      register_block t start body ti;
    | Iswitch(index, cases) ->
      let lbl_end = get_start_blocks t i.next n in
      let lbl_cases = Array.map cases (fun case -> get_start_blocks t case lbl_end) in
      let ti =
        (* Switches with 1 and 2 branches have been eliminated earlier.
         Here, we do something for switches with 3 branches. *)
        if Array.length index = 3 then begin
          let get_dest n = lbl_cases.(index.(n)) in
          let s0 = (C.Test (Iinttest_imm (Iunsigned Clt, 1)), get_dest 0) in
          let s1 = (C.Test (Iinttest_imm (Iunsigned Ceq, 1)), get_dest 1) in
          let s2 = (C.Test (Iinttest_imm (Iunsigned Cgt, 1)), get_dest 2) in
          create_terminator t (Cfg.Branch [s0;s1;s2])
        end else begin
          create_terminator t (Cfg.Switch lbl_cases)
        end in
      register_block t start body ti
    | Iexit nfail ->
      let lbl, t = find_exit_label_try_depth nfail in
      let pops =
        List.init (!try_depth - t) (fun _ -> create_instruction Cfg.Poptrap i) in
      let ti = create_terminator t (Cfg.Branch  [(Always,lbl)]) i in
      register_block t start pops@body ti;
      let delta_traps = !try_depth - t in
      let trap_depth = trap_depth + delta_traps in
      start_blocks t i.next ~trap_depth n
    | Icatch (_rec_flag, handlers, body) -> ()
    | Itrywith(body, handler) -> ()

let run (f : Mach.fundecl) ~preserve_orig_labels =
  let t =
    let cfg =
      Cfg.create ~fun_name:f.fun_name
        ~fun_tailrec_entry_point_label:new_label ()
    in
    create cfg
  in
  let fun_body = create_blocks t tailrec_entry_block f.fun_body  in
  let tailrec_entry_block =
    let ti = (create_terminator t (Branch [(Always, next.start)]) f.fun_body) in
    create_block t ~trap_depth t.cfg.fun_tailrec_entry_point_label []
      ti
  in




  let entry_block =
    create_empty_block t t.cfg.entry_label ~trap_depth
  in
  if Proc.prologue_required f then (
    entry_block.body <- [create_empty_instruction Lprologue ~trap_depth]);
  (* CR-someday gyorsh: insert Iname_for_debugger in front of Lprologue *)
  (* CR-soon gyorsh: recover dbg lost when using create_empty_instruction. *)
  (* CR-soon gyorsh: call dead code and dead block elimination and
     fallthrough elimination. *)

  add_terminator t tailrec_entry_block

  (* Register predecessors now rather than during cfg construction, because
     of forward jumps: the blocks do not exist when the jump that reference
     them is processed. *)
  register_predecessors_for_all_blocks t;
  (* Layout was constructed in reverse, fix it now: *)
  Cfg_with_layout.create t.cfg ~layout:(List.rev t.layout)
    ~preserve_orig_labels ~new_labels:t.new_labels
