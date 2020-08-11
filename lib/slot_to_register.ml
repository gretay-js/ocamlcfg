open Data_flow_analysis


(** Identifier for a stack slot. *)
module Slot = struct
  module T = struct
    type t = { loc: int; reg_class: int }

    let compare a b =
      match compare a.reg_class b.reg_class with
      | 0 -> compare a.loc b.loc
      | c -> c
  end

  include T
  module Map = Map.Make(T)
  module Set = Set.Make(T)
end

(** Set of register identifiers, without any type. *)
module RegSet = Set.Make(Int)

module SlotsInRegs = struct
  type t = (Reg.t * int) Slot.Map.t

  let empty = Slot.Map.empty
  let equal = Slot.Map.equal (fun (_, a) (_, b) -> a = b)

  let lub = Slot.Map.merge (fun _ l r ->
    match l, r with
    | Some (_, a), Some (_, b) when a = b -> l
    | _ -> None)
end

module AvailableSlotProblem = struct
  module K = struct
    module S = SlotsInRegs

    type t =
      { reg_kills: RegSet.t;
        (* List of registers overwritten by the instruction *)
        slot_kills: Slot.Set.t;
        (* List of slots overwritten by the instruction. *)
        gens: (Reg.t * int) Slot.Map.t
        (* Spill instruction which writes a reg to stack *)
      }

    let f s kg =
      Slot.Map.merge
        (fun slot gen prev ->
          match gen, prev with
          | Some _, _ -> gen
          | None, Some _ when Slot.Set.mem slot kg.slot_kills -> None
          | None, Some (_, r) when RegSet.mem r kg.reg_kills -> None
          | None, _ -> prev)
        kg.gens
        s

    let dot curr prev =
      let reg_kills = RegSet.union curr.reg_kills prev.reg_kills in
      let slot_kills = Slot.Set.union curr.slot_kills prev.slot_kills in
      let gens = f prev.gens curr in
      { reg_kills; slot_kills; gens }
  end

  type t = Cfg.t

  let cfg t = t

  let entry _ _ = SlotsInRegs.empty
  let empty _ _ = SlotsInRegs.empty

  let kg t id =
    let open Cfg in
    let open Reg in
    let kill res destroyed =
      let res_kills =
        res
        |> Array.to_list
        |> List.filter_map (fun reg ->
          match reg.loc with
          | Reg r -> Some r
          | _ -> None)
        |> RegSet.of_list
      in
      let slot_kills =
        res
        |> Array.to_list
        |> List.filter_map (fun reg ->
          match reg.loc with
          | Stack (Local loc) -> Some { Slot.loc; reg_class = Proc.register_class reg }
          | _ -> None)
        |> Slot.Set.of_list
      in
      let reg_kills = RegSet.union (RegSet.of_list (Array.to_list destroyed)) res_kills in
      { K.reg_kills; slot_kills; gens = Slot.Map.empty }
    in
    let spill r slot =
      { K.reg_kills = RegSet.empty;
        slot_kills = Slot.Set.empty;
        gens = Slot.Map.singleton slot r }
    in
    match Inst_id.get_inst t id with
    | `Term term ->
      kill term.res (destroyed_at_terminator term.desc)
    | `Basic ({ desc = Op Move; _ } as i)
    | `Basic ({ desc = Op Spill; _ } as i)
    | `Basic ({ desc = Op Reload; _ } as i) ->
      (match i.arg, i.res with
      | [| { loc = Reg r; _ } as src |], [| { loc = Stack (Local loc); _ } as dst |] ->
        spill (src, r) { Slot.loc; reg_class = Proc.register_class dst }
      | _ ->
        kill i.res (destroyed_at_basic i.desc))
    | `Basic i ->
      kill i.res (destroyed_at_basic i.desc)
end

module Liveness = struct
  (* Describes the reason why a slot is live at a program point. *)
  type t
    = Live
    (** The slot is used along a path by a reload which was not rewritten.
      * Liveness info is unchanged.
      *)
    | Extended of Reg.t * int
    (** The slot is used along a path, but the use was rewritten to a register.
      * The register replaces the stack slot in the live set.
      *)
    | ExtendedLive of Reg.t * int
    (** The slot was rewritten to a register, while also being reloaded.
      * The register is added to the live set.
      *)

  let lub a b =
    match a, b with
    | Live, Live ->
      b
    | Live, Extended(reg, r) ->
      ExtendedLive(reg, r)
    | Live, ExtendedLive _ ->
      b
    | Extended(reg, r), Live ->
      ExtendedLive(reg, r)
    | Extended(_, a'), Extended(_, b')
    | Extended(_, a'), ExtendedLive(_, b') ->
      (* The live range should have been extended to only one register. *)
      assert (a' = b'); b
    | ExtendedLive _, Live ->
      a
    | ExtendedLive(_, a'), Extended(_, b')
    | ExtendedLive(_, a'), ExtendedLive(_, b') ->
      assert (a' = b'); a

  let equal a b =
    match a, b with
    | Live, Live -> true
    | Live, Extended _ -> false
    | Live, ExtendedLive _ -> false
    | Extended _, Live -> false
    | Extended(_, a'), Extended(_, b') -> a' = b'
    | Extended _, ExtendedLive _ -> false
    | ExtendedLive _, Live -> false
    | ExtendedLive _, Extended _ -> false
    | ExtendedLive(_, a'), ExtendedLive(_, b') -> a' = b'

  let f a b =
    match a, b with
    | Live, Live ->
      b
    | Live, Extended(reg, r) ->
      ExtendedLive(reg, r)
    | Live, ExtendedLive _ ->
      b
    | Extended(reg, r), Live ->
      ExtendedLive(reg, r)
    | Extended(_, a'), Extended(_, b')
    | Extended(_, a'), ExtendedLive(_, b') ->
      assert (a' = b'); b
    | ExtendedLive _, Live ->
      a
    | ExtendedLive(_, a'), Extended(_, b')
    | ExtendedLive(_, a'), ExtendedLive(_, b') ->
      assert (a' = b'); a
end

module FixupProblem = struct
  module K = struct
    module S = struct
      type t = Liveness.t Slot.Map.t

      let lub = Slot.Map.union (fun _ l r -> Some (Liveness.lub l r))

      let equal = Slot.Map.equal (fun l r -> Liveness.equal l r)
    end

    type t =
      { kills: Slot.Set.t;
        gens: Liveness.t Slot.Map.t;
      }

    let f s kg =
      Slot.Map.merge
        (fun slot l r ->
          let killed = Slot.Set.mem slot kg.kills in
          match l, r with
          | Some l', Some r' when not killed -> Some (Liveness.f l' r')
          | _, Some _ -> r
          | _, None when killed -> None
          | _, None -> l)
        s
        kg.gens

    let dot curr prev =
      let kills = Slot.Set.union curr.kills prev.kills in
      let gens = f prev.gens curr in
      { kills; gens }
  end

  type t =
    { cfg: Cfg.t;
      (* Newly modified cfg. *)
      fixup_map: ((Reg.t * int) Slot.Map.t) Inst_id.Map.t
      (** The fixup map identifies instructions to which the value from a stack
        * slot was forwarded to through a specific register.
        *)
    }

  let cfg { cfg; _ } = cfg

  let entry _ _ = Slot.Map.empty
  let empty _ _ = Slot.Map.empty

  let kg { cfg; fixup_map } id =
    let kills arr =
      arr
      |> Array.to_list
      |> List.filter_map (fun reg ->
        match reg.Reg.loc with
        | Reg.Stack (Reg.Local loc) ->
          Some { Slot.loc; reg_class = Proc.register_class reg}
        | _ -> None)
      |> Slot.Set.of_list
    in
    let gens arg =
      let arg_gens =
        Array.fold_left
          (fun gens reg ->
            match reg.Reg.loc with
            | Reg.Stack (Reg.Local loc) ->
              let slot = { Slot.loc; reg_class = Proc.register_class reg} in
              Slot.Map.add slot Liveness.Live gens
            | _ -> gens)
          Slot.Map.empty
          arg
      in
      match Inst_id.Map.find_opt id fixup_map with
      | Some fixups ->
        Slot.Map.fold
          (fun slot (reg, r) gens ->
            Slot.Map.update slot
              (function
              | None -> Some (Liveness.Extended(reg, r))
              | Some l -> Some (Liveness.lub (Liveness.Extended(reg, r)) l))
              gens)
          fixups
          arg_gens
      | None ->
        arg_gens
    in
    match Inst_id.get_inst cfg id with
    | `Term t -> { K.kills = kills t.Cfg.res; gens = gens t.Cfg.arg }
    | `Basic i -> { K.kills = kills i.Cfg.res; gens = gens i.Cfg.arg }

end

let rewrite_reg id arg ~avail_in =
  match arg.Reg.loc with
  | Reg.Stack (Reg.Local loc)  ->
    let reg_class = Proc.register_class arg in
    let slot = { Slot.loc; reg_class} in
    (match Slot.Map.find_opt slot avail_in with
    | None ->
      arg, []
    | Some (reg, r) ->
      { arg with loc = Reg.Reg r }, [(id, slot, (reg, r))])
  | _ ->
    arg, []

let rewrite inst id ~solution ~fixup =
  match Inst_id.Map.find id solution with
  | avail_in, _ ->
    let new_fixups = ref fixup in
    let arg =
      Array.map
        (fun r ->
          if Array.mem r inst.Cfg.res then r else begin
            let r', fixup = rewrite_reg id r ~avail_in in
            new_fixups := fixup @ !new_fixups;
            r'
          end)
        inst.Cfg.arg
    in
    { inst with arg }, !new_fixups
  | exception Not_found ->
    inst, []

let adjust_liveness live_out inst =
  let live =
    inst.Cfg.live
      |> Slot.Map.fold
          (fun _ reg_or_live live ->
            match reg_or_live with
            | Liveness.Live -> live
            | Liveness.Extended(reg, _)
            | Liveness.ExtendedLive(reg, _) ->
              if Reg.Set.is_empty live
                then live
                else Reg.Set.add reg live)
          live_out
      |> Reg.Set.filter
          (fun r ->
            match r with
            | { Reg.loc = Reg.Stack (Reg.Local loc); _ } ->
              let slot = { Slot.reg_class = Proc.register_class r; loc } in
              (match Slot.Map.find slot live_out with
              | Live -> true
              | ExtendedLive _ -> true
              | Extended _ -> false)
            | _ -> true)
  in
  { inst with live }

module AvailableSlotSolver = Make_forward_cfg_solver(AvailableSlotProblem)
module FixupSolver = Make_backward_cfg_solver(FixupProblem)


let run cfg =
  let solution = AvailableSlotSolver.solve cfg in
  (* Rewrite stack slots with registers if the spilled register happens to be alive. *)
  let fixup = List.fold_left
    (fun fixup bb ->
      let label = bb.Cfg.start in
      let body, _, fixup = List.fold_left
        (fun (body, n, fixup) i ->
          let i', fixup = rewrite i (Inst_id.Inst(label, n)) ~solution ~fixup in
          i' :: body, n + 1, fixup)
        ([], 0, fixup)
        bb.body
      in
      bb.body <- List.rev body;
      let term, fixup = rewrite bb.terminator (Inst_id.Term label) ~solution ~fixup in
      bb.terminator <- term;
      fixup)
    []
    (Cfg.blocks cfg)
  in
  (* If anything was rewritten, live ranges need to be extended. *)
  match fixup with
  | [] -> ()
  | _ ->
    let fixup_map =
      List.fold_left
        (fun acc (id, slot, fixup) ->
          Inst_id.Map.update
            id
            (fun prev ->
              match prev with
              | None -> Some (Slot.Map.singleton slot fixup)
              | Some slots -> Some (Slot.Map.add slot fixup slots))
            acc)
        Inst_id.Map.empty
        fixup
    in
    let solution = FixupSolver.solve { FixupProblem.cfg; fixup_map } in
    Cfg.iter_blocks cfg ~f:(fun label bb ->
      let body, _ = List.fold_left
        (fun (body, n) i->
          match Inst_id.Map.find (Inst_id.Inst(label, n)) solution with
          | live_out, _ when not (Slot.Map.is_empty live_out) ->
            let i' = adjust_liveness live_out i in
            ((match i'.Cfg.desc with
            | Cfg.Op Cfg.Spill ->
              (match i'.Cfg.res with
              | [| { Reg.loc = Reg.Stack (Reg.Local loc); _ } as r |] ->
                let slot = { Slot.loc; reg_class = Proc.register_class r} in
                (match Slot.Map.find slot live_out with
                | Live | ExtendedLive _ ->
                  (* The spill slot is still reloaded somewhere - keep it *)
                  i' :: body
                | Extended _ ->
                  (* All uses of the slot were replaced with the register *)
                  body)
              | _ -> i' :: body)
            | Cfg.Op Cfg.Move | Cfg.Op Cfg.Reload ->
              (match i'.Cfg.arg, i'.Cfg.res with
              | [| arg |], [| res |] when arg.Reg.loc = res.Reg.loc ->
                (* A reload was turned into a NOP - erase it. *)
                body
              | _ -> i' :: body)
            | _ ->
              i' :: body), n + 1)
          | _
          | exception Not_found ->
            i :: body, n + 1)
        ([], 0)
        bb.body
      in
      (match Inst_id.Map.find (Inst_id.Term label) solution with
      | live_out, _ when not (Slot.Map.is_empty live_out) ->
        bb.terminator <- adjust_liveness live_out bb.terminator;
      | _
      | exception Not_found -> ());
      bb.body <- List.rev body)
