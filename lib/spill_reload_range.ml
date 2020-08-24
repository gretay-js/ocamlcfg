open Data_flow_analysis

module RegMap = Map.Make(Int)

module RegSet = Set.Make(Int)

module Range = struct
  (** If [id, reg, slot] is in this map at a program point, it means that the
      instruction is a spill or a reload operating on the reg-slot pair and it
      can be moved without conflict to this program point. *)
  type t = Inst_id.Set.t RegMap.t Stack_slot.Map.t

  let bot = Stack_slot.Map.empty

  (* An instruction can be hoisted into a join point if it is available on any path. *)
  let lub = Stack_slot.Map.merge
    (fun _ regs_a regs_b ->
      match regs_a, regs_b with
      | Some regs_a, Some regs_b ->
        let live_regs =
          RegMap.merge
            (fun _ insts_a insts_b ->
              match insts_a, insts_b with
              | Some insts_a, Some insts_b ->
                let live_insts = Inst_id.Set.inter insts_a insts_b in
                if Inst_id.Set.is_empty live_insts then None else Some live_insts
              | _ -> None)
          regs_a
          regs_b
        in
        if RegMap.is_empty live_regs then None else Some live_regs
      | _ -> None)

  let equal = Stack_slot.Map.equal (RegMap.equal Inst_id.Set.equal)

  let _print fmt t =
    let insts =
      Stack_slot.Map.fold
        (fun slot regs acc ->
          RegMap.fold
            (fun reg insts acc->
              Inst_id.Set.fold (fun id acc -> (slot, reg, id) :: acc) insts acc)
            regs
            acc)
        t
        []
    in
    Format.fprintf fmt "[";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
      (fun fmt (slot, reg, id) ->
        Format.fprintf fmt "(%a,%d,%a)" Stack_slot.print slot reg Inst_id.print id)
      fmt
      insts;
    Format.fprintf fmt "]"

  let to_id_set t =
    Stack_slot.Map.fold
      (fun _ -> RegMap.fold (fun _  -> Inst_id.Set.fold Inst_id.Set.add))
      t
      Inst_id.Set.empty
end

module Range_action = struct
  module S = Range
  module G = struct
    type t =
      { gens: Inst_id.Set.t RegMap.t Stack_slot.Map.t
      ; reg_kills: RegSet.t
      ; slot_kills: Stack_slot.Set.t
      }

    let apply s kg =
      Stack_slot.Map.merge
        (fun slot regs_gen regs_s ->
          let live_regs_s =
            match regs_s with
            | None -> None
            | Some _ when Stack_slot.Set.mem slot kg.slot_kills -> None
            | Some regs ->
              let live_regs =
                RegMap.filter (fun reg _ -> not (RegSet.mem reg kg.reg_kills)) regs
              in
              if RegMap.is_empty live_regs then None else Some live_regs
          in
          match regs_gen, live_regs_s with
          | Some regs_gen, Some regs_s ->
            Some (RegMap.union
              (fun _ insts_gen insts_s -> Some (Inst_id.Set.union insts_gen insts_s))
              regs_gen
              regs_s)
          | Some _, None -> regs_gen
          | None, _ -> live_regs_s)
        kg.gens
        s

    let dot curr prev =
      let reg_kills = RegSet.union curr.reg_kills prev.reg_kills in
      let slot_kills = Stack_slot.Set.union curr.slot_kills prev.slot_kills in
      let gens = apply prev.gens curr in
      { gens; reg_kills; slot_kills }
  end

  let apply = G.apply
end

module type Range_kind = sig
  val kind : [ `Spill | `Reload ]
end

module Make_range_problem (P: Range_kind) = struct
  module A = Range_action

  type t = Cfg.t

  let cfg t = t

  let entry _ _ = A.S.bot

  let action t id =
    let open Cfg in
    let open Reg in
    let make_action gens arg res destroyed =
      let res_reg_kills, res_slot_kills =
        Array.fold_left
          (fun (reg_kills, slot_kills) reg ->
            match Stack_slot.of_reg reg with
            | Some slot -> reg_kills, Stack_slot.Set.add slot slot_kills
            | None ->
              (match reg.loc with
              | Reg r -> RegSet.add r reg_kills, slot_kills
              | _ -> reg_kills, slot_kills))
          (destroyed |> Array.to_list |> RegSet.of_list, Stack_slot.Set.empty)
          res
      in
      let arg_reg_kills, arg_slot_kills =
        Array.fold_left
          (fun (reg_kills, slot_kills) reg ->
            match P.kind with
            | `Spill ->
              let slot_kills =
                match Stack_slot.of_reg reg with
                | None -> slot_kills
                | Some slot -> Stack_slot.Set.add slot slot_kills
              in
              reg_kills, slot_kills
            | `Reload ->
              let reg_kills =
                match reg.loc with
                | Reg r -> RegSet.add r reg_kills
                | _ -> reg_kills
              in
              reg_kills, slot_kills)
          (RegSet.empty, Stack_slot.Set.empty)
          arg
      in
      let reg_kills = RegSet.union arg_reg_kills res_reg_kills in
      let slot_kills = Stack_slot.Set.union arg_slot_kills res_slot_kills in
      { A.G.gens; reg_kills; slot_kills }
    in
    match Inst_id.get_inst t id with
    | `Basic b ->
      let singleton slot r =
        Stack_slot.Map.singleton slot (RegMap.singleton r (Inst_id.Set.singleton id))
      in
      let gens =
        match b.desc with
        | Op Spill | Op Move | Op Reload ->
          (match b.arg, b.res, P.kind with
          | [| { loc = Reg r; _ } |], [| dst |], `Spill ->
            (match Stack_slot.of_reg dst with
            | None -> Stack_slot.Map.empty
            | Some slot -> singleton slot r)
          | [| src |], [| { loc = Reg r; _ } |], `Reload ->
            (match Stack_slot.of_reg src with
            | None -> Stack_slot.Map.empty
            | Some slot -> singleton slot r)
          | _ -> Stack_slot.Map.empty)
        | _ -> Stack_slot.Map.empty
      in
      make_action gens b.arg b.res (Cfg.destroyed_at_basic b.desc)
    | `Term t ->
      make_action Stack_slot.Map.empty t.arg t.res (Cfg.destroyed_at_terminator t.desc)
end

let ids_to_sets t =
  Inst_id.Map.map
    (fun { sol_in; sol_out } ->
      { sol_in = Range.to_id_set sol_in; sol_out = Range.to_id_set sol_out })
    t

type t = Inst_id.Set.t Data_flow_analysis.solution Inst_id.Map.t

let solve_spills t =
  let module S =
    Make_forward_cfg_solver(Make_range_problem(struct let kind = `Spill end))
  in
  ids_to_sets (S.solve t)

let solve_reloads t =
  let module S =
    Make_backward_cfg_solver(Make_range_problem(struct let kind = `Reload end))
  in
  ids_to_sets (S.solve t)
