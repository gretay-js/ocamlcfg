open Data_flow_analysis

module Problem = struct
  module A = struct
    module S = struct
      type t = Inst_id.Set.t Stack_slot.Map.t

      let lub a b =
        Stack_slot.Map.union (fun _ a' b' -> Some (Inst_id.Set.union a' b')) a b

      let equal = Stack_slot.Map.equal Inst_id.Set.equal

      let bot = Stack_slot.Map.empty
    end

    module G = struct
      type t = Inst_id.t Stack_slot.Map.t

      let dot curr prev =
        Stack_slot.Map.merge
          (fun _ cid pid ->
            match cid, pid with
            | Some _, _ -> cid
            | _, _ -> pid)
          curr
          prev
    end

    let apply s kg =
      Stack_slot.Map.merge
        (fun _ cid pset ->
          match cid, pset with
          | Some id, _ -> Some (Inst_id.Set.singleton id)
          | None, _ -> pset)
        kg
        s
  end

  type t = Cfg.t

  let cfg t = t

  let entry _ _ = Stack_slot.Map.empty

  let action t id =
    let kill_gen res =
      res
      |> Array.to_list
      |> List.filter_map Stack_slot.of_reg
      |> List.fold_left
        (fun acc slot -> Stack_slot.Map.add slot id acc)
        Stack_slot.Map.empty
    in
    match Inst_id.get_inst t id with
    | `Term t -> kill_gen t.res
    | `Basic i -> kill_gen i.res
end

include Make_forward_cfg_solver(Problem)
