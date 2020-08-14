open Data_flow_analysis



module ReachingSpillProblem = struct
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

module ReachingSpillSolver = Make_forward_cfg_solver(ReachingSpillProblem)

let is_move inst =
  match inst.Cfg.desc with
  | Cfg.Op Cfg.Spill
  | Cfg.Op Cfg.Reload
  | Cfg.Op Cfg.Move ->
    true
  | _ ->
    false

let sink cfg spill_id reload_id =
  let spill = Inst_id.get_basic cfg spill_id in
  let reload = Inst_id.get_basic cfg reload_id in
  if not (is_move spill) || not (is_move reload) then () else begin
    if spill.Cfg.arg.(0).loc = reload.Cfg.res.(0).loc then begin
      Cfg.print_basic Format.std_formatter spill; Format.printf "\n";
      Cfg.print_basic Format.std_formatter reload; Format.printf "\n\n"
    end
  end

let run cfg =
  (* Find spills which are paired to a single reload. *)
  let solution = ReachingSpillSolver.solve cfg in
  let spills_to_reloads, reloads_to_spills =
    List.fold_left
      (fun acc block ->
        let start = block.Cfg.start in
        fst (List.fold_left
          (fun ((s_to_r, r_to_s) as acc, n) inst ->
            match inst.Cfg.desc with
            | Cfg.Op Cfg.Move | Cfg.Op Cfg.Spill | Cfg.Op Cfg.Reload ->
              let reload_id = Inst_id.Inst(start, n) in
              (match Stack_slot.of_reg inst.Cfg.arg.(0) with
              | Some slot ->
                (match Inst_id.Map.find reload_id solution with
                | { sol_in; _ } ->
                  (match Stack_slot.Map.find slot sol_in with
                  | spills ->
                    let r_to_s = Inst_id.Map.add reload_id spills r_to_s in
                    let s_to_r =
                      Inst_id.Set.fold
                        (fun spill_id acc ->
                          Inst_id.Map.update spill_id (function
                            | None -> Some (Inst_id.Set.singleton reload_id)
                            | Some reloads -> Some(Inst_id.Set.add reload_id reloads)) acc)
                        spills
                        s_to_r
                    in
                    ((s_to_r, r_to_s), n + 1)
                  | exception Not_found -> (acc, n + 1))
                | exception Not_found -> (acc, n + 1))
              | None -> (acc, n + 1))
            | _ -> (acc, n + 1))
          (acc, 0)
          block.Cfg.body)
      )
      (Inst_id.Map.empty, Inst_id.Map.empty)
      (Cfg.blocks cfg)
  in
  let candidates =
    Inst_id.Map.fold
      (fun spill reloads acc ->
        match Inst_id.Set.elements reloads with
        | [reload] ->
          (match Inst_id.Map.find reload reloads_to_spills with
          | spill_set ->
            (match Inst_id.Set.elements spill_set with
            | [spill'] ->
              assert (Inst_id.equal spill spill');
              (spill, reload) :: acc
            | _ -> acc)
          | exception Not_found -> acc)
        | _ -> acc)
      spills_to_reloads
      []
  in
  List.iter (fun (spill_id, reload_id) -> sink cfg spill_id reload_id) candidates
