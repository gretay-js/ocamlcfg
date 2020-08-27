open Data_flow_analysis


module FixupProblem = struct
  module A = struct
    module S = struct
      type t = Reg.t Phys_loc.Map.t

      let lub = Phys_loc.Map.union (fun _ a _ -> Some a)

      let equal = Phys_loc.Map.equal (fun _ _ -> true)

      let bot = Phys_loc.Map.empty
    end

    module G = struct
      type t =
        { kills: Phys_loc.Set.t;
          gens: Reg.t Phys_loc.Map.t;
        }

      let dot curr prev =
        let kills = Phys_loc.Set.union curr.kills prev.kills in
        let gens =
          Phys_loc.Map.merge
            (fun key gen_curr gen_prev ->
              match gen_curr, gen_prev with
              | Some _, _ -> gen_curr
              | None, Some _ when not (Phys_loc.Set.mem key curr.kills) -> gen_prev
              | None, _ -> None)
            curr.gens
            prev.gens
        in
        { kills; gens }
    end

    let apply s kg =
      s
      |> Phys_loc.Set.fold
          (fun key s -> Phys_loc.Map.remove key s)
          kg.G.kills
      |> Phys_loc.Map.fold
          (fun key data s -> Phys_loc.Map.update key (fun _ -> Some data) s)
          kg.G.gens
  end

  type t = Cfg.t

  let cfg t = t

  let entry _ _ = Phys_loc.Map.empty

  let action cfg id =
    let kill_gen arg res destroyed =
      let kills =
        Phys_loc.Set.union
          (destroyed
            |> Array.to_seq
            |> Seq.map (fun r -> Phys_loc.Reg r)
            |> Phys_loc.Set.of_seq)
          (res
            |> Array.to_seq
            |> Seq.filter_map Phys_loc.of_reg
            |> Phys_loc.Set.of_seq)
      in
      let gens =
        Array.fold_left
          (fun acc reg ->
            match Phys_loc.of_reg reg with
            | Some loc -> Phys_loc.Map.add loc reg acc
            | None -> acc)
          Phys_loc.Map.empty
          arg
      in
      { A.G.kills; gens }
    in
    match Inst_id.get_inst cfg id with
    | `Basic b -> kill_gen b.Cfg.arg b.Cfg.res (Cfg.destroyed_at_basic b.Cfg.desc)
    | `Term t -> kill_gen t.Cfg.arg t.Cfg.res (Cfg.destroyed_at_terminator t.Cfg.desc)
end

module FixupSolver = Make_backward_cfg_solver(FixupProblem)

let update id res live ~solution =
  (match Inst_id.Map.find id solution with
  | { sol_in; _ } ->
    let result_locations =
      res
      |> Array.to_seq
      |> Seq.filter_map Phys_loc.of_reg
      |> Phys_loc.Set.of_seq
    in
    let live_across =
      Phys_loc.Map.filter
        (fun loc _ -> not (Phys_loc.Set.mem loc result_locations))
        sol_in
    in
    let live_locs =
      Reg.Set.fold
        (fun reg acc ->
          match Phys_loc.of_reg reg with
          | Some loc -> Phys_loc.Set.add loc acc
          | None -> acc)
        live
        Phys_loc.Set.empty
    in
    let live =
      Reg.Set.filter
        (fun reg ->
          match Phys_loc.of_reg reg with
          | Some loc -> Phys_loc.Map.mem loc live_across
          | None -> true)
        live
    in
    Phys_loc.Map.fold
      (fun loc reg acc ->
        if Phys_loc.Set.mem loc live_locs then acc
        else Reg.Set.add reg acc)
      live_across
      live
  | exception Not_found -> live)

let fix cfg =
  let solution = FixupSolver.solve cfg in
  Cfg.iter_blocks cfg ~f:(fun block bb ->
    let open Cfg in
    let body, _ =
      List.fold_left
        (fun (acc, n) b ->
          let b' =
            { b with live = update (Inst_id.Inst(block, n)) b.res b.live ~solution }
          in
          b' :: acc, n + 1)
      ([], 0)
      bb.body
    in
    bb.body <- List.rev body;
    let t = bb.terminator in
    let t' = { t with live = update (Inst_id.Term block) t.res t.live ~solution } in
    bb.terminator <- t');
