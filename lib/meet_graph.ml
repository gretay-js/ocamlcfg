

module Edge = struct
  module T = struct
    type t = { target: Label.t; meet: bool }

    let compare a b = Label.compare a.target b.target
  end

  include T
  module Set = Set.Make(T)
end


module Node = struct
  type meet =
    | Before of Inst_id.t
    | After of Inst_id.t
    | Anywhere of Inst_id.t
    | Nowhere

  type t =
    { block: Label.t
    ; meet: meet
    ; predecessors: Edge.Set.t
    ; successors: Edge.Set.t
    }
end


type t =
  { graph: Node.t Label.Map.t
  ; spill: Inst_id.t
  ; reload: Inst_id.t
  }

type meet_point =
  | Before of Inst_id.t
  | After of Inst_id.t
  | Anywhere of Inst_id.t
  | Edge of Label.t * Label.t

type block_meets =
  { spill_before: bool
  ; spill_after: bool
  ; reload_before: bool
  ; reload_after: bool
  ; meet: Node.meet
  ; bb: Cfg.basic_block
  }

let of_cfg cfg ~spill ~spills ~reload ~reloads =
  let spill_block = Inst_id.parent spill in
  let reload_block = Inst_id.parent reload in
  (* Identify the set of nodes reachable from the spill. *)
  let reach_from_spill =
    let rec dfs block acc =
      if Label.Set.mem block acc then acc
      else
        let acc = Label.Set.add block acc in
        if Label.equal block reload_block then acc
        else
          let bb = Cfg.get_block_exn cfg block in
          let succs = Cfg.successor_labels cfg ~normal:true ~exn:true bb in
          Label.Set.fold dfs succs acc
    in
    dfs spill_block Label.Set.empty
  in
  (* Identify the set of nodes reachable from the reload on the reverse CFG. *)
  let reach_from_reload =
    let rec dfs acc block =
      if Label.Set.mem block acc then acc
      else
        let acc = Label.Set.add block acc in
        if Label.equal block spill_block then acc
        else
          let bb = Cfg.get_block_exn cfg block in
          List.fold_left dfs acc (Cfg.predecessor_labels bb)
    in
    dfs Label.Set.empty reload_block
  in
  (* The set of interesting nodes on paths between the spill and the reload. *)
  let blocks_between = Label.Set.inter reach_from_reload reach_from_spill in
  (* Helper to compute and compose meet information for an instruction. *)
  let update_meet node meet =
    match meet with
    | Node.Nowhere ->
      let open Data_flow_analysis in
      let { sol_in = reloads_after; sol_out = reloads_before } =
        Inst_id.Map.find node reloads
      in
      let { sol_in = spills_before; sol_out = spills_after } =
        Inst_id.Map.find node spills
      in
      let spill_before = Inst_id.Set.mem spill spills_before in
      let spill_after = Inst_id.Set.mem spill spills_after in
      let reload_before = Inst_id.Set.mem reload reloads_before in
      let reload_after = Inst_id.Set.mem reload reloads_after in
      let before = spill_before && reload_before in
      let after = spill_after && reload_after in
      if before && after then Node.Anywhere node
      else if before then Node.Before node
      else if after then Node.After node
      else Node.Nowhere
    | Node.Before _ | Node.After _ | Node.Anywhere _ ->
      meet
  in
  (* Aggregate meet information for each node in the CFG. *)
  let block_meets =
    Label.Set.fold
      (fun block acc ->
        let bb = Cfg.get_block_exn cfg block in
        let end_id = Inst_id.Term block in
        let start_id =
          match bb.Cfg.body with
          | [] -> end_id
          | _ -> Inst_id.Inst(block, 0)
        in
        let open Data_flow_analysis in
        let spill_before =
          let { sol_in; _ } = Inst_id.Map.find start_id spills in
          Inst_id.Set.mem spill sol_in
        in
        let spill_after =
          let { sol_out; _ } = Inst_id.Map.find end_id spills in
          Inst_id.Set.mem spill sol_out
        in
        let reload_before =
          let { sol_out; _ } = Inst_id.Map.find start_id reloads in
          Inst_id.Set.mem reload sol_out
        in
        let reload_after =
          let { sol_in; _ } = Inst_id.Map.find end_id reloads in
          Inst_id.Set.mem reload sol_in
        in
        let meet, _ =
          List.fold_left
            (fun (meet, n) _ -> update_meet (Inst_id.Inst(block, n)) meet, n + 1)
            (Node.Nowhere, 0)
            bb.Cfg.body
        in
        let meet = update_meet (Inst_id.Term block) meet in
        let info = { spill_before; spill_after; reload_before; reload_after; meet; bb } in
        Label.Map.add block info acc)
      blocks_between
      Label.Map.empty
  in
  (* Build the graph. *)
  let graph =
    Label.Map.mapi
      (fun block info ->
        let successors =
          Cfg.successor_labels cfg ~normal:true ~exn:true info.bb
            |> Label.Set.elements
            |> List.filter_map (fun target ->
              match Label.Map.find target block_meets with
              | pred_info ->
                Some { Edge.target; meet = info.reload_before && pred_info.spill_after }
              | exception Not_found -> None)
            |> Edge.Set.of_list
        in
        let predecessors =
          Cfg.predecessor_labels info.bb
            |> List.filter_map (fun target ->
              match Label.Map.find target block_meets with
              | succ_info ->
                Some { Edge.target; meet = info.spill_after && succ_info.reload_before }
              | exception Not_found -> None)
            |> Edge.Set.of_list
        in
        { Node.block; meet = info.meet; predecessors; successors })
      block_meets
  in
  { graph; spill; reload }

let meet_points { graph; _ } =
  graph
    |> Label.Map.bindings
    |> List.map (fun (block, { Node.meet; predecessors; successors; _ }) ->
        let node_meet =
          match meet with
          | Node.Before id -> [Before id]
          | Node.After id -> [After id]
          | Node.Anywhere id -> [Anywhere id]
          | Node.Nowhere -> []
        in
        let pred_meet =
          predecessors
            |> Edge.Set.elements
            |> List.filter_map (fun { Edge.target; meet } ->
              if meet then Some (Edge(target, block)) else None)
        in
        let succ_meet =
          successors
            |> Edge.Set.elements
            |> List.filter_map (fun { Edge.target; meet } ->
              if meet then Some (Edge(block, target)) else None)
        in
        List.append node_meet (List.append pred_meet succ_meet))
    |> List.flatten

let split_points t =
  let starts =
    List.map (function
      | Before id | After id | Anywhere id ->
        let block = Inst_id.parent id in (block, block)
      | Edge(st, en) ->
        (st, en)) (meet_points t)
  in
  let starts_up = List.map fst starts in
  let starts_down = List.map snd starts in
  let rec dfs dir acc node =
    let acc = Label.Set.add node acc in
    let { Node.predecessors; successors; _ } = Label.Map.find node t.graph in
    let edges =
      match dir with
      | `Up -> predecessors
      | `Down -> successors
    in
    Edge.Set.fold (fun { Edge.target; _ } acc ->
      if Label.Set.mem target acc then acc else dfs dir acc target) edges acc
  in
  let reach_up = List.fold_left (dfs `Up) Label.Set.empty starts_up in
  let reach_up_down = List.fold_left (dfs `Down) reach_up starts_down in
  let all_nodes =
    Label.Map.bindings t.graph |> List.map fst |> Label.Set.of_list
  in
  Label.Set.diff all_nodes reach_up_down

let print fmt { graph; spill; reload } =
  Format.fprintf fmt "Meet_graph between %a -> %a:\n"
      Inst_id.print spill
      Inst_id.print reload;
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
    (fun fmt (block, { Node.meet; predecessors; successors; _ }) ->
      Format.fprintf fmt "block %d" block;
      (match meet with
      | Node.Before id -> Format.fprintf fmt " before %a:\n" Inst_id.print id
      | Node.After id -> Format.fprintf fmt " after %a:\n" Inst_id.print id
      | Node.Anywhere id -> Format.fprintf fmt " anywhere %a:\n" Inst_id.print id
      | Node.Nowhere -> Format.fprintf fmt ":\n");
      let print_edge { Edge.target; meet } =
        Format.fprintf fmt " %d%s" target (if meet then ":meet" else "")
      in
      Format.fprintf fmt "\tpreds:";
      Edge.Set.iter print_edge predecessors;
      Format.fprintf fmt "\n\tsuccs:";
      Edge.Set.iter print_edge successors;
      Format.fprintf fmt "\n")
    fmt
    (Label.Map.bindings graph);
  Format.pp_print_flush fmt ();
