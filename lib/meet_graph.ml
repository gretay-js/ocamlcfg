

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
    ; spill_before: bool
    ; spill_after: bool
    ; reload_before: bool
    ; reload_after: bool
    ; predecessors: Edge.Set.t
    ; successors: Edge.Set.t
    }
end


type t =
  { graph: Node.t Label.Map.t
  ; spill_inst_id: Inst_id.t
  ; reload_inst_id: Inst_id.t
  }

module Meet_point = struct
  type loc = Before | After | Anywhere

  type t =
    | Node of Inst_id.t * loc
    | Edge of Label.t * Label.t

  let equal a b =
    match a, b with
    | Node(ai, al), Node(bi, bl) -> Inst_id.equal ai bi && al = bl
    | Node _, Edge _ -> false
    | Edge(ast, aen), Edge(bst, ben) -> Label.equal ast bst && Label.equal aen ben
    | Edge _, Node _ -> false

  let print fmt = function
    | Node(inst, loc) ->
      Format.fprintf fmt "Node(%s %a)"
        (match loc with
        | Before -> "before"
        | After -> "after"
        | Anywhere -> "anywhere")
        Inst_id.print
        inst
    | Edge(st, en) ->
      Format.fprintf fmt "Edge(%d, %d)" st en
end

type block_meets =
  { spill_before: bool
  ; spill_after: bool
  ; reload_before: bool
  ; reload_after: bool
  ; meet: Node.meet
  ; bb: Cfg.basic_block
  }

let of_cfg cfg ~spill_inst_id ~spills ~reload_inst_id ~reloads =
  let spill_block = Inst_id.parent spill_inst_id in
  let reload_block = Inst_id.parent reload_inst_id in
  (* Identify the set of nodes reachable from the spill_inst_id. *)
  let reach_from_spill =
    let rec dfs block acc =
      if Label.Set.mem block acc then acc
      else
        let acc = Label.Set.add block acc in
        let bb = Cfg.get_block_exn cfg block in
        let succs = Cfg.successor_labels cfg ~normal:true ~exn:true bb in
        Label.Set.fold dfs succs acc
    in
    dfs spill_block Label.Set.empty
  in
  (* Identify the set of nodes reachable from the reload_inst_id on the reverse CFG. *)
  let reach_from_reload =
    let rec dfs acc block =
      if Label.Set.mem block acc then acc
      else
        let acc = Label.Set.add block acc in
        let bb = Cfg.get_block_exn cfg block in
        List.fold_left dfs acc (Cfg.predecessor_labels bb)
    in
    dfs Label.Set.empty reload_block
  in
  (* The set of interesting nodes on paths between the spill_inst_id and the reload_inst_id. *)
  let blocks_between = Label.Set.inter reach_from_reload reach_from_spill in
  (* Helper to compute and compose meet information for an instruction. *)
  let update_meet node meet =
    match meet with
    | Node.Nowhere ->
      (try
        let open Data_flow_analysis in
        let { sol_in = reloads_after; sol_out = reloads_before } =
          Inst_id.Map.find node reloads
        in
        let { sol_in = spills_before; sol_out = spills_after } =
          Inst_id.Map.find node spills
        in
        let spill_before = Inst_id.Set.mem spill_inst_id spills_before in
        let spill_after = Inst_id.Set.mem spill_inst_id spills_after in
        let reload_before = Inst_id.Set.mem reload_inst_id reloads_before in
        let reload_after = Inst_id.Set.mem reload_inst_id reloads_after in
        let before = spill_before && reload_before in
        let after = spill_after && reload_after in
        if before && after then Node.Anywhere node
        else if before then Node.Before node
        else if after then Node.After node
        else Node.Nowhere
      with Not_found -> Node.Nowhere)
    | Node.Before _ | Node.After _ | Node.Anywhere _ ->
      meet
  in
  (* Aggregate meet information for each node in the CFG. *)
  let block_meets =
    Label.Set.fold
      (fun block acc ->
        let bb = Cfg.get_block_exn cfg block in
        let end_id = Inst_id.Term block in
        let info =
          try
            let start_id =
              match bb.Cfg.body with
              | [] -> end_id
              | _ -> Inst_id.Inst(block, 0)
            in
            let open Data_flow_analysis in
            let spill_before =
              let { sol_in; _ } = Inst_id.Map.find start_id spills in
              Inst_id.Set.mem spill_inst_id sol_in
            in
            let spill_after =
              let { sol_out; _ } = Inst_id.Map.find end_id spills in
              Inst_id.Set.mem spill_inst_id sol_out
            in
            let reload_before =
              let { sol_out; _ } = Inst_id.Map.find start_id reloads in
              Inst_id.Set.mem reload_inst_id sol_out
            in
            let reload_after =
              let { sol_in; _ } = Inst_id.Map.find end_id reloads in
              Inst_id.Set.mem reload_inst_id sol_in
            in
            let meet, _ =
              List.fold_left
                (fun (meet, n) _ -> update_meet (Inst_id.Inst(block, n)) meet, n + 1)
                (Node.Nowhere, 0)
                bb.Cfg.body
            in
            let meet = update_meet (Inst_id.Term block) meet in
            { spill_before; spill_after; reload_before; reload_after; meet; bb }
          with Not_found ->
            { spill_before = false;
              spill_after = false;
              reload_before = false;
              reload_after = false;
              meet = Node.Nowhere;
              bb }
        in
        Label.Map.add block info acc)
      blocks_between
      Label.Map.empty
  in
  (* Build the graph. *)
  let graph =
    Label.Map.mapi
      (fun block info ->
        let successors =
          if Label.equal block (Inst_id.parent reload_inst_id) then Edge.Set.empty
          else
            Cfg.successor_labels cfg ~normal:true ~exn:true info.bb
              |> Label.Set.elements
              |> List.filter_map (fun target ->
                match Label.Map.find target block_meets with
                | succ_info ->
                  Some { Edge.target; meet = info.spill_after && succ_info.reload_before }
                | exception Not_found -> None)
              |> Edge.Set.of_list
        in
        let predecessors =
          if Label.equal block (Inst_id.parent spill_inst_id) then Edge.Set.empty
          else
            Cfg.predecessor_labels info.bb
              |> List.filter_map (fun target ->
                match Label.Map.find target block_meets with
                | pred_info ->
                  Some { Edge.target; meet = info.reload_before && pred_info.spill_after }
                | exception Not_found -> None)
              |> Edge.Set.of_list
        in
        { Node.block;
          meet = info.meet;
          spill_before = info.spill_before;
          spill_after = info.spill_after;
          reload_after = info.reload_after;
          reload_before = info.reload_before;
          predecessors; successors })
      block_meets
  in
  { graph; spill_inst_id; reload_inst_id }

let meet_points { graph; _ } =
  graph
    |> Label.Map.bindings
    |> List.map (fun (block, { Node.meet; predecessors; successors; _ }) ->
        let open Meet_point in
        let node_meet =
          match meet with
          | Node.Before id -> [Node(id, Before)]
          | Node.After id -> [Node(id, After)]
          | Node.Anywhere id -> [Node(id, Anywhere)]
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
  Label.Map.bindings t.graph
    |> List.filter (fun (block, node) ->
        let { Node.spill_before; spill_after; reload_before; reload_after; meet; _ } =
          node
        in
        match meet with
        | Node.Before _ | Node.After _ | Node.Anywhere _ -> false
        | Node.Nowhere when Label.equal block (Inst_id.parent t.spill_inst_id) ->
          not spill_after
        | Node.Nowhere when Label.equal block (Inst_id.parent t.reload_inst_id) ->
          not reload_before
        | Node.Nowhere ->
          if not spill_before && not reload_after then true
          else if spill_before && not spill_after then true
          else if reload_after && not reload_before then true
          else false)
    |> List.map fst
    |> Label.Set.of_list

let sccs { graph; spill_inst_id; _ } =
  let module Scc_problem =
    struct
      type t = { graph: Node.t Label.Map.t; entry: Label.t }

      let entries { entry; _ } = Label.Set.singleton entry

      let next { graph; _ } node =
        let info = Label.Map.find node graph in
        info.successors
          |> Edge.Set.elements
          |> List.map (fun { Edge.target; _ } -> target)
          |> Label.Set.of_list

      module Node = Label
    end
  in
  let module Scc_solver = Scc.Make_solver(Scc_problem) in
  Scc_solver.solve { Scc_problem.graph; entry = Inst_id.parent spill_inst_id }

let spill_reaches { graph; _ } spill_inst_id =
  match Label.Map.find spill_inst_id graph with
  | { Node.spill_before; _ } -> spill_before
  | exception Not_found -> false

let reload_reaches { graph; _ } reload_inst_id =
  match Label.Map.find reload_inst_id graph with
  | { Node.reload_after; _ } -> reload_after
  | exception Not_found -> false

let has_node { graph; _ } node =
  Label.Map.mem node graph

let print fmt { graph; spill_inst_id; reload_inst_id } =
  Format.fprintf fmt "Meet_graph between %a -> %a:\n"
      Inst_id.print spill_inst_id
      Inst_id.print reload_inst_id;
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
  Format.pp_print_flush fmt ()
