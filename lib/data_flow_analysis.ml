include Data_flow_analysis_intf.S

module Make_queue (T: Node_id) : sig
  type t
  val empty : t
  val push : t -> T.t -> t
  val pop : t -> (T.t * t) option
end = struct
  type t = T.t list * T.t list * T.Set.t

  let empty = ([], [], T.Set.empty)

  let push q x =
    let front, back, set = q in
    if T.Set.mem x set then q
    else (front, x :: back, T.Set.add x set)

  let pop = function
    | (x :: front, back, set) ->
      Some (x, (front, back, T.Set.remove x set))
    | ([], back, set) ->
      match List.rev back with
      | [] -> None
      | x :: back' ->
        Some (x, (back', [], T.Set.remove x set))
end

module Make_solver (P: Problem) = struct
  module Q = Make_queue(P.Node)

  let solve t =
    let module Map = P.Node.Map in
    let q = List.fold_left Q.push Q.empty (P.entries t) in
    let rec fixpoint solution q =
      match Q.pop q with
      | None -> solution
      | Some (n, q) ->
        let get_in_out solution n =
          match Map.find n solution with
          | (sol_in, sol_out) -> sol_in, sol_out, solution
          | exception Not_found ->
            let (sol_in, sol_out) = P.init t n in
            let solution = Map.add n (sol_in, sol_out) solution in
            sol_in, sol_out, solution
        in
        let outs_prev, solution =
          List.fold_left
            (fun (outs_prev, solution) node ->
              let _, sol_out, solution = get_in_out solution node in
              sol_out :: outs_prev, solution)
            ([], solution)
            (P.prev t n)
        in
        let sol_in, solution =
          match outs_prev with
          | [] ->
            let sol_in, _, solution = get_in_out solution n in
            sol_in, solution
          | s :: ss ->
            List.fold_left P.S.lub s ss, solution
        in
        let sol_out = P.f t n sol_in in
        match Map.find n solution with
        | (sol_in', sol_out')
          when P.S.equal sol_in' sol_in && P.S.equal sol_out' sol_out ->
          fixpoint solution q
        | _
        | exception Not_found ->
          let solution' = Map.add n (sol_in, sol_out) solution in
          fixpoint solution' (List.fold_left Q.push q (P.next t n))
    in
    fixpoint P.Node.Map.empty q
end

module Make_kill_gen_solver (P: KillGenProblem) = struct
  module ParentMap = P.Parent.Map

  module T = struct
    module S = P.K.S
    module Node = P.Parent

    type t =
      { pt: P.t
      ; kill_gens: P.K.t ParentMap.t
      }

    let entries { pt; _ } = P.entries pt
    let next { pt; _ } = P.next pt
    let prev { pt; _ } = P.prev pt
    let init { pt; _ } = P.init pt

    let f { kill_gens; _ } n sol =
      P.K.f sol (ParentMap.find n kill_gens)
  end

  module Parent_solver = Make_solver(T)

  let solve pt =
    let kill_gens =
      (* Compute per-block kill-gen info by composing child nodes. *)
      let rec advance node kg =
        match P.next_node pt node with
        | None -> kg
        | Some node' ->
          let kg' = P.kg pt node' in
          advance node' (P.K.dot kg' kg)
      in
      let rec dfs kill_gens parent =
        if ParentMap.mem parent kill_gens then kill_gens
        else
          let start = P.start_node pt parent in
          let kg = advance start (P.kg pt start) in
          let kill_gens' = ParentMap.add parent kg kill_gens in
          List.fold_left dfs kill_gens' (P.next pt parent)
      in
      List.fold_left dfs ParentMap.empty (P.entries pt)
    in
    let parent_solution =
      Parent_solver.solve { T.pt; T.kill_gens }
    in
    T.Node.Map.fold
      (fun parent _ solution ->
        let sol_in, _ = T.Node.Map.find parent parent_solution in
        let rec advance node solution sol_in =
          let kg = P.kg pt node in
          let sol_out = P.K.f sol_in kg in
          let solution' = P.Node.Map.add node (sol_in, sol_out) solution in
          match P.next_node pt node with
          | None -> solution'
          | Some node' ->
            advance node' solution' sol_out
        in
        advance (P.start_node pt parent) solution sol_in)
      parent_solution P.Node.Map.empty
end

let cfg_next cfg node =
  let block = Cfg.get_block_exn cfg node in
  let next = Cfg.successor_labels cfg ~normal:true ~exn:true block in
  Label.Set.elements next

let cfg_prev cfg node =
  let block = Cfg.get_block_exn cfg node in
  Cfg.predecessor_labels block

module Make_forward_cfg_solver (P: CfgKillGenProblem) = struct
  module T = struct
    module S = P.K.S
    module K = P.K

    module Parent = Label
    module Node = Inst_id

    type t = P.t

    let entries t = [Cfg.entry_label (P.cfg t)]

    let next t = cfg_next (P.cfg t)
    let prev t = cfg_prev (P.cfg t)

    let start_node t block =
      let bb = Cfg.get_block_exn (P.cfg t) block in
      match bb.body with
      | [] -> Node.Term block
      | _ -> Node.Inst (block, 0)

    let next_node t = function
      | Node.Term _ -> None
      | Node.Inst(block, n) ->
        let bb = Cfg.get_block_exn (P.cfg t) block in
        if n + 1 = List.length bb.body then Some (Node.Term block)
        else Some (Node.Inst (block, n + 1))

    let init = P.init
    let kg = P.kg
  end

  let solve = let module M = Make_kill_gen_solver(T) in M.solve
end

module Make_backward_cfg_solver (P: CfgKillGenProblem) = struct
  module T = struct
    module S = P.K.S
    module K = P.K

    module Parent = Label
    module Node = Inst_id

    type t = P.t

    let entries t = P.cfg t |> Cfg.exit_labels |> Label.Set.elements

    let next t = cfg_prev (P.cfg t)
    let prev t = cfg_next (P.cfg t)

    let start_node _ block = Node.Term block

    let next_node t = function
      | Node.Term block ->
        let bb = Cfg.get_block_exn (P.cfg t) block in
        (match bb.body with
        | [] -> None
        | insts -> Some (Node.Inst (block, List.length insts - 1)))
      | Node.Inst (_, 0) ->
        None
      | Node.Inst (block, n) ->
        Some (Node.Inst (block, n - 1))

    let init = P.init
    let kg = P.kg
  end

  let solve = let module M = Make_kill_gen_solver(T) in M.solve
end

let all_nodes cfg =
  Dom.Set.of_list (List.map (fun bb -> bb.Cfg.start) (Cfg.blocks cfg))

module DominatorsProblem = struct
  module S = struct
    include Dom.Set
    let lub = Dom.Set.inter
  end

  module Node = Label

  type t = Cfg.t

  let entries cfg = [Cfg.entry_label cfg]

  let next = cfg_next
  let prev = cfg_prev

  let init cfg node =
    if node = Cfg.entry_label cfg then
      (Dom.Set.singleton node, Dom.Set.empty)
    else
      (all_nodes cfg, Dom.Set.empty)

  let f _cfg node v = Dom.Set.add node v
end

module Dominators = struct
  let solve cfg =
    let module DominatorsSolver = Make_solver(DominatorsProblem) in
    Label.Map.map snd (DominatorsSolver.solve cfg)
end
