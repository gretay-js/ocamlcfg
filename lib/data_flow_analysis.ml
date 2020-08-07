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
    let entries = P.entries t in
    let q = P.Node.Set.fold (fun n q -> Q.push q n) entries Q.empty in
    let rec fixpoint solution q =
      match Q.pop q with
      | None -> solution
      | Some (n, q) ->
        let outs_prev =
          List.map
            (fun prev ->
              match Map.find prev solution with
              | (_, sol_out) -> sol_out
              | exception Not_found -> P.empty t prev)
            (P.prev t n)
        in
        let sol_in =
          match outs_prev with
          | [] -> P.entry t n
          | ss when P.Node.Set.mem n entries ->
            List.fold_left P.S.lub (P.entry t n) ss
          | s :: ss ->
            List.fold_left P.S.lub s ss
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
    let empty { pt; _ } = P.empty pt
    let entry { pt; _ } = P.entry pt

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
      P.Parent.Set.fold (fun n kg -> dfs kg n) (P.entries pt) ParentMap.empty
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

    let entries t = Label.Set.singleton (Cfg.entry_label (P.cfg t))

    let next t = cfg_next (P.cfg t)
    let prev t = cfg_prev (P.cfg t)

    let start_node t block =
      let bb = Cfg.get_block_exn (P.cfg t) block in
      match bb.body with
      | [] -> Inst_id.Term block
      | _ -> Inst_id.Inst (block, 0)

    let next_node t = function
      | Inst_id.Term _ -> None
      | Inst_id.Inst(block, n) ->
        let bb = Cfg.get_block_exn (P.cfg t) block in
        if n + 1 = List.length bb.body then Some (Inst_id.Term block)
        else Some (Inst_id.Inst (block, n + 1))

    let entry = P.entry
    let empty = P.empty
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

    let entries t = P.cfg t |> Cfg.exit_labels

    let next t = cfg_prev (P.cfg t)
    let prev t = cfg_next (P.cfg t)

    let start_node _ block =
      Inst_id.Term block

    let next_node t = function
      | Inst_id.Term block ->
        let bb = Cfg.get_block_exn (P.cfg t) block in
        (match bb.body with
        | [] -> None
        | insts -> Some (Inst_id.Inst (block, List.length insts - 1)))
      | Inst_id.Inst (_, 0) ->
        None
      | Inst_id.Inst (block, n) ->
        Some (Inst_id.Inst (block, n - 1))

    let entry = P.entry
    let empty = P.empty
    let kg = P.kg
  end

  let solve = let module M = Make_kill_gen_solver(T) in M.solve
end
