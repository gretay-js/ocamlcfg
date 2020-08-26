include Data_flow_analysis_intf

type 's solution = { sol_in: 's; sol_out: 's }

module type Solver = Solver with type 'a solution := 'a solution

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
    let entry_nodes = P.entry_nodes t in
    let q = P.Node.Set.fold (fun n q -> Q.push q n) entry_nodes Q.empty in
    let rec fixpoint solution q =
      match Q.pop q with
      | None -> solution
      | Some (n, q) ->
        let outs_prev =
          List.map
            (fun prev ->
              match Map.find_opt prev solution with
              | Some { sol_out; _ } -> sol_out
              | None -> P.S.bot)
            (P.prev t n)
        in
        let sol_in =
          match outs_prev with
          | [] -> P.entry t n
          | ss when P.Node.Set.mem n entry_nodes ->
            List.fold_left P.S.lub (P.entry t n) ss
          | s :: ss ->
            List.fold_left P.S.lub s ss
        in
        match Map.find_opt n solution with
        | Some { sol_in = sol_in'; _ } when P.S.equal sol_in' sol_in ->
          (* XCR lwhite: Not immediately clear to me why we need
             to check that the input did not change.

             nlicker: transfer does not need to be injective, since it's not strictly
             monotone. I was actually mistaken initially since what we need to check
             for is whether the input changed or not.
           *)
          fixpoint solution q
        | _
        | exception Not_found ->
          let sol_out = P.transfer t n sol_in in
          let solution' = Map.add n { sol_in; sol_out } solution in
          fixpoint solution' (List.fold_left Q.push q (P.next t n))
    in
    fixpoint Map.empty q
end

module Make_kill_gen_solver (P: Semigroup_action_problem) = struct
  module GroupMap = P.Group.Map

  module T = struct
    module S = P.A.S
    module Node = P.Group

    type t =
      { pt: P.t
      ; kill_gens: P.A.G.t GroupMap.t
      }

    let entry_nodes { pt; _ } = P.entry_groups pt

    (* XCR lwhite: This is a somewhat subjective matter of taste, but
       I think that these definitions would be clearer (and slightly
       more efficient) if they were eta-expanded. *)
    let next { pt; _ } id = P.next_group pt id

    let prev { pt; _ } id = P.prev_group pt id

    let entry { pt; _ } = P.entry pt

    let transfer { kill_gens; _ } n sol =
      P.A.apply sol (GroupMap.find n kill_gens)
  end

  module Group_solver = Make_solver(T)

  let solve pt =
    let kill_gens =
      (* Compute per-block kill-gen info by composing child nodes. *)
      let rec advance node action =
        match P.next_node pt node with
        | None -> action
        | Some node' ->
          let action' = P.action pt node' in
          advance node' (P.A.G.dot action' action)
      in
      let rec dfs kill_gens parent =
        if GroupMap.mem parent kill_gens then kill_gens
        else begin
          let start = P.start_node pt parent in
          let action = advance start (P.action pt start) in
          let kill_gens' = GroupMap.add parent action kill_gens in
          List.fold_left dfs kill_gens' (P.next_group pt parent)
        end
      in
      P.Group.Set.fold (fun n action -> dfs action n) (P.entry_groups pt) GroupMap.empty
    in
    let parent_solution =
      Group_solver.solve { T.pt; T.kill_gens }
    in
    T.Node.Map.fold
      (fun parent _ solution ->
        let { sol_in; _ } = T.Node.Map.find parent parent_solution in
        let rec advance node solution sol_in =
          let action = P.action pt node in
          let sol_out = P.A.apply sol_in action in
          let solution' = P.Node.Map.add node { sol_in; sol_out } solution in
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

module Make_forward_cfg_solver (P: Cfg_semigroup_action_problem) = struct
  module T = struct
    module S = P.A.S
    module A = P.A

    module Group = Label
    module Node = Inst_id

    type t = P.t

    let entry_groups t = Label.Set.singleton (Cfg.entry_label (P.cfg t))

    let next_group t id = cfg_next (P.cfg t) id
    let prev_group t id = cfg_prev (P.cfg t) id

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

    let action = P.action
  end

  (* XCR lwhite: This is fine, but if you want you can just do
     [include Make_kill_gen_solver(T)]

     nlicker: include does not work since there T is exposed multiple times.
     Also, I prefer not to expose more names than what is needed. *)

  let solve = let module M = Make_kill_gen_solver(T) in M.solve
end

module Make_backward_cfg_problem (P: Cfg_semigroup_action_problem) = struct
  module S = P.A.S
  module A = P.A

  module Group = Label
  module Node = Inst_id

  type t = P.t

  let next_group t id = cfg_prev (P.cfg t) id
  let prev_group t id = cfg_next (P.cfg t) id

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
  let action = P.action
end

module Make_backward_cfg_solver (P: Cfg_semigroup_action_problem) = struct
  module T = struct
    include Make_backward_cfg_problem(P)

    let entry_groups t = P.cfg t |> Cfg.exit_labels
  end

  let solve = let module M = Make_kill_gen_solver(T) in M.solve
end

module Make_backward_noexn_cfg_solver (P: Cfg_semigroup_action_problem) = struct
  module T = struct
    include Make_backward_cfg_problem(P)

    let entry_groups t = P.cfg t |> Cfg.exit_labels_noexn
  end

  let solve = let module M = Make_kill_gen_solver(T) in M.solve
end
