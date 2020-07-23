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

module Make_solver (P: Problem) : sig
  val solve : P.t -> (P.S.t * P.S.t) P.Node.Map.t
end = struct
  module Q = Make_queue(P.Node)

  let solve t =
    let module Map = P.Node.Map in
    let q = List.fold_left Q.push Q.empty (P.entries t) in
    let rec fixpoint solution q =
      match Q.pop q with
      | None -> solution
      | Some (n, q) ->
        let nodes_prev = P.prev t n in
        let sols_prev = List.map (fun node ->
          try snd (Map.find node solution)
          with Not_found -> P.init t node) nodes_prev
        in
        let sol_in =
          match sols_prev with
          | [] -> P.S.top
          | s :: ss -> List.fold_left P.S.lub s ss
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

module Make_kill_gen_solver (P: KillGenProblem) : sig
  val solve : P.t -> (P.K.S.t P.Node.Map.t) P.Parent.Map.t
end = struct
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
      let rec advance parent node kg =
        match P.next_node pt parent node with
        | None -> kg
        | Some node' ->
          let kg' = P.kg pt parent node' in
          advance parent node' (P.K.dot kg' kg)
      in
      let rec dfs kill_gens parent =
        if ParentMap.mem parent kill_gens then kill_gens
        else
          let start = P.start_node pt parent in
          let kg = advance parent start (P.kg pt parent start) in
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
        let rec advance parent node solution sol =
          let solution' = P.Node.Map.add node sol solution in
          match P.next_node pt parent node with
          | None -> solution'
          | Some node' ->
            let kg = P.kg pt parent node in
            let sol' = P.K.f sol kg in
            advance parent node' solution' sol'
        in
        let block_sol =
          advance parent (P.start_node pt parent) P.Node.Map.empty sol_in
        in
        ParentMap.add parent block_sol solution)
      parent_solution ParentMap.empty
end

let cfg_next cfg node =
  let block = Cfg.get_block_exn cfg node in
  let next = Cfg.successor_labels cfg ~normal:true ~exn:true block in
  Label.Set.elements next

let cfg_prev cfg node =
  let block = Cfg.get_block_exn cfg node in
  Cfg.predecessor_labels block

module Make_forward_cfg_solver (P: CfgKillGenProblem) : sig
  val solve : P.t -> (P.K.S.t Inst_id.Map.t) Label.Map.t
end = struct
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
      | [] -> Node.Term
      | _ -> Node.Inst 0

    let next_node t block = function
      | Node.Term -> None
      | Node.Inst n ->
        let bb = Cfg.get_block_exn (P.cfg t) block in
        if n + 1 = List.length bb.body then Some Node.Term
        else Some (Node.Inst (n + 1))

    let init = P.init
    let kg = P.kg
  end

  include Make_kill_gen_solver(T)
end

module Make_backward_cfg_solver (P: CfgKillGenProblem) : sig
  val solve : P.t -> (P.K.S.t Inst_id.Map.t) Label.Map.t
end = struct
  module T = struct
    module S = P.K.S
    module K = P.K

    module Parent = Label
    module Node = Inst_id

    type t = P.t

    let entries t =
      let open Cfg in
      blocks (P.cfg t)
      |> List.filter (fun block ->
        match block.terminator.desc with
        | Never
        | Return
        | Tailcall _ -> true
        | Always _
        | Parity_test _
        | Truth_test _
        | Float_test _
        | Int_test _
        | Switch _
        | _ -> false)
      |> List.map (fun block -> block.start)

    let next t = cfg_prev (P.cfg t)
    let prev t = cfg_next (P.cfg t)

    let start_node _ _ = Node.Term

    let next_node t block = function
      | Node.Term ->
        let bb = Cfg.get_block_exn (P.cfg t) block in
        (match bb.body with
        | [] -> None
        | insts -> Some (Node.Inst (List.length insts - 1)))
      | Node.Inst 0 ->
        None
      | Node.Inst n ->
        Some (Node.Inst (n - 1))

    let init = P.init
    let kg = P.kg
  end

  include Make_kill_gen_solver(T)
end

module DominatorsProblem = struct
  module S = struct
    include Dom.Set
    let lub = Dom.Set.inter
    let top = Dom.Set.empty
  end

  module Node = Label

  type t = Cfg.t

  let entries cfg = [Cfg.entry_label cfg]

  let next = cfg_next
  let prev = cfg_prev

  let init cfg node =
    if node = Cfg.entry_label cfg then Dom.Set.singleton node
    else Dom.Set.of_list (List.map (fun bb -> bb.Cfg.start) (Cfg.blocks cfg))

  let f _cfg node v = Dom.Set.add node v
end

module Dominators = struct
  let solve cfg =
    let module DominatorsSolver = Make_solver(DominatorsProblem) in
    Label.Map.map snd (DominatorsSolver.solve cfg)
end
