include Data_flow_analysis_intf.S

module type Problem = sig
  type t
  type v
  type node

  val init : Cfg.t -> t
  val entries : t -> node list
  val next : t -> node -> node list
  val prev : t -> node -> node list
  val f : t -> node -> v -> v
  val top : node -> v
  val lub : v -> v -> v
end

module Make_solver (P: Problem) = struct
  let solve cfg =
    let t = P.init cfg in
    let q = List.fold_left Queue.push Queue.empty (P.entries t) in
    let solution = Hashtbl.create 10 in
    let get_solution node =
      try Hashtbl.find solution node
      with Not_found -> P.top node
    in
    let rec fixpoint q =
      match Queue.pop q with
      | None -> solution
      | Some (n, q) ->
        let nodes_prev = P.prev t n in
        let sols_prev = List.map get_solution nodes_prev in
        let sol_prev =
          match sols_prev with
          | [] -> P.top n
          | s :: ss -> List.fold_left P.lub s ss
        in
        let sol_node = P.f t n sol_prev in
        if sol_node = get_solution n then fixpoint q
        else begin
          Hashtbl.remove solution n;
          Hashtbl.add solution n sol_node;
          fixpoint (List.fold_left Queue.push q (P.next t n))
        end
    in
    fixpoint q
end

let cfg_fwd_next cfg node =
  let block = Cfg.get_block_exn cfg node in
  let next = Cfg.successor_labels cfg ~normal:true ~exn:true block in
  Label.Set.fold List.cons next []

let cfg_fwd_prev cfg node =
  let block = Cfg.get_block_exn cfg node in
  Cfg.predecessor_labels block

module IntSet = Set.Make(struct
  type t = int
  let compare = Stdlib.compare
end)

module ReachingSpillsProblem = struct
  type t =
    { cfg: Cfg.t
    ; gens: (Label.t, Spill.Set.t) Hashtbl.t
    ; kills: (Label.t, IntSet.t) Hashtbl.t
    }
  type v = Spill.Set.t
  type node = Label.t

  let init cfg =
    let gens = Hashtbl.create 10 in
    let kills = Hashtbl.create 10 in
    Cfg.iter_blocks cfg ~f:(fun _node bb ->
      List.iter (fun inst ->
        let open Cfg in
        match inst.desc with
        | Op Spill -> failwith "X"
        | Op Reload -> failwith "Y"
        | _ -> ()) bb.body
    );
    { cfg; gens; kills }

  let entries { cfg; _ } = [Cfg.entry_label cfg]
  let next { cfg; _ } node = cfg_fwd_next cfg node
  let prev { cfg; _ } node = cfg_fwd_prev cfg node

  let f { gens; kills; _ } node v =
    let gen = Hashtbl.find gens node in
    let kill = Hashtbl.find kills node in
    let not_killed = Spill.Set.filter
      (fun { stack_location; _ } -> not (IntSet.mem stack_location kill))
      v
    in
    Spill.Set.union gen not_killed

  let lub = Spill.Set.union
  let top _node = Spill.Set.empty
end

module ReachingSpills = Make_solver(ReachingSpillsProblem)

module DominatorsProblem = struct
  type t = Cfg.t
  type v = Dom.Set.t
  type node = Label.t

  let init cfg = cfg
  let entries cfg = [Cfg.entry_label cfg]
  let next = cfg_fwd_next
  let prev = cfg_fwd_prev

  let f _cfg node v = Dom.Set.add node v

  let lub = Dom.Set.inter
  let top node = Dom.Set.singleton node
end

module Dominators = Make_solver(DominatorsProblem)
