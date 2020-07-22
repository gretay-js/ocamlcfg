include Data_flow_analysis_intf.S

module type Problem = sig
  type t
  type v
  type node = Label.t

  val init : Cfg.t -> t
  val entries : t -> node list
  val next : t -> node -> node list
  val prev : t -> node -> node list
  val f : t -> node -> v -> v
  val top : t -> node -> v
  val compare : v -> v -> int
  val lub : v -> v -> v
end

module Make_solver (P: Problem) = struct
  let solve_bb cfg =
    let t = P.init cfg in
    let q = List.fold_left Queue.push Queue.empty (P.entries t) in
    let solution = Hashtbl.create 10 in
    let rec fixpoint q =
      match Queue.pop q with
      | None -> solution
      | Some (n, q) ->
        let nodes_prev = P.prev t n in
        let sols_prev = List.map (fun node ->
          try Hashtbl.find solution node
          with Not_found -> P.top t node) nodes_prev
        in
        let sol_prev =
          match sols_prev with
          | [] -> P.top t n
          | s :: ss -> List.fold_left P.lub s ss
        in
        let sol_node = P.f t n sol_prev in
        match Hashtbl.find solution n with
        | sol_old when P.compare sol_old sol_node = 0 ->
          fixpoint q
        | _
        | exception Not_found ->
          Hashtbl.remove solution n;
          Hashtbl.add solution n sol_node;
          fixpoint (List.fold_left Queue.push q (P.next t n))
    in
    fixpoint q
end

let cfg_fwd_next cfg node =
  let block = Cfg.get_block_exn cfg node in
  let next = Cfg.successor_labels cfg ~normal:true ~exn:true block in
  Label.Set.elements next

let cfg_fwd_prev cfg node =
  let block = Cfg.get_block_exn cfg node in
  Cfg.predecessor_labels block

module IntSet = Set.Make(struct
  type t = int
  let compare = Stdlib.compare
end)

let get_spill_slot inst =
  let open Cfg in
  match inst.desc with
  | Op Spill ->
    (match inst.res with
    | [| { Reg.loc = Reg.Stack (Reg.Local n); _ } |] -> Some n
    | _ -> failwith "invalid spill")
  | _ ->
    None

module ReachingSpillsProblem = struct
  type kg =
    { gen: Spill.Set.t
    ; kill: IntSet.t
    }
  type t =
    { cfg: Cfg.t
    ; kill_gen: (Label.t, kg) Hashtbl.t
    }
  type v = Spill.Set.t
  type node = Label.t

  let init cfg =
    let kill_gen = Hashtbl.create 10 in
    Cfg.iter_blocks cfg ~f:(fun block bb ->
      let kg = List.fold_left
        (fun kg inst ->
          match get_spill_slot inst with
          | None -> kg
          | Some stack_slot ->
            let spill = { Spill.block; inst = inst.id; stack_slot } in
            let reachable_spills =
              Spill.Set.filter (fun spill -> spill.stack_slot <> stack_slot) kg.gen
            in
            { kill = IntSet.add stack_slot kg.kill
            ; gen = Spill.Set.add spill reachable_spills
            })
        { gen =  Spill.Set.empty; kill = IntSet.empty }
        bb.body
      in
      Hashtbl.add kill_gen block kg
    );
    { cfg; kill_gen }

  let entries { cfg; _ } = [Cfg.entry_label cfg]
  let next { cfg; _ } node = cfg_fwd_next cfg node
  let prev { cfg; _ } node = cfg_fwd_prev cfg node

  let f { kill_gen; _ } node v =
    let { gen; kill } = Hashtbl.find kill_gen node in
    let not_killed = Spill.Set.filter
      (fun { stack_slot; _ } -> not (IntSet.mem stack_slot kill))
      v
    in
    Spill.Set.union gen not_killed

  let lub = Spill.Set.union
  let top _cfg _node = Spill.Set.empty
  let compare = Spill.Set.compare
end

module ReachingSpills = struct
  include Make_solver(ReachingSpillsProblem)

  let solve cfg =
    let inst_sol = Hashtbl.create 10 in
    let bb_sol = solve_bb cfg in
    (* After solving at the block level, compute reachable sets for
       individual instructions in each block *)
    Cfg.iter_blocks cfg ~f:(fun block bb ->
      match List.map (Hashtbl.find bb_sol) (cfg_fwd_prev cfg block) with
      | sols_in ->
        let sol_in = List.fold_left Spill.Set.union (Spill.Set.empty) sols_in in
        ignore (List.fold_left
          (fun spills_in inst ->
            let spills_out =
              match get_spill_slot inst with
              | None -> spills_in
              | Some stack_slot ->
                let spill = { Spill.block; inst = inst.id; stack_slot } in
                let reachable_spills =
                  Spill.Set.filter
                    (fun spills -> spills.stack_slot <> stack_slot)
                    spills_in
                in
                Spill.Set.add spill reachable_spills
            in
            Hashtbl.add inst_sol (block, inst.id) spills_out;
            spills_out)
          sol_in bb.body)
      | exception Not_found ->
        ());
    inst_sol
end

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
  let top _cfg node = Dom.Set.singleton node
  let compare = Dom.Set.compare
end

module Dominators = struct
  include Make_solver(DominatorsProblem)
  let solve = solve_bb
end
