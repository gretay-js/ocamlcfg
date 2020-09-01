open Data_flow_analysis

module CL = Cfg_with_layout



let inter_sets sets =
  match sets with
  | [] -> failwith "at least one set required"
  | s :: ss -> List.fold_left Label.Set.inter s ss

let find_dominators doms spill blocks =
  let common =
    blocks
      |> List.map (fun block -> Label.Map.find block doms)
      |> inter_sets
  in
  (* Return all dominators if there are any besides the entry. *)
  let spill_doms = Label.Map.find spill doms in
  let doms_after_spill = Label.Set.diff common spill_doms in
  if Label.Set.is_empty doms_after_spill then None else Some doms_after_spill

let build_post_dominators cfg reload =
  let entry = Inst_id.parent reload in
  let module Dom =
    struct
      include Full_set.Make(Label)
      let bot = full
      let lub = inter
    end
  in
  let module Dominator_problem =
    struct
      module S = Dom
      module Node = Label

      type t = { cfg: Cfg.t; entry: Label.t }

      let entry_nodes { entry; _ } = Label.Set.singleton entry

      let prev { cfg; _ } node =
        let block = Cfg.get_block_exn cfg node in
        let next = Cfg.successor_labels cfg ~normal:true ~exn:true block in
        Label.Set.elements next

      let next { cfg; _ } node =
        let block = Cfg.get_block_exn cfg node in
        Cfg.predecessor_labels block

      let entry { entry; _ } node =
        assert (Label.equal entry node);
        S.singleton node

      let transfer _cfg node doms_in = S.add node doms_in
    end
  in
  let module Dominator_solver = Make_solver(Dominator_problem) in
  let dominators = Dominator_solver.solve { cfg; entry } in
  Label.Map.map
    (fun { sol_out; _ } ->
      match Dom.to_set sol_out with
      | None -> Label.Set.empty
      | Some set -> set |> Dom.Set.elements |> Label.Set.of_list)
    dominators

let find_post_dominators reload blocks pdoms =
  let common =
    blocks
      |> List.map (fun block -> Label.Map.find block pdoms)
      |> inter_sets
  in
  (* Return all dominators if there are any besides the entry. *)
  let pdom = common |> Label.Set.remove (Inst_id.parent reload) in
  if Label.Set.is_empty pdom then None else Some pdom

(* Find the set of the dominators/post-dominators of the split points, excluding
   any nodes which might be part of loops. *)
let find_dom_pdom g splits ~doms ~pdoms ~spill_inst_id ~reload_inst_id =
  match find_dominators doms (Inst_id.parent spill_inst_id) splits with
  | Some dom ->
    (match find_post_dominators reload_inst_id splits pdoms with
    | Some pdom ->
      let sccs = Meet_graph.sccs g in
      let loop_nodes = List.fold_left Label.Set.union Label.Set.empty sccs in
      let dom = Label.Set.diff dom loop_nodes in
      let pdom = Label.Set.diff pdom loop_nodes in
      if Label.Set.is_empty dom || Label.Set.is_empty pdom then None
      else
        let dominates doms a b =
          (* Checks if a dominates/post-dominates b *)
          match Label.Map.find b doms with
          | nodes when Label.Set.mem a nodes -> true
          | _ -> false
          | exception Not_found -> false
        in
        let dom_compare doms a b =
          if dominates doms a b then 1
          else if dominates doms b a then -1
          else failwith "invalid dominators"
        in
        (* There is a total ordering betwee the set of dominators of a node.
           sort both sets, placing the innermost dominator first. *)
        let dom =
          dom
          |> Label.Set.elements
          |> List.filter (Meet_graph.spill_reaches g)
          |> List.sort (dom_compare doms)
        in
        let pdom =
          pdom
          |> Label.Set.elements
          |> List.filter (Meet_graph.reload_reaches g)
          |> List.sort (dom_compare pdoms)
        in
        (* Pick the innermost post-dom and match it with the innermost dom. *)
        (match pdom with
        | [] -> None
        | reload_block :: _ ->
          let spill_block =
            List.find_opt (fun d -> dominates doms d reload_block) dom
          in
          (match spill_block with
          | None -> None
          | Some spill_block -> Some (spill_block, reload_block)))
    | _ -> None)
  | _ -> None

module Meet_point = Meet_graph.Meet_point


(** Shrink the set of meet nodes/edges by removing the ones dominated by others. *)
let rec find_minimal_meets meets ~doms =
  let has_dominator_of meets' doms =
    List.exists
      (fun node ->
        match node with
        | Meet_point.Node(id', _) -> Label.Set.mem (Inst_id.parent id') doms
        | Meet_point.Edge(_, en) -> Label.Set.mem en doms)
      meets'
  in
  match meets with
  | [] -> []
  | elem :: meets' ->
    let meets' = List.filter (fun elem' -> not (Meet_point.equal elem elem')) meets' in
    let dominated =
      try match elem with
      | Meet_point.Node(id, _) ->
        has_dominator_of meets' (Label.Map.find (Inst_id.parent id) doms)
      | Meet_point.Edge(st, _) ->
        has_dominator_of meets' (Label.Map.find st doms)
      with Not_found -> false
    in
    let rest = find_minimal_meets meets' ~doms in
    if dominated then rest else elem :: rest

let is_move inst =
  match inst.Cfg.desc with
  | Cfg.Op (Cfg.Spill | Cfg.Reload | Cfg.Move) -> true
  | _ -> false

module Fixed_id = struct
  module T = struct
    (* While Inst_id allows fast access for analyses, it is invalidated by changes to
       the CFG. Fixed IDs identify an instruction by block label and UID instead. *)
    type t = Label.t * int

    let compare (_, a) (_, b) = a - b

    let of_inst_id cfg inst_id =
      let id =
        match Inst_id.get_inst cfg inst_id with
        | `Basic b -> b.Cfg.id
        | `Term t -> t.Cfg.id
      in
      Inst_id.parent inst_id, id

    let to_inst_id cfg (block, id) =
      let bb = Cfg.get_block_exn cfg block in
      if id = bb.Cfg.terminator.Cfg.id then Inst_id.Term block
      else
        let rec loop n insts =
          match insts with
          | [] ->
            Misc.fatal_errorf "In function %s, block %d: missing instruction %d"
              (Cfg.fun_name cfg) block id
          | i :: _ when i.Cfg.id = id -> n
          | _ ::insts' -> loop (n + 1) insts'
        in
        Inst_id.Inst(block, loop 0 bb.Cfg.body)

    let get_basic_exn cfg (block, id) =
      let bb = Cfg.get_block_exn cfg block in
      List.find (fun b -> b.Cfg.id = id) bb.Cfg.body
  end

  include T
  module Map = Map.Make(T)
end

(** Returns the ID of the instruction of the end point, if the edge is the only one
    reaching it. *)
let get_unique_target cfg st en =
  let bb = Cfg.get_block_exn cfg en in
  match Cfg.predecessor_labels bb with
  | [ st' ] when Label.equal st st' ->
    (match bb.Cfg.body with
    | b :: _ -> Some b.Cfg.id
    | [] -> Some bb.Cfg.terminator.Cfg.id)
  | _ ->
    None

let find_edges_leaving_block cfg g st =
  let bb = Cfg.get_block_exn cfg st in
  let term = bb.Cfg.terminator in
  let edge_of_label l =
    if not (Meet_graph.has_node g l) then []
    else
      match get_unique_target cfg st l with
      | None -> [`Edge(st, l)]
      | Some id -> [`Before_inst(l, id)]
  in
  let edges_of_set labels =
    labels
    |> Label.Set.elements
    |> List.map edge_of_label
    |> List.flatten
  in
  (match term.Cfg.desc with
  | Tailcall (Self _) | Return | Tailcall (Func _) ->
      [`Fail]
  | Never ->
      []
  | Always l ->
      edge_of_label l
  | Call { successor; _ } ->
      Label.Set.singleton successor |> Label.Set.union bb.Cfg.exns |> edges_of_set
  | Switch labels ->
      labels |> Array.to_seq |> Label.Set.of_seq |> edges_of_set
  | Raise _ -> edges_of_set bb.Cfg.exns
  | Parity_test { ifso; ifnot } | Truth_test { ifso; ifnot } ->
      edges_of_set (Label.Set.of_list [ifso; ifnot])
  | Float_test { lt; gt; eq; uo } ->
      edges_of_set (Label.Set.of_list [lt; gt; eq; uo])
  | Int_test { lt; gt; eq; imm = _; is_signed = _ } ->
      edges_of_set (Label.Set.of_list [lt; gt; eq]))

let find_meet_placement cfg g meets =
  let open Meet_point in
  (* Find placement points inside blocks or on edges. *)
  let placements =
    meets
    |> List.map (fun meet ->
        match meet with
        | Node(inst_id, loc) ->
          (match inst_id, loc with
          | (Inst_id.Inst _ | Inst_id.Term _), (Before|Anywhere) ->
              [ `Before_inst(Inst_id.parent inst_id, Inst_id.get_id cfg inst_id) ]
          | Inst_id.Inst _, After ->
              [ `After_inst(Inst_id.parent inst_id, Inst_id.get_id cfg inst_id) ]
          | Inst_id.Term st, After ->
              find_edges_leaving_block cfg g st)
        | Edge(st, en) ->
            (match get_unique_target cfg st en with
            | Some id -> [`Before_inst(en, id)]
            | None -> [`Edge(st, en)]))
    |> List.flatten
  in
  let rec aggregate edges insts fails = function
    | [] -> edges, insts, fails
    | `Fail :: placements ->
      aggregate edges insts true placements
    | `After_inst(block, id) :: placements ->
      aggregate edges (`After_inst(block, id) :: insts) fails placements
    | `Before_inst(block, id) :: placements ->
      aggregate edges (`Before_inst(block, id) :: insts) fails placements
    | `Edge(st, en) :: placements ->
      let placements =
        List.filter
          (function
          | `Edge(st', en') -> st <> st' || en <> en'
          | _ -> false)
          placements
      in
      aggregate (`Edge(st, en) :: edges) insts fails placements
  in
  aggregate [] [] false placements

let place_meets cl edges insts ~arg ~res =
  let cfg = CL.cfg cl in
  let desc = Cfg.Op Cfg.Move in
  (* This loop destroys positional Inst_ids - fetch all information beforehand. *)
  List.iter
    (fun place ->
      match place with
      | `Edge(start_edge, end_edge) ->
        let block, id = CL.split_edge cl ~start_edge ~end_edge in
        ignore (Cfg.create_and_add_basic cfg block id `Before ~desc ~arg ~res)
      | `Before_inst(block, id) ->
        ignore (Cfg.create_and_add_basic cfg block id `Before ~desc ~arg ~res)
      | `After_inst(block, id) ->
        ignore (Cfg.create_and_add_basic cfg block id `After ~desc ~arg ~res))
    (List.append edges insts)

let place_instruction cl place inst : unit =
  let cfg = CL.cfg cl in
  match place with
  | `Edge(start_edge, end_edge) ->
    let block, id = CL.split_edge cl ~start_edge ~end_edge in
    ignore (Cfg.add_basic cfg block id `Before inst)
  | `Before_inst(block, id) -> ignore (Cfg.add_basic cfg block id `Before inst)
  | `After_inst(block, id) -> ignore (Cfg.add_basic cfg block id `After inst)

let remove_spill_reload cl g meets ~spill ~reload =
  let cfg = CL.cfg cl in
  let spill_block, spill_id = spill in
  let reload_block, reload_id = reload in
  match find_meet_placement cfg g meets with
  | ([] as edges), (([]|[_]) as insts), false
  | ([_] as edges), ([] as insts), false ->
    let arg = (Fixed_id.get_basic_exn cfg spill).Cfg.arg in
    let res = (Fixed_id.get_basic_exn cfg reload).Cfg.res in
    place_meets cl edges insts ~arg ~res;
    Cfg.remove_basic_exn cfg spill_block spill_id;
    Cfg.remove_basic_exn cfg reload_block reload_id;
    true
  | _, _, _ ->
    false

let find_pdom_placement cfg g ~reload ~reloads ~pdom =
  let id = Inst_id.Term pdom in
  match Inst_id.Map.find id reloads with
  | { sol_out; _ } when Inst_id.Set.mem (Fixed_id.to_inst_id cfg reload) sol_out ->
    let bb = Cfg.get_block_exn cfg pdom in
    let term = bb.Cfg.terminator in
    [`Before_inst(pdom, term.Cfg.id)]
  | _ ->
    find_edges_leaving_block cfg g pdom
      |> List.map (function
        | `Edge(st, en) -> [`Edge(st, en)]
        | `Before_inst(block, id) -> [`Before_inst(block, id)]
        | `After_inst(block, id) -> [`After_inst(block, id)]
        | `Fail -> [])
      |> List.flatten
  | exception Not_found ->
    []

let move_spill_reload cl meets g ~spill ~reload ~reloads ~dom ~pdom =
  let cfg = CL.cfg cl in
  let spill_block, spill_id = spill in
  let reload_block, reload_id = reload in
  match find_meet_placement cfg g meets with
  | ([] as edges), (([]|[_]) as insts), false
  | ([_] as edges), ([] as insts), false ->
    (match find_pdom_placement cfg g ~reload ~reloads ~pdom with
    | [pdom_place] ->
      let dom_place =
        let bb = Cfg.get_block_exn cfg dom in
        match bb.Cfg.body with
        | b :: _ -> `Before_inst(dom, b.Cfg.id)
        | [] -> `Before_inst(dom, bb.Cfg.terminator.Cfg.id)
      in
      let spill_inst = Fixed_id.get_basic_exn cfg spill in
      let reload_inst = Fixed_id.get_basic_exn cfg reload in
      let arg = spill_inst.Cfg.arg in
      let res = reload_inst.Cfg.res in
      place_meets cl edges insts ~arg ~res;
      place_instruction cl dom_place spill_inst;
      place_instruction cl pdom_place reload_inst;
      Cfg.remove_basic_exn cfg spill_block spill_id;
      Cfg.remove_basic_exn cfg reload_block reload_id;
      true
    | _ -> false)
  | _, _, _ ->
    false

let sink cl spill reload doms spills reloads =
  let cfg = CL.cfg cl in
  let group = Cfg.fun_name cfg in
  let spill_inst_id = Fixed_id.to_inst_id cfg spill in
  let reload_inst_id = Fixed_id.to_inst_id cfg reload in
  let spill_inst = Inst_id.get_basic cfg spill_inst_id in
  let reload_inst = Inst_id.get_basic cfg reload_inst_id in
  if not (is_move spill_inst && is_move reload_inst) then false
  else begin
    let g = Meet_graph.of_cfg cfg ~spill_inst_id ~spills ~reload_inst_id ~reloads in
    let same_register = spill_inst.Cfg.arg.(0).loc = reload_inst.Cfg.res.(0).loc in
    let meets = find_minimal_meets (Meet_graph.meet_points g) ~doms in
    match Label.Set.elements (Meet_graph.split_points g) with
    | [] ->
      (* There is actually no need to spill - erase spill/reload, extend live range. *)
      if same_register then begin
        (* No copies to be inserted - just extend register liveness. *)
        Statistics.inc ~group ~key:"sink_erase";
        remove_spill_reload cl g [] ~spill ~reload
      end else begin
        (* Insert copies at the optimal set of meet points. *)
        if List.length meets <= 2 then begin
          Statistics.inc ~group ~key:"sink_erase_and_move";
          remove_spill_reload cl g meets ~spill ~reload
        end else begin
          (* Too many meet points - not profitable *)
          Statistics.inc ~group ~key:"sink_erase_and_move_too_many";
          false
        end
      end
    | splits ->
      let pdoms = build_post_dominators cfg reload_inst_id in
      (match find_dom_pdom g splits ~doms ~pdoms ~spill_inst_id ~reload_inst_id with
      | Some (dom, pdom) ->
        (* Remove meet points if they are on the dom-pdom path. *)
        let meets =
          List.filter
            (fun meet ->
              let st, en =
                match meet with
                | Meet_point.Node(id, _) -> let block = Inst_id.parent id in block, block
                | Meet_point.Edge(st, en) -> st, en
              in
              try
                let doms_of_st = Label.Map.find st doms in
                let pdoms_of_en = Label.Map.find en pdoms in
                not (Label.Set.mem dom doms_of_st && Label.Set.mem pdom pdoms_of_en)
              with Not_found -> true)
            meets
        in
        (* The spill and the reload can be placed before the dom and after the pdom *)
        if same_register then begin
          (* No need to insert moves *)
          match List.length meets with
          | 0 ->
            Statistics.inc ~group ~key:"sink_sink_same_reg_no_meet";
            false;
          | _ ->
            Statistics.inc ~group ~key:"sink_sink_same_reg";
            move_spill_reload cl [] g ~spill ~reload ~reloads ~dom ~pdom
        end else begin
          match List.length meets with
          | 0 ->
            (* Spill and reload could be moved closer to each other. *)
            Statistics.inc ~group ~key:"sink_sink_no_meet";
            false
          | 1 ->
            (* Spill and reload moved closer, freed up a path. *)
            Statistics.inc ~group ~key:"sink_sink_and_move";
            move_spill_reload cl meets g ~spill ~reload ~reloads ~dom ~pdom
          | _ ->
            (* Too many meet points - not profitable. *)
            Statistics.inc ~group ~key:"sink_sink_and_move_too_many";
            false
        end
      | _ -> false)
  end

let run cl =
  (* Find spills which are paired to a single reload. *)
  let cfg = CL.cfg cl in
  let solution = Reaching_spills.solve cfg in
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
                Statistics.inc ~group:(Cfg.fun_name cfg) ~key:"spills";
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
          block.Cfg.body))
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
              (Fixed_id.of_inst_id cfg spill, Fixed_id.of_inst_id cfg reload) :: acc
            | _ -> acc)
          | exception Not_found -> acc)
        | _ -> acc)
      spills_to_reloads
      []
  in
  if List.length candidates > 30 then ()
  else begin
    (* Try to sink all candidates, redoing analyses when a spill/reload was sunk. *)
    let run_analyses cl =
      let cfg = CL.cfg cl in
      let doms = Dominators.dominators cfg in
      let sreach = Spill_reload_range.solve_spills cfg in
      let rreach = Spill_reload_range.solve_reloads cfg in
      (doms, sreach, rreach)
    in
    let _, changed =
      List.fold_left
        (fun (analyses, changed) (spill_id, reload_id) ->
          let doms, sreach, rreach = analyses in
          if sink cl spill_id reload_id doms sreach rreach then run_analyses cl, true
          else analyses, changed)
        (run_analyses cl, false)
        candidates
    in
    if changed then Fix_liveness.fix (CL.cfg cl);
  end
