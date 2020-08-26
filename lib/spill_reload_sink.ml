open Data_flow_analysis


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
let find_dom_pdom g splits ~doms ~pdoms ~spill ~reload =
  match find_dominators doms (Inst_id.parent spill) splits with
  | Some dom ->
    (match find_post_dominators reload splits pdoms with
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

let remove_spill_reload _cfg _meets ~spill:_ ~reload:_ =
  false

let move_spill_reload _cfg _meets ~spill:_ ~reload:_ ~dom:_ ~pdom:_ =
  false

let sink cfg spill reload doms spills reloads =
  let group = Cfg.fun_name cfg in
  let spill_inst = Inst_id.get_basic cfg spill in
  let reload_inst = Inst_id.get_basic cfg reload in
  if not (is_move spill_inst && is_move reload_inst) then false
  else begin
    let g = Meet_graph.of_cfg cfg ~spill ~spills ~reload ~reloads in
    let same_register = spill_inst.Cfg.arg.(0).loc = reload_inst.Cfg.res.(0).loc in
    let meets = find_minimal_meets (Meet_graph.meet_points g) ~doms in
    match Label.Set.elements (Meet_graph.split_points g) with
    | [] ->
      (* There is actually no need to spill - erase spill/reload, extend live range. *)
      if same_register then begin
        (* TODO: No copies to be inserted - just extend register liveness. *)
        Statistics.inc ~group ~key:"sink_erase";
        remove_spill_reload cfg [] ~g ~spill ~reload
      end else begin
        (* TODO: Insert copies at the optimal set of meet points. *)
        if List.length meets <= 2 then begin
          Statistics.inc ~group ~key:"sink_erase_and_move";
          remove_spill_reload cfg meets ~g ~spill ~reload
        end else begin
          (* Too many meet points - not profitable *)
          Statistics.inc ~group ~key:"sink_erase_and_move_too_many";
          false
        end
      end
    | splits ->
      let pdoms = build_post_dominators cfg reload in
      (match find_dom_pdom g splits ~doms ~pdoms ~spill ~reload with
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
                Label.Set.mem dom doms_of_st && Label.Set.mem pdom pdoms_of_en
              with Not_found -> true)
            meets
        in
        (* The spill and the reload can be placed before the dom and after the pdom *)
        if same_register then begin
          (* No need to insert moves *)
          (match meets with
          | [] -> Statistics.inc ~group ~key:"sink_sink"
          | _ -> Statistics.inc ~group ~key:"sink_sink_free_path");
          move_spill_reload cfg [] ~spill ~reload ~dom ~pdom
        end else begin
          if List.length meets <= 1 then  begin
            Statistics.inc ~group ~key:"sink_sink_and_move";
            move_spill_reload cfg meets ~spill ~reload ~dom ~pdom
          end else begin
            (* Too many meet points - not profitable. *)
            Statistics.inc ~group ~key:"sink_sink_and_move_too_many";
            false
          end
        end
      | _ -> false)
  end

let run cfg =
  (* Find spills which are paired to a single reload. *)
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
              (spill, reload) :: acc
            | _ -> acc)
          | exception Not_found -> acc)
        | _ -> acc)
      spills_to_reloads
      []
  in
  (* Try to sink all candidates, redoing analyses when a spill/reload was sunk. *)
  let doms = ref (Dominators.dominators cfg) in
  let spill_reach = ref (Spill_reload_range.solve_spills cfg) in
  let reload_reach = ref (Spill_reload_range.solve_reloads cfg) in
  candidates |> List.iter
    (fun (spill_id, reload_id) ->
      if sink cfg spill_id reload_id !doms !spill_reach !reload_reach then begin
        doms := Dominators.dominators cfg;
        spill_reach := Spill_reload_range.solve_spills cfg;
        reload_reach := Spill_reload_range.solve_reloads cfg
      end)
