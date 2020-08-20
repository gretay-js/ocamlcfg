open Data_flow_analysis
open Spill_reload_range

let is_move inst =
  match inst.Cfg.desc with
  | Cfg.Op Cfg.Spill
  | Cfg.Op Cfg.Reload
  | Cfg.Op Cfg.Move ->
    true
  | _ ->
    false

module Meet_point = struct
  type t
    = Before
    | After
    | Anywhere
    | Nowhere
end

let blocks_of_insts insts =
  insts
  |> Inst_id.Set.elements
  |> List.map Inst_id.parent
  |> Label.Set.of_list

let find_dominators doms spill insts =
  let common =
    match Label.Set.elements (blocks_of_insts insts) with
    | [] -> Label.Set.empty
    | b :: bs ->
      List.fold_left
        (fun common block -> Label.Set.inter (Label.Map.find block doms) common)
        (Label.Map.find b doms)
        bs
  in
  (* Return all dominators if there are any besides the entry. *)
  let spill_doms = Label.Map.find spill doms in
  let doms_after_spill = Label.Set.diff common spill_doms in
  if Label.Set.is_empty doms_after_spill then None else Some doms_after_spill

let find_post_dominators cfg start nodes =
  let blocks = blocks_of_insts nodes in
  let entry = Inst_id.parent start in
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
  let common =
    Label.Set.fold
      (fun block doms -> Dom.lub (Label.Map.find block dominators).sol_out doms)
      blocks
      Dom.full
  in
  (* Return all dominators if there are any besides the entry. *)
  Option.bind (Dom.to_set common) (fun doms ->
    let doms =
      doms
      |> Dom.Set.elements
      |> Label.Set.of_list
      |> Label.Set.remove entry
    in
    if Label.Set.is_empty doms then None else Some doms)

let sink cfg spill_id reload_id doms spills reloads =
  let spill = Inst_id.get_basic cfg spill_id in
  let reload = Inst_id.get_basic cfg reload_id in
  if not (is_move spill) || not (is_move reload) then false else begin
    (* Identify all nodes leading to the reload. *)
    let reaches_reload =
      let rec dfs acc node =
        if Inst_id.Set.mem node acc then acc
        else
          let acc = Inst_id.Set.add node acc in
          if Inst_id.equal node spill_id then acc
          else List.fold_left dfs acc (Inst_id.get_predecessors_of_inst cfg node)
      in
      dfs Inst_id.Set.empty reload_id
    in
    (* Identify all nodes between the pair and find potential hoist points. *)
    let node_meets =
      let classify_node node =
        let { sol_in = reloads_after; sol_out = reloads_before } =
          Inst_id.Map.find node reloads
        in
        let { sol_in = spills_before; sol_out = spills_after } =
          Inst_id.Map.find node spills
        in
        let spill_before = Inst_id.Set.mem spill_id spills_before in
        let spill_after = Inst_id.Set.mem spill_id spills_after in
        let reload_before = Inst_id.Set.mem reload_id reloads_before in
        let reload_after = Inst_id.Set.mem reload_id reloads_after in
        let before = spill_before && reload_before in
        let after = spill_after && reload_after in
        let open Meet_point in
        if before && after then Anywhere
        else if before then Before
        else if after then After
        else Nowhere
      in
      let rec dfs acc node =
        if Inst_id.Map.mem node acc then acc
        else if not (Inst_id.Set.mem node reaches_reload) then acc
        else
          let acc = Inst_id.Map.add node (classify_node node) acc in
          if Inst_id.equal node reload_id then acc
          else List.fold_left dfs acc (Inst_id.get_successors_of_inst cfg node)
      in
      dfs Inst_id.Map.empty spill_id
    in
    (* Identify the set of nodes where both the spill and the reload reach *)
    let meets  =
      Inst_id.Map.fold
        (fun id t meets ->
          let open Meet_point in
          match t with
          | Nowhere -> meets
          | Before | After | Anywhere ->
            Inst_id.Set.add id meets)
        node_meets
        Inst_id.Set.empty
    in
    if Inst_id.Set.is_empty meets then false
    else begin
      (* Find the nodes betwee the spill and the reload which are on a path that
         contains at least a meet point. These node do not explicitly require spils
         or reloads to be inserted between them. *)
      let nodes =
        node_meets
        |> Inst_id.Map.bindings
        |> List.map fst
        |> Inst_id.Set.of_list
      in
      let nodes_on_meet_path =
        let rec dfs next test acc node =
          if Inst_id.Set.mem node acc then acc
          else if not (Inst_id.Set.mem node nodes) then acc
          else if not (test node) then acc
          else
            let acc = Inst_id.Set.add node acc in
            List.fold_left (dfs next test) acc (next node)
        in
        let succ = Inst_id.get_successors_of_inst cfg in
        let succ_test node =
          let { sol_out = reloads_before; _ } = Inst_id.Map.find node reloads in
          Inst_id.Set.mem reload_id reloads_before
        in
        let pred = Inst_id.get_predecessors_of_inst cfg in
        let pred_test node =
          let { sol_out = spills_after; _ } = Inst_id.Map.find node spills in
          Inst_id.Set.mem spill_id spills_after
        in
        Inst_id.Set.fold
          (fun node acc -> dfs pred pred_test (dfs succ succ_test acc node) node)
          meets
          Inst_id.Set.empty
      in
      (* Find the set of nodes which which are not on a path with meet nodes.
         These nodes will have to be dominated by spills/post-dominated by reloads.
         Find all the dominators/post-dominators that are common to this set. *)
      let splits = Inst_id.Set.diff nodes nodes_on_meet_path in
      (match find_dominators doms (Inst_id.parent spill_id) splits with
      | Some dom ->
        (match find_post_dominators cfg reload_id splits with
        | Some post_dom ->
          (* Some insert points will not be reached by the spill or the reload.
             Discard them from the set of potential candidates. *)
          (* If there are any meet points, we need to see if the spill/reload work on the
             same register. If that is the case, no new instructions are required at the
             meet point. Otherwise, a move betweent the registers has to be inserted. *)
          if spill.Cfg.arg.(0).loc = reload.Cfg.res.(0).loc then begin
            (* Find the nodes which require a dominating spill/reload. *)

            Format.printf "%s: %d %d\n%a\n%a"
              (Cfg.fun_name cfg)
              (List.length (Cfg.blocks cfg))
              (Inst_id.Set.cardinal meets)
              Cfg.print_basic spill
              Cfg.print_basic reload;

            Format.printf "\n\tmeets: ";
            dom |> Label.Set.iter (Format.printf " %d");
            Format.printf "\n\n";

            Format.printf "\n\tsplits: ";
            post_dom |> Label.Set.iter (Format.printf " %d");
            Format.printf "\n\n";

            false
          end else begin
            false
          end
        | _ -> false)
      | _ -> false)
    end
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
          block.Cfg.body)
      )
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
  let doms = ref (Dominators.dominators cfg) in
  let spill_reach = ref (Spill_sink_solver.solve cfg) in
  let reload_reach = ref (Reload_hoist_solver.solve cfg) in

  if not (String.equal (Cfg.fun_name cfg) "camlCsm_lib__Csm_1_4_4_message_generated__fun_82538") then () else
  Inst_id.Map.iter (fun id { sol_in; sol_out } ->
    Format.printf "%a: \n\t" Inst_id.print id;
    sol_in |> Inst_id.Set.iter (fun id -> Format.printf " %a" Inst_id.print id); Format.printf "\n\t";
    sol_out |> Inst_id.Set.iter (fun id -> Format.printf " %a" Inst_id.print id); Format.printf "\n\n"
    ) !spill_reach;

  (*Printf.printf "%s %d\n" (Cfg.fun_name cfg) (List.length candidates);*)
  candidates |> List.iter
    (fun (spill_id, reload_id) ->
      if sink cfg spill_id reload_id !doms !spill_reach !reload_reach then begin
        doms := Dominators.dominators cfg;
        spill_reach := Spill_sink_solver.solve cfg;
        reload_reach := Reload_hoist_solver.solve cfg
      end)
