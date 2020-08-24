open Data_flow_analysis

(*
let is_move inst =
  match inst.Cfg.desc with
  | Cfg.Op Cfg.Spill
  | Cfg.Op Cfg.Reload
  | Cfg.Op Cfg.Move ->
    true
  | _ ->
    false
*)

let find_dominators doms spill blocks =
  let common =
    match Label.Set.elements blocks with
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

let find_post_dominators cfg start blocks =
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

let sink cfg spill reload doms spills reloads =
  let graph = Meet_graph.of_cfg cfg ~spill ~spills ~reload ~reloads in
  match Meet_graph.meet_points graph with
  | [] -> false
  | _meets ->
    let splits = Meet_graph.split_points graph in
    (match find_dominators doms (Inst_id.parent spill) splits with
    | Some dom ->
      (match find_post_dominators cfg reload splits with
      | Some pdom ->
        Format.printf "\n\n\n%s %d\n\n" (Cfg.fun_name cfg) (List.length (Cfg.blocks cfg));
        Meet_graph.print Format.std_formatter graph;
        Format.printf "splits: ";
        Label.Set.iter (fun id -> Format.printf " %d" id) splits;
        Format.printf "\ndoms:";
        dom |> Label.Set.iter (Format.printf " %d");
        Format.printf "\npdoms:";
        pdom |> Label.Set.iter (Format.printf " %d");
        Format.printf "\n\n\n";
        false
      | _ -> false)
    | _ -> false)
  (*
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
  *)

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
  let doms = ref (Dominators.dominators cfg) in
  let spill_reach = ref (Spill_reload_range.solve_spills cfg) in
  let reload_reach = ref (Spill_reload_range.solve_reloads cfg) in
  (*
  ignore (Meet_graph.of_cfg )
  if not (String.equal (Cfg.fun_name cfg) "camlCsm_lib__Csm_1_4_4_message_generated__fun_82538") then () else
  Inst_id.Map.iter (fun id { sol_in; sol_out } ->
    Format.printf "%a: \n\t" Inst_id.print id;
    sol_in |> Inst_id.Set.iter (fun id -> Format.printf " %a" Inst_id.print id); Format.printf "\n\t";
    sol_out |> Inst_id.Set.iter (fun id -> Format.printf " %a" Inst_id.print id); Format.printf "\n\n"
    ) !spill_reach;
  *)
  (*Printf.printf "%s %d\n" (Cfg.fun_name cfg) (List.length candidates);*)
  candidates |> List.iter
    (fun (spill_id, reload_id) ->
      if sink cfg spill_id reload_id !doms !spill_reach !reload_reach then begin
        doms := Dominators.dominators cfg;
        spill_reach := Spill_reload_range.solve_spills cfg;
        reload_reach := Spill_reload_range.solve_reloads cfg
      end)
