open Data_flow_analysis

let reg_to_loc r = r.Reg.loc, Proc.register_class r

module LocationSet = struct
  include Set.Make(struct
    type t = Reg.location * int
    let compare = compare
  end)

  let bot = empty

  let lub = union

  let of_reg_array arr =
    arr
    |> Array.map reg_to_loc
    |> Array.to_list
    |> of_list

  let print fmt =
    iter
      (fun (loc, cls) ->
        let open Reg in
        match loc with
        | Unknown -> ()
        | Reg r -> Format.fprintf fmt " [%i:%d]" r cls
        | Stack(Local s) -> Format.fprintf fmt " [s%i:%d]" s cls
        | Stack(Incoming s) -> Format.fprintf fmt " [si%i:%d]" s cls
        | Stack(Outgoing s) -> Format.fprintf fmt " [so%i:%d]" s cls)
end

module Problem = struct
  module A = struct
    module S = LocationSet

    module G = struct
      type t =
        { kill: LocationSet.t
        ; gen: LocationSet.t
        }

      let dot curr prev =
        { kill = LocationSet.union curr.kill prev.kill;
          gen = LocationSet.union curr.gen (LocationSet.diff prev.gen curr.kill)
        }
    end

    let apply s action =
      LocationSet.union action.G.gen (LocationSet.diff s action.G.kill)
  end

  type t = Cfg.t

  let cfg t = t

  let entry _ _ = LocationSet.bot

  let action cfg id =
    let block = Cfg.get_block_exn cfg (Inst_id.parent id) in
    let is_handler =
      block.is_trap_handler && match id with
      | Inst_id.Term _ when block.body = [] -> true
      | Inst_id.Inst(_, 0) -> true
      | _ -> false
    in
    let kill_gen inst destroyed =
      let kill_inst =
        LocationSet.union
          (LocationSet.of_reg_array inst.Cfg.res)
          (LocationSet.of_reg_array (Array.map Proc.phys_reg destroyed))
      in
      let gen_inst = LocationSet.of_reg_array inst.Cfg.arg in
      let kill =
        if is_handler then LocationSet.add (reg_to_loc Proc.loc_exn_bucket) kill_inst
        else kill_inst
      in
      let gen =
        if is_handler then LocationSet.remove (reg_to_loc Proc.loc_exn_bucket) gen_inst
        else gen_inst
      in
      { A.G.kill; gen }
    in
    match Inst_id.get_inst cfg id with
    | `Term i -> kill_gen i (Cfg.destroyed_at_terminator i.Cfg.desc)
    | `Basic i -> kill_gen i (Cfg.destroyed_at_basic i.Cfg.desc)
end

module Solver = Make_backward_cfg_solver(Problem)

let verify inst id ~solution =
  match Inst_id.Map.find id solution with
  | { sol_in = live_out; _ } ->
    (* Check if the live-across set recorded in the instruction includes
     * the same locations as the one computed by the analysis. *)
    let live_across =
      inst.Cfg.live
      |> Reg.Set.elements
      |> List.map reg_to_loc
      |> LocationSet.of_list
    in
    let live_across' =
      LocationSet.diff live_out (LocationSet.of_reg_array inst.Cfg.res)
    in
    (* In linear, some live sets are overapproximated - this is a loose check to
     * ensure that everything that is compute as live here is included in the
     * live-across set attached to each instruction.
     *)
    if LocationSet.subset live_across' live_across
      then `Equal
      else `Diff(live_across, live_across')
  | exception Not_found ->
    (* Unreachable code, no diagnostic. *)
    `Equal

let has_live_info_inst = function
  | Cfg.Op _ ->
    true
  | Cfg.Pushtrap _
  | Cfg.Poptrap
  | Cfg.Reloadretaddr
  | Cfg.Prologue ->
    false

let has_live_info_term = function
  | Cfg.Always _
  | Cfg.Never
  | Cfg.Int_test _
  | Cfg.Parity_test _
  | Cfg.Float_test _
  | Cfg.Truth_test _
  | Cfg.Tailcall _
  | Cfg.Switch _
  | Cfg.Return ->
    false
  | Cfg.Call _
  | Cfg.Raise _ ->
    true

let diagnostic cfg i live live' =
  let buffer = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.fprintf fmt "%s\n" (Cfg.fun_name cfg);
  (match i with
  | `Basic b -> Cfg.print_basic fmt b
  | `Terminator t -> Cfg.print_terminator fmt t);
  Format.fprintf fmt "\nref: %a" LocationSet.print live;
  Format.fprintf fmt "\nout: %a" LocationSet.print live';
  Format.fprintf fmt "\n\n";
  Misc.fatal_error (Buffer.contents buffer)

let run cfg =
  let solution = Solver.solve cfg in
  Cfg.iter_blocks cfg ~f:(fun block bb ->
    let _ = List.fold_left
      (fun n i->
        (match verify i (Inst_id.Inst(block, n)) ~solution with
        | `Diff(live, live') when has_live_info_inst i.Cfg.desc ->
          diagnostic cfg (`Basic i) live live'
        | _ -> ());
        n + 1)
      0
      bb.body
    in
    let t = bb.terminator in
    match verify t (Inst_id.Term block) ~solution with
    | `Diff(live, live') when has_live_info_term t.Cfg.desc ->
      diagnostic cfg (`Terminator t) live live'
    | _ -> ());
