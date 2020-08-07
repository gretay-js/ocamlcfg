open Data_flow_analysis

(** Identifier for a stack slot. *)
module Slot = struct
  module T = struct
    type t = { loc: int; reg_class: int }

    let compare a b =
      match compare a.reg_class b.reg_class with
      | 0 -> compare a.loc b.loc
      | c -> c
  end

  include T
  module Map = Map.Make(T)
end


module SlotsInRegs = struct
  type t = int Slot.Map.t

  let empty = Slot.Map.empty
  let equal = Slot.Map.equal (=)

  let lub = Slot.Map.merge (fun _ l r ->
    match l, r with
    | Some a, Some b when a = b -> Some a
    | _ -> None)
end

module Problem = struct
  module RegSet = Set.Make(Int)

  module K = struct
    module S = SlotsInRegs

    type t =
      { kills: RegSet.t;
        (* List of registers overwritten by the instruction *)
        gens: int Slot.Map.t
        (* Spill instruction which writes a reg to stack *)
      }

    let dot curr prev =
      let kills = RegSet.union curr.kills prev.kills in
      let gens =
        Slot.Map.merge
          (fun _ l r ->
            match l, r with
            | Some _, _ -> l
            | None, Some r when RegSet.mem r curr.kills -> None
            | None, _ -> r)
          curr.gens
          prev.gens
      in
      { kills; gens }

    let f s kg =
      Slot.Map.merge
        (fun _ l r ->
          match l, r with
          | _, Some _ -> r
          | Some r, None when RegSet.mem r kg.kills -> None
          | _, None -> l)
        s
        kg.gens
  end

  type t = Cfg.t

  let cfg t = t

  let entry _ _ = SlotsInRegs.empty
  let empty _ _ = SlotsInRegs.empty

  let kg t id =
    let open Cfg in
    let open Reg in
    let kill res destroyed =
      let res_kills =
        res
        |> Array.to_list
        |> List.filter_map (fun reg ->
          match reg.loc with
          | Reg r -> Some r
          | _ -> None)
        |> RegSet.of_list
      in
      let kills = RegSet.union (RegSet.of_list (Array.to_list destroyed)) res_kills in
      { K.kills; gens = Slot.Map.empty }
    in
    let spill r loc reg_class =
      { K.kills = RegSet.empty; gens = Slot.Map.singleton { Slot.loc; reg_class} r }
    in
    match Inst_id.get_inst t id with
    | `Term term ->
      kill term.res (destroyed_at_terminator term.desc)
    | `Basic ({ desc = Op Move; _ } as i) ->
      (match i.arg, i.res with
      | [| { loc = Reg r; _ } |], [| { loc = Stack (Local slot); _ } as dst |] ->
        spill r slot (Proc.register_class dst)
      | _ ->
        kill i.res (destroyed_at_basic i.desc))
    | `Basic ({ desc = Op Spill; _ } as i) ->
      (match i.arg, i.res with
      | [| { loc = Reg r; _ } |], [| { loc = Stack (Local loc); _ } as dst |] ->
        spill r loc (Proc.register_class dst)
      | _ ->
        failwith "invalid spill")
    | `Basic i ->
      kill i.res (destroyed_at_basic i.desc)
end

module Solver = Make_forward_cfg_solver(Problem)


let rewrite inst id ~solution =
  match Inst_id.Map.find id solution with
  | avail_in, _ ->
    let arg =
      Array.map
        (fun r ->
          match r.Reg.loc with
          | Reg.Stack (Reg.Local loc) when not (Array.mem r inst.Cfg.res) ->
            let reg_class = Proc.register_class r in
            (match Slot.Map.find_opt { Slot.loc; reg_class} avail_in with
            | None -> r
            | Some reg -> { r with loc = Reg.Reg reg })
          | _ -> r)
        inst.Cfg.arg
    in
    { inst with arg }
  | exception Not_found -> inst

let run cfg =
  let solution = Solver.solve cfg in
  Cfg.iter_blocks cfg ~f:(fun block bb ->
    let term = rewrite bb.terminator (Inst_id.Term block) ~solution in
    let body, _ = List.fold_left
      (fun (body, n) i->
        let i' = rewrite i (Inst_id.Inst(block, n)) ~solution in
        i' :: body, n + 1)
      ([], 0)
      bb.body
    in
    bb.terminator <- term;
    bb.body <- List.rev body)
