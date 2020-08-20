open Data_flow_analysis

module Dom = struct
  include Full_set.Make(Label)

  let bot = full

  let lub = inter
end

module Dominator_problem = struct
  module S = Dom
  module Node = Label

  type t = Cfg.t

  let entry_nodes cfg = Label.Set.singleton (Cfg.entry_label cfg)

  let next cfg node =
    let block = Cfg.get_block_exn cfg node in
    let next = Cfg.successor_labels cfg ~normal:true ~exn:true block in
    Label.Set.elements next

  let prev cfg node =
    let block = Cfg.get_block_exn cfg node in
    Cfg.predecessor_labels block

  let entry _cfg node = Dom.singleton node

  let transfer _cfg node doms_in = Dom.add node doms_in
end

module Dominator_solver = Make_solver(Dominator_problem)

let dominators cfg =
  let solution = Dominator_solver.solve cfg in
  Label.Map.map
    (fun { sol_out; _ } ->
      match Dom.to_set sol_out with
      | None -> Label.Set.empty
      | Some set -> set |> Dom.Set.elements |> Label.Set.of_list)
    solution
