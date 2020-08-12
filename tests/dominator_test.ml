open Core
open Test_graph

module Dom = struct
  type t = ID.Set.t
  let lub = ID.Set.inter
  let equal = ID.Set.equal
end

let print_solution solution =
  ID.Map.iter
    (fun id { Ocamlcfg.Analysis.sol_out; _ } ->
      Printf.printf "%d: " id;
      ID.Set.iter (Printf.printf " %d") sol_out;
      Printf.printf "\n")
    solution

let () =
  let
    module DominatorsProblem = struct
      module S = Dom

      let empty cfg _ = all_nodes cfg
      let entry _ node = ID.Set.singleton node

      let f _cfg node v = ID.Set.add node v
    end
  in
  let module Solver = Make_forward_graph_solver(DominatorsProblem) in
  print_solution (Solver.solve simple_graph)
