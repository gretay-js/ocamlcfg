open Core
open Test_graph

module Dom = struct
  include Ocamlcfg.Full_set.Make(ID)
  let bot = full
  let lub = inter
end

let print_solution solution =
  ID.Map.iter
    (fun id { Ocamlcfg.Analysis.sol_out; _ } ->
      Printf.printf "%d: " id;
      (match Dom.to_set sol_out with
      | Some sol -> Dom.Set.iter (Printf.printf " %d") sol;
      | None -> Printf.printf "unreachable");
      Printf.printf "\n")
    solution

let () =
  let
    module DominatorsProblem = struct
      module S = Dom

      let entry _ node = Dom.singleton node

      let transfer _cfg node doms_in = Dom.add node doms_in
    end
  in
  let module Solver = Make_forward_graph_solver(DominatorsProblem) in
  print_solution (Solver.solve simple_graph)
