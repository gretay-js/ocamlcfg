open Core
open Test_graph

module Dom = struct
  type t =
    | Bot
    | Val of ID.Set.t

  let bot = Bot

  let lub a b =
    match a, b with
    | Bot, _ -> b
    | Val _, Bot -> a
    | Val a', Val b' -> Val (ID.Set.inter a' b')

  let equal a b =
    match a, b with
    | Val a', Val b' -> ID.Set.equal a' b'
    | Val _, Bot -> false
    | Bot, Val _ -> false
    | Bot, Bot -> true
end

let print_solution solution =
  ID.Map.iter
    (fun id { Ocamlcfg.Analysis.sol_out; _ } ->
      Printf.printf "%d: " id;
      (match sol_out with
      | Dom.Val sol -> ID.Set.iter (Printf.printf " %d") sol;
      | Dom.Bot -> Printf.printf "unreachable");
      Printf.printf "\n")
    solution

let () =
  let
    module DominatorsProblem = struct
      module S = Dom

      let entry _ node = Dom.Val (ID.Set.singleton node)

      let transfer _cfg v val_in =
        match val_in with
        | Dom.Bot -> Dom.Bot
        | Dom.Val val_in' -> Dom.Val (ID.Set.add v val_in')
    end
  in
  let module Solver = Make_forward_graph_solver(DominatorsProblem) in
  print_solution (Solver.solve simple_graph)
