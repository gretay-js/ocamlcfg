open Core

module Label = Ocamlcfg.Label
module Scc = Ocamlcfg.Scc

module Scc_problem = struct
  module Node = Label

  type t = unit

  let entries _ = Label.Set.singleton 1

  let next _ node =
    Label.Set.of_list (match node with
    | 1 -> [2]
    | 2 -> [3;4]
    | 3 -> [5]
    | 4 -> [5]
    | 5 -> [6; 1]
    | 6 -> [7]
    | 7 -> [7;8]
    | 8 -> [9]
    | 9 -> [10]
    | 10 -> [9;11]
    | 11 -> []
    | _ -> failwith "invalid node")
end

module Scc_solver = Scc.Make_solver(Scc_problem)


let () =
  let sccs = Scc_solver.solve () in
  List.iter sccs ~f:(fun scc ->
    scc |> Label.Set.iter (fun node -> Printf.printf " %d" node);
    Printf.printf "\n")
