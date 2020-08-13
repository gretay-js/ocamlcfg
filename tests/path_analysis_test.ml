open Core
open Test_graph

module Paths = struct
  module Path = struct
    type t = ID.t list [@@deriving sexp, equal, compare]
  end

  module Set = Set.Make(Path)

  type t = Set.t [@@deriving sexp, equal]

  let lub = Set.union

  let bot = Set.empty
end

let print_solution solution =
  ID.Map.iter
    (fun id { Ocamlcfg.Analysis.sol_out; _ } ->
      print_s [%message (id: ID.t)];
      Paths.Set.iter sol_out ~f:(fun path ->
        print_string "  ";
        print_s [%message (path : Paths.Path.t)]))
    solution

let paths_next _ node s =
  s
  |> Paths.Set.filter ~f:(fun path -> List.count path ~f:(Int.equal node) < 2)
  |> Paths.Set.map ~f:(fun path -> node :: path)

let () =
  let
    module Problem = struct
      module S = Paths

      let entry _ _ = Paths.Set.singleton []

      let transfer = paths_next
    end
  in
  let module Solver = Make_forward_graph_solver(Problem) in
  print_solution (Solver.solve simple_graph)

let () =
  let
    module Problem = struct
      module S = Paths

      let entry _ _ = Paths.Set.singleton []

      let transfer = paths_next
    end
  in
  let module Solver = Make_backward_graph_solver(Problem) in
  print_solution (Solver.solve simple_graph)

