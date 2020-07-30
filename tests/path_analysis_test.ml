open Core
open Test_graph

module Paths = struct
  module Path = struct
    type t = ID.t list [@@deriving sexp, equal, compare]
  end

  module Set = Set.Make(Path)

  type t = Set.t [@@deriving sexp, equal]

  let lub = Set.union
end

let print_solution solution =
  ID.Map.iter
    (fun id (_, path_out) ->
      print_s [%message (id: ID.t)];
      Paths.Set.iter path_out ~f:(fun path ->
        print_string "  ";
        print_s [%message (path : Paths.Path.t)]))
    solution

let path_f _ node s =
  s
  |> Paths.Set.filter ~f:(fun path -> List.count path ~f:(Int.equal node) < 2)
  |> Paths.Set.map ~f:(fun path -> node :: path)

let () =
  let
    module Problem = struct
      module S = Paths

      let init t id =
        ((if is_entry t id then Paths.Set.singleton [] else Paths.Set.empty), Paths.Set.empty)

      let f = path_f
    end
  in
  let module Solver = Make_forward_graph_solver(Problem) in
  print_solution (Solver.solve simple_graph)

let () =
  let
    module Problem = struct
      module S = Paths

      let init t id =
        ((if is_exit t id then Paths.Set.singleton [] else Paths.Set.empty), Paths.Set.empty)

      let f = path_f
    end
  in
  let module Solver = Make_backward_graph_solver(Problem) in
  print_solution (Solver.solve simple_graph)

