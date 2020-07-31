open Core
open Ocamlcfg.Analysis

module G = struct
  module ID = struct
    module T = struct
      type t = int [@@deriving sexp, equal, compare]
    end
    include T
    module Map = Caml.Map.Make(T)
    module Set = Caml.Set.Make(T)
  end

  type t = ID.t list ID.Map.t
end

include G

module type Test_graph_problem = sig
  module S : Semilattice

  val empty : t -> ID.t -> S.t
  val entry : t -> ID.t -> S.t
  val f : t -> ID.t -> S.t -> S.t
end

let graph_next t id = ID.Map.find id t

let graph_prev t id =
  t
  |> ID.Map.bindings
  |> List.filter_map ~f:(fun (n, next) ->
        if List.mem next id ~equal:Int.equal then Some n else None)

let is_entry t id = List.is_empty (graph_prev t id)
let is_exit t id = List.is_empty (graph_next t id)

let all_nodes t =
    t
    |> ID.Map.bindings
    |> List.map ~f:fst
    |> ID.Set.of_list

module Make_forward_graph_solver (P: Test_graph_problem) = struct
  module T = struct
    module S = P.S
    module Node = ID

    type t = G.t

    let entries t =
      t
      |> ID.Map.bindings
      |> List.filter_map ~f:(fun (n, _) ->
          match graph_prev t n with
          | [] -> Some n
          | _ -> None)

    let next = graph_next
    let prev = graph_prev
    let entry = P.entry
    let empty = P.empty
    let f = P.f
  end

  let solve = let module M = Make_solver(T) in M.solve
end

module Make_backward_graph_solver (P: Test_graph_problem) = struct
  module T = struct
    module S = P.S
    module Node = ID

    type t = G.t

    let entries t =
      t
      |> ID.Map.bindings
      |> List.filter_map ~f:(fun (n, next) ->
          match next with
          | [] -> Some n
          | _ -> None)

    let next = graph_prev
    let prev = graph_next
    let entry = P.entry
    let empty = P.empty
    let f = P.f
  end

  let solve = let module M = Make_solver(T) in M.solve
end

(**
 * Simple test graph with a conditional in a loop.
 *
 *      1
 *      |
 *      2<----
 *      |     \
 *      3     |
 *     / \    |
 *    4   5   |
 *     \ /    |
 *      6     |
 *      |     /
 *      7-----
 *      |
 *      8
 *)
let simple_graph : G.t =
  ID.Map.singleton 1 [2]
  |> ID.Map.add 2 [3]
  |> ID.Map.add 3 [4;5]
  |> ID.Map.add 4 [6]
  |> ID.Map.add 5 [6]
  |> ID.Map.add 6 [7]
  |> ID.Map.add 7 [2;8]
  |> ID.Map.add 8 []
