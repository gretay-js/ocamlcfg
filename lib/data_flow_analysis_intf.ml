
module S = struct
  module type Semilattice = sig
    type t

    val equal : t -> t -> bool
    val lub : t -> t -> t
    (* Outgoing value from nodes with no predecessors *)
  end

  (* Identifier for a node. *)
  module type Node_id = sig
    type t
    module Map : Map.S with type key := t
    module Set : Set.S with type elt := t
  end

  (* Data-flow problem on an arbitrary graph. *)
  module type Problem = sig
    module S : Semilattice
    module Node : Node_id

    type t

    val entries : t -> Node.t list

    val next : t -> Node.t -> Node.t list
    val prev : t -> Node.t -> Node.t list

    val empty : t -> Node.t -> S.t
    val entry : t -> Node.t -> S.t
    val f : t -> Node.t -> S.t -> S.t
  end

  module type KillGen = sig
    module S : Semilattice
    type t
    (* Type describing node-specific transfer in a composable way. *)
    val f : S.t -> t -> S.t
    (* Applies the effect onto the semilattice *)
    val dot : t -> t -> t
    (* Composes two structures: h = dot f g, h(x) = f(g(x))) *)
  end

  (* Data-flow problem with kill/gen sets. *)
  module type KillGenProblem = sig
    module K : KillGen

    module Parent : Node_id

    module Node : sig
      include Node_id
      val parent : t -> Parent.t
    end

    type t

    val entries : t -> Parent.t list

    val next : t -> Parent.t -> Parent.t list
    val prev : t -> Parent.t -> Parent.t list

    val start_node : t -> Parent.t -> Node.t
    val next_node : t -> Node.t -> Node.t option

    val empty : t -> Parent.t -> K.S.t
    val entry : t -> Parent.t -> K.S.t
    val kg : t -> Node.t -> K.t
  end

  (* Identifier for an instruction in the cfg. *)
  module Inst_id = struct
    module T = struct
      type t
        = Inst of Label.t * int
        | Term of Label.t

      let compare a b =
        match a, b with
        | Term la, Term lb -> Label.compare la lb
        | Term la, Inst (lb, _) ->
          (match Label.compare la lb with
          | 0 -> 1
          | c -> c)
        | Inst (la, _), Term lb ->
          (match Label.compare la lb with
          | 0 -> -1
          | c -> c)
        | Inst (la, ia), Inst (lb, ib) ->
          match Label.compare la lb with
          | 0 -> ia - ib
          | c -> c

      let parent = function
        | Inst(p, _) -> p
        | Term p -> p
    end

    include T
    module Map = Map.Make(T)
    module Set = Set.Make(T)
  end

  (* Data-flow problem explicitly for the cfg. *)
  module type CfgKillGenProblem = sig
    module K : KillGen

    type t
    val cfg : t -> Cfg.t

    val empty : t -> Label.t -> K.S.t
    val entry : t -> Label.t -> K.S.t
    val kg : t -> Inst_id.t -> K.t
  end
end
