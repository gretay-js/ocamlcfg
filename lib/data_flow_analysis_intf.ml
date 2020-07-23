
module S = struct
  module type Semilattice = sig
    type t

    val equal : t -> t -> bool
    val lub : t -> t -> t
    val top : t
  end

  (* Data-flow problem on an arbitrary graph. *)
  module type Problem = sig
    module S : Semilattice
    module Node : Identifiable.S

    type t

    val entries : t -> Node.t list

    val next : t -> Node.t -> Node.t list
    val prev : t -> Node.t -> Node.t list

    val f : t -> Node.t -> S.t -> S.t
  end

  (* Data-flow problem with kill/gen sets. *)
  module type KillGenProblem = sig
    module S : Semilattice

    module KillGen : sig
      type t
      val f : S.t -> t -> S.t
      val dot : t -> t -> t
    end

    module Parent : Identifiable.S
    module Node : Identifiable.S

    type t

    val entries : t -> Parent.t list

    val next : t -> Parent.t -> Parent.t list
    val prev : t -> Parent.t -> Parent.t list

    val start_node : t -> Parent.t -> Node.t
    val next_node : t -> Parent.t -> Node.t -> Node.t option
    val prev_node : t -> Parent.t -> Node.t -> Node.t option

    val kg : t -> Parent.t -> Node.t -> KillGen.t
  end

  module Dom = struct
    (* Lattice for dominator analysis. *)
    type t = Label.t
    module Set = Label.Set
  end
end
