module type Semilattice = sig
  type t

  val equal : t -> t -> bool
  (** Returns true if two semilattice values are equal. *)

  val lub : t -> t -> t
  (** Least Upper Bound of two values. *)
end

module type Semigroup = sig
  type t

  val dot : t -> t -> t
  (** Associative binary operator. *)
end

module type Semigroup_action = sig
  module S : Semilattice
  module G : Semigroup

  val f : S.t -> G.t -> S.t
  (** Applies the effect onto the semilattice.

      The composition must satisfy the following property:

      f(x, dot a b) = f(f(x, a), b)
    *)
end

(** Identifier for a node. *)
module type Node_id = sig
  type t

  module Map : Map.S with type key := t
  module Set : Set.S with type elt := t
end

(** Data-flow problem on an arbitrary graph. *)
module type Problem = sig
  module S : Semilattice
  module Node : Node_id

  type t

  val entries : t -> Node.Set.t

  val next : t -> Node.t -> Node.t list
  val prev : t -> Node.t -> Node.t list

  val empty : t -> Node.t -> S.t
  val entry : t -> Node.t -> S.t
  val f : t -> Node.t -> S.t -> S.t
end

(** Data-flow problem with semigroup actions. *)
module type Semigroup_action_problem = sig
  module A : Semigroup_action

  module Parent : Node_id

  module Node : sig
    include Node_id
    val parent : t -> Parent.t
  end

  type t

  val entries : t -> Parent.Set.t

  val next : t -> Parent.t -> Parent.t list
  val prev : t -> Parent.t -> Parent.t list

  val start_node : t -> Parent.t -> Node.t
  val next_node : t -> Node.t -> Node.t option

  val empty : t -> Parent.t -> A.S.t
  val entry : t -> Parent.t -> A.S.t
  val kg : t -> Node.t -> A.G.t
end

(** Data-flow problem explicitly for the cfg. *)
module type Cfg_semigroup_action_problem = sig
  module A : Semigroup_action

  type t

  val cfg : t -> Cfg.t

  val empty : t -> Label.t -> A.S.t
  val entry : t -> Label.t -> A.S.t
  val kg : t -> Inst_id.t -> A.G.t
end

(** Solver for a data flow problem. *)
module type Solver = sig
  (** Type of the input to the problem *)
  type t

  (** Type of solutions to the problem *)
  type 'a solution

  (** Type of the semilattice. *)
  module S : Semilattice

  (** Type of the nodes. *)
  module Node : Node_id

  val solve : t -> S.t solution Node.Map.t
  (** Solver method *)
end

module type S = sig

  module type Semilattice = Semilattice

  module type Semigroup = Semigroup

  module type Semigroup_action = Semigroup_action

  module type Node_id = Node_id

  module type Problem = Problem

  module type Semigroup_action_problem = Semigroup_action_problem

  module type Cfg_semigroup_action_problem = Cfg_semigroup_action_problem

  (** Solution to a data flow problem. *)
  type 's solution = { sol_in: 's; sol_out: 's }

  module type Solver = Solver with type 'a solution := 'a solution

  module Make_solver (P: Problem) : Solver
    with type t := P.t
     and module S := P.S
     and module Node := P.Node

  module Make_kill_gen_solver (P: Semigroup_action_problem) : Solver
    with type t := P.t
     and module S := P.A.S
     and module Node := P.Node

  module Make_forward_cfg_solver (P: Cfg_semigroup_action_problem) : Solver
    with type t := P.t
     and module S := P.A.S
     and module Node := Inst_id

  module Make_backward_cfg_solver (P: Cfg_semigroup_action_problem): Solver
    with type t := P.t
     and module S := P.A.S
     and module Node := Inst_id

end
