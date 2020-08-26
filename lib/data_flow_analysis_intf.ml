module type Semilattice = sig
  type t

  val equal : t -> t -> bool
  (** Returns true if two semilattice values are equal. *)

  val lub : t -> t -> t
  (** Least Upper Bound of two values. *)

  val bot : t
  (** Bottom element, all other lattice values are greater than this. *)
end

module type Semigroup = sig
  type t

  val dot : t -> t -> t
  (** Associative binary operator. *)
end

module type Semigroup_action = sig
  module S : Semilattice
  module G : Semigroup

  val apply : S.t -> G.t -> S.t
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

  (* XCR lwhite: I'd probably use something like [entry_nodes] rather than
     [entries]. "entries" has other meanings e.g. the entries in a set or map, which can
     easily be confused. *)
  val entry_nodes : t -> Node.Set.t

  val next : t -> Node.t -> Node.t list
  val prev : t -> Node.t -> Node.t list

  (* XCR lwhite: I think that [empty] is supposed to always be the bottom element of the
     semilattice. i.e. lub empty x = x. So it might make more sense to put it in the
     [Semilattice] module type without any parameters.

     nlicker: empty was created from the cfg and/or node IDs since in the case of
     dominators it is the set of all nodes. I have adapted dominator_test to handle
     this symbolically (and more efficiently). *)
  val entry : t -> Node.t -> S.t

  val transfer : t -> Node.t -> S.t -> S.t
end

(** Data-flow problem with semigroup actions.

    This solver is generalised for graphs with some hierarchy, where sequences of nodes
    with a single successor can be grouped under a parent node. In this case, the solver
    can compose the semigroup actions of the nodes to compute an action for the whole
    group, solving the data flow problem at the group level and then computing values
    for individual nodes.

    A specific instance of this problem is on the control flow graph, where nodes are
    instructions and parents are basic blocks.
    *)
module type Semigroup_action_problem = sig
  module A : Semigroup_action

  (* XCR lwhite: You are keeping the names very general here, but that makes it less clear
     what you are trying to do. I think it is fine to use names related to basic blocks
     and instructions, to make it clear what is going on. So I'd suggest s/Parent/Block/
     and s/Node/Instruction/.

     nlicker: I do not really want this to be specific to basic blocks/instructions.
     That is the goal of the Cfg_* solvers, which operate specifically on the CFG.
     I have added some comments to clarify the constraints.
   *)

  (** Identifier for a group of nodes with single successors. *)
  module Group : Node_id

  (** Identifier for individual nodes, linking back to their parent group. *)
  module Node : sig
    include Node_id
    val parent : t -> Group.t
  end

  type t

  val entry_groups : t -> Group.Set.t

  val next_group : t -> Group.t -> Group.t list
  val prev_group : t -> Group.t -> Group.t list

  val start_node : t -> Group.t -> Node.t
  val next_node : t -> Node.t -> Node.t option

  val entry : t -> Group.t -> A.S.t

  (* XCR lwhite: Thanks to the rename, the name [kg] no longer
     has an obvious meaning. Perhaps [transfer_function] or
     [action]? *)
  val action : t -> Node.t -> A.G.t
end

(** Data-flow problem explicitly for the cfg. *)
module type Cfg_semigroup_action_problem = sig
  module A : Semigroup_action

  type t

  val cfg : t -> Cfg.t

  val entry : t -> Label.t -> A.S.t

  val action : t -> Inst_id.t -> A.G.t
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

  module Make_backward_noexn_cfg_solver (P: Cfg_semigroup_action_problem): Solver
    with type t := P.t
     and module S := P.A.S
     and module Node := Inst_id
end
