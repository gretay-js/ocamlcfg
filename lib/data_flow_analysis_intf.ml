(* XCR lwhite: [foo_intf] files do not normally contain module definitions. The point of
   an [foo_intf] file is to define module types. It's a separate file because if you
   define the module types in the main [foo] file then you have to repeat them in
   the ml and mli.
   It looks like you've copied the pattern from [cfg_intf], which is also doing slightly
   the wrong thing. I suspect that what you actually want here is to define the various
   module types and then definie a module *type* [S] that contains aliases to all of them

   nlicker: removed S and retained only module types
*)


module type Semilattice = sig
  type t

  val equal : t -> t -> bool
  (** Returns true if two semilattice values are equal. *)

  val lub : t -> t -> t
  (** Least Upper Bound of two values. *)
end

(* XCR lwhite: This seems more general than Kill/Gen. What you have here is a semigroup
     action on a semilattice.
     I'd probably add a module type [Semigroup] and then have something
     like:
     {[ module type Semigroup_action = sig
          module S : Semilattie
          module A : Semigroup
          val apply : S.t -> A.t -> S.t
        end ]} *)
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

(** Solution to a data flow problem. *)
type 's solution = { sol_in: 's; sol_out: 's }

(** Solver for a data flow problem. *)
module type Solver = sig
  (** Type of the input to the problem *)
  type t

  (** Type of the semilattice. *)
  module S : Semilattice

  (** Type of the nodes. *)
  module Node : Node_id

  val solve : t -> S.t solution Node.Map.t
  (** Solver method *)
end
