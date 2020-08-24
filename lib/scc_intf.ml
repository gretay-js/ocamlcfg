
module type Graph = sig
  module Node : sig
    include Data_flow_analysis.Node_id
    val compare : t -> t -> int
  end

  type t

  val entries : t -> Node.Set.t

  val next : t -> Node.t -> Node.Set.t
end


module type S = sig
  module type Graph = Graph

  module Make_solver (G: Graph) : sig
    val solve : G.t -> G.Node.Set.t list
    (** Returns the list of all SCCs with more than one node or self loops *)
  end
end
