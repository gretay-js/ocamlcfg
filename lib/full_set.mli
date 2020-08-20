
module type S = sig
  type elt

  type t

  val singleton : elt -> t

  val empty : t

  val full : t

  val inter : t -> t -> t

  val equal : t -> t -> bool

  val add : elt -> t -> t

  module Set : Set.S with type elt := elt

  val to_set : t -> Set.t option
end

module Make (Ord: Set.OrderedType) : S with type elt := Ord.t
