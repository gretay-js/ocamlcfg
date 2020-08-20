
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

module Make (Ord: Set.OrderedType) = struct
  module Set = Set.Make(Ord)

  type elt = Ord.t

  type t =
    | Full
    (** Symbolic representation for the set with all elements. *)
    | Partial of Set.t
    (** A set with particular elements. *)

  let singleton (node: elt) = Partial (Set.singleton node)

  let empty = Partial Set.empty

  let full = Full

  let inter a b =
    match a, b with
    | Full, _ -> b
    | Partial _, Full -> a
    | Partial a', Partial b' -> Partial (Set.inter a' b')

  let equal a b =
    match a, b with
    | Partial a', Partial b' -> Set.equal a' b'
    | Partial _, Full -> false
    | Full, Partial _ -> false
    | Full, Full -> true

  let add node = function
    | Full -> Full
    | Partial s -> Partial (Set.add node s)

  let to_set = function
    | Full -> None
    | Partial s -> Some s
end
