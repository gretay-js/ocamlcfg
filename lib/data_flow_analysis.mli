
include module type of struct
  include Data_flow_analysis_intf.S
end

module ReachingSpills : sig
  val solve : Cfg.t -> (Label.t, Spill.Set.t) Hashtbl.t
end

module Dominators : sig
  val solve : Cfg.t -> (Label.t, Dom.Set.t) Hashtbl.t
end
