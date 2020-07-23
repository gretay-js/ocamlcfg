
include module type of struct
  include Data_flow_analysis_intf.S
end

module Make_solver (P: Problem) : sig
  (* Functor to build a solver for a data-flow problem. *)
  val solve : P.t -> (P.S.t * P.S.t) P.Node.Map.t
end

module Make_kill_gen_solver (P: KillGenProblem) : sig
  (* Functor to build a solver for a kill-gen problem. *)
  val solve : P.t -> (P.S.t P.Node.Map.t) P.Parent.Map.t
end

module Dominators : sig
  (* Dominator analysis. *)
  val solve : Cfg.t -> Dom.Set.t Label.Map.t
end
