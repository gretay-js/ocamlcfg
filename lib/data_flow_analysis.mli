
include module type of struct
  include Data_flow_analysis_intf.S
end

module Make_solver (P: Problem) : sig
  (* Functor to build a solver for a data-flow problem.g *)
  val solve : P.t -> (P.S.t * P.S.t) P.Node.Map.t
end

module Make_kill_gen_solver (P: KillGenProblem) : sig
  (* Functor to build a solver for a kill-gen problem. *)
  val solve : P.t -> (P.K.S.t * P.K.S.t) P.Node.Map.t
end

module Make_forward_cfg_solver (P: CfgKillGenProblem) : sig
  (* Functor to build a forward solver on the cfg. *)
  val solve : P.t -> (P.K.S.t * P.K.S.t) Inst_id.Map.t
end

module Make_backward_cfg_solver (P: CfgKillGenProblem) : sig
  (* Functor to build a backward solver on the cfg. *)
  val solve : P.t -> (P.K.S.t * P.K.S.t) Inst_id.Map.t
end

(* Helper to look up an instruction in the cfg. *)
val get_inst
  :  Cfg.t
  -> Inst_id.t
  -> [`Basic of Cfg.basic Cfg.instruction|`Term of Cfg.terminator Cfg.instruction]

(* Returns all the predecessors of an instruction. *)
val get_pred_insts : Cfg.t -> Inst_id.t -> Inst_id.t list
