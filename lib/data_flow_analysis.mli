include module type of struct
  include Data_flow_analysis_intf
end

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
