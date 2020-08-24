
type t = Inst_id.Set.t Data_flow_analysis.solution Inst_id.Map.t

val solve_spills : Cfg.t -> t

val solve_reloads : Cfg.t -> t
