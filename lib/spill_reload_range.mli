

module Spill_sink_solver : sig
  val solve : Cfg.t -> Inst_id.Set.t Data_flow_analysis.solution Inst_id.Map.t
end

module Reload_hoist_solver : sig
  val solve : Cfg.t -> Inst_id.Set.t Data_flow_analysis.solution Inst_id.Map.t
end
