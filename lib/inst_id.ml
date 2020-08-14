
module T = struct
  type t
    = Term of Label.t
    | Inst of Label.t * int

  let compare a b =
    match a, b with
    | Term la, Term lb -> Label.compare la lb
    | Term la, Inst (lb, _) ->
      (match Label.compare la lb with
      | 0 -> 1
      | c -> c)
    | Inst (la, _), Term lb ->
      (match Label.compare la lb with
      | 0 -> -1
      | c -> c)
    | Inst (la, ia), Inst (lb, ib) ->
      match Label.compare la lb with
      | 0 -> ia - ib
      | c -> c

  let equal a b = compare a b = 0
end

module Map = Map.Make(T)
module Set = Set.Make(T)
include T

let at_terminator term = Term term

let at_instruction term inst = Inst(term, inst)

let parent = function
  | Inst(p, _) -> p
  | Term p -> p

let get_inst cfg = function
  | Term block ->
    let bb = Cfg.get_block_exn cfg block in
    `Term bb.Cfg.terminator
  | Inst(block, n) ->
    let bb = Cfg.get_block_exn cfg block in
    `Basic (List.nth bb.Cfg.body n)

let get_preceding_terminators cfg block =
  Cfg.get_block_exn cfg block
  |> Cfg.predecessor_labels
  |> List.map (fun l -> Term l)

let get_predecessors_of_inst cfg = function
  | Term block ->
    let bb = Cfg.get_block_exn cfg block in
    (match bb.body with
    | [] -> get_preceding_terminators cfg block
    | insts ->
      [Inst (block, List.length insts - 1)])
  | Inst (block, 0) ->
    get_preceding_terminators cfg block
  | Inst (block, n) ->
    [Inst (block, n - 1)]

let print fmt = function
  | Term block -> Format.fprintf fmt "(%d:term)" block
  | Inst(block, idx) -> Format.fprintf fmt "(%d:%d)" block idx
