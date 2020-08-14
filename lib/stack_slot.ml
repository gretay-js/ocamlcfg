
(** Identifier for a stack slot. *)
module T = struct
  type t = { loc: int; reg_class: int }

  let compare a b =
    match compare a.reg_class b.reg_class with
    | 0 -> compare a.loc b.loc
    | c -> c
end

include T
module Map = Map.Make(T)
module Set = Set.Make(T)

let of_reg reg =
  match reg.Reg.loc with
  | Reg.Stack (Reg.Local loc) ->
    Some { loc; reg_class = Proc.register_class reg }
  | _ ->
    None

let print fmt { loc; reg_class } =
  Format.fprintf fmt "stk[%d:%d]" loc reg_class
