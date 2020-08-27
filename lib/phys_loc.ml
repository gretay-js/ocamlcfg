

module T = struct
  type t =
    | Reg of int
    | Stack of Stack_slot.t

  let compare a b =
    match a, b with
    | Reg a', Reg b' -> a' - b'
    | Reg _, Stack _ -> -1
    | Stack a', Stack b' -> Stack_slot.compare a' b'
    | Stack _, Reg _ -> +1
end

include T
module Set = Set.Make(T)
module Map = Map.Make(T)

let of_reg reg =
  match reg.Reg.loc with
  | Reg.Reg r ->
    Some (Reg r)
  | Reg.Stack (Reg.Local loc) ->
    Some (Stack { Stack_slot.loc; reg_class = Proc.register_class reg })
  | _ ->
    None

let print fmt t =
  match t with
  | Reg r -> Format.fprintf fmt "reg[%d]" r
  | Stack stk -> Stack_slot.print fmt stk
