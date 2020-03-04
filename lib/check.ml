let equal_op o1 o2 =
  let equal_call c1 c2 =

let equal_basic b1 b2 =
  match b1, b2 with
  | Op o1, Op o2 -> check_equal_opt o1 o2
  | Poptrap, Poptrap
  | Prologue, Prologue
  | Reloadretaddr, Reloadretaddr
    -> Poly.equal b1 b2
  | Call c1, Call c2 ->
    equal_call c1 c2
  | Pushtrap l1, Pushtrap l2 equal_block l1 c1 l2 c2
  | _ -> false

let check_equal_terminator t1 t2 =
  match t1 t2 with
    | Never, Never
    | Always l1
    | Is_even {ifso=t1;ifnot=t2} | Is_true {ifso=t1;ifnot=t2}
    | Float_test of float_test
    | Int_test of int_test
    | Switch of Label.t array
    | Return
    | Raise of Cmm.raise_kind
    | Tailcall of tail_call_operation

let equal_block l1 c1 l2 c2 =
  match (Cfg.get_block c1 l1, Cfg.get_block c2 l2) with
  | Some b1, Some b2 ->
      if not (List.equal check_equal_basic (Cfg.body b1) (Cfg.body b2)) then
        false
      else if
        not (check_equal_terminator (Cfg.terminator b1) (Cfg.terminator b2))
      then false
      else true
  | _ -> false


