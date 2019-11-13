type t = stack ref

and stack =
  | Emp
  | Unknown
  | Push of Label.t * t
  | Pop of t

(* CR-soon gyorsh: we could represent adjust trap with negative delta as pop
   and *)

(* CR-soon gyorsh: test it *)
(* rep that shortens chains of pop;push *)
let rec rep t =
  match !t with
  | Unknown -> t
  | Emp -> t
  | Push (_, s) ->
      s := !(rep s);
      t
  | Pop s -> (
      let s' = rep s in
      match !s' with
      | Push (_, s1) -> s1
      | _ ->
          s := !s';
          t )

let emp = ref Emp

let unknown () = ref Unknown

let pop t = rep (ref (Pop t))

let push t h = ref (Push (h, rep t))

exception Unresolved

let top_exn t =
  match !(rep t) with
  | Emp -> None
  | Push (h, _) -> Some h
  | Unknown | Pop _ -> raise Unresolved

(** Raises [Unresolved] if t contains any pop or Unknown. *)
let rec to_list_exn t =
  match !(rep t) with
  | Emp -> []
  | Push (h, s) ->
      (* This won't terminate s is a cycle back to t. *)
      h :: to_list_exn s
  | Unknown | Pop _ -> raise Unresolved

let rec print t =
  match !t with
  | Emp -> Printf.printf "emp\n"
  | Unknown -> Printf.printf "unknown\n"
  | Push (h, s) ->
      Printf.printf "%d::" h;
      print s
  | Pop s ->
      Printf.printf "pop::";
      print s

let fail msg s1 s2 =
  Printf.printf "%s\n" msg;
  print s1;
  print s2;
  Misc.fatal_error "Malformed trap stack: mismatched pop/push trap handlers."

(* Ensure we never create a cycle *)
let rec unify s1 s2 =
  match (!s1, !s2) with
  | Emp, Emp -> ()
  | Unknown, Unknown -> ()
  | Unknown, _ -> s1 := !(rep s2)
  | _, Unknown -> s2 := !(rep s1)
  | Push (h1, s1), Push (h2, s2) ->
      if h1 = h2 then unify s1 s2 else fail "push/push" s1 s2
  | Pop s1, Pop s2 -> unify s1 s2
  | Pop s, _ -> (
      match !s with
      | Push (_, s') -> unify s' s2
      | _ -> fail "pop/" s1 s2 )
  | _, Pop s -> (
      match !s with
      | Push (_, s') -> unify s1 s'
      | _ -> fail "/pop" s1 s2 )
  | Emp, _ | _, Emp -> fail "emp" s1 s2
