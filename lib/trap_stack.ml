type t = stack ref

and stack =
  | Emp
  | Unknown
  | Push of Label.t * t
  | Pop of t
  | Link of t

let rec rep t =
  match !t with
  | Link s -> rep s
  | Push (h, s) -> ref (Push (h, rep s))
  | Pop s -> (
      let s' = rep s in
      match !s' with
      | Push (_, s1) -> s1
      | _ -> ref (Pop s') )
  | Unknown -> t
  | Emp -> t

exception Unresolved

let top_exn t =
  match !(rep t) with
  | Emp -> None
  | Push (h, _) -> Some h
  | Unknown | Pop _ | Link _ -> raise Unresolved

(** Raises [Unresolved] if t contains any pop or links or Unknown. *)
let rec to_list_exn t =
  match !(rep t) with
  | Emp -> []
  | Push (h, s) ->
      (* This won't terminate s is a cycle back to t. *)
      h :: to_list_exn s
  | Unknown | Pop _ | Link _ -> raise Unresolved

(* Ensure we never create a cycle *)
let rec unify s1 s2 =
  match (!s1, !s2) with
  | Emp, Emp -> s1
  | Unknown, _ ->
      s1 := Link (rep s2);
      s1
  | _, Unknown ->
      s2 := Link (rep s1);
      s2
  | Link s1, Link s2 -> unify s1 s2
  | Link s1, _ ->
      (* check that we are not creating a cycle *)
      unify s1 s2
  | _, Link s2 ->
      (* check that we are not creating a cycle *)
      unify s1 s2
  | Emp, _ | _, Emp ->
      Misc.fatal_error "Malformed trap stack: size mismatch."
  | Push (h1, s1), Push (h2, s2) ->
      if h1 = h2 then unify s1 s2
      else
        Misc.fatal_error
          "Malformed trap stack: mismatched pushtrap handlers."
  | Pop s1, Pop s2 -> unify s1 s2
  | Pop _, Push _ | Push _, Pop _ ->
      Misc.fatal_error
        "Malformed trap stack: mismatched pushtrap or poptrap handlers."
