(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = stack ref

and h = handler ref

and stack =
  | Emp
  | Unknown
  | Link of t
  | Push of
      { h : h;
        t : t
      }

and handler =
  | Unknown
  | Link of h
  | Label of Label.t

(* rep shortens chains of pop::push *)
let rec rep (t : t) =
  match !t with
  | Link t -> rep t
  | _ -> t

let rec rep_h (h : h) =
  match !h with
  | Link h -> rep_h h
  | _ -> h

let emp = ref Emp

let unknown () = ref (Unknown : stack)

let push t h = ref (Push { h = ref (Label h); t = rep t })

let push_unknown t = ref (Push { h = ref (Unknown : handler); t = rep t })

exception Unresolved

let top_exn t =
  match !(rep t) with
  | Emp -> None
  | Push p -> (
      match !(rep_h p.h) with
      | Label l -> Some l
      | Link _ -> assert false (* removed by rep_h *)
      | Unknown -> raise Unresolved )
  | Link _ -> assert false (* removed by rep *)
  | Unknown -> raise Unresolved

(** Raises [Unresolved] if t contains any pop or Unknown. *)
let rec to_list_exn t =
  match !(rep t) with
  | Emp -> []
  | Push p -> (
      match !(rep_h p.h) with
      | Label l ->
          (* This won't terminate if [t] has a cycle back to [t]. *)
          l :: to_list_exn p.t
      | Link _ -> assert false (* removed by rep_h *)
      | Unknown -> raise Unresolved )
  | Link _ -> assert false (* removed by rep *)
  | Unknown -> raise Unresolved

let rec print_h h =
  match !h with
  | Label l -> Printf.printf "%d" l
  | Link h' ->
      Printf.printf "=";
      print_h h'
  | Unknown -> Printf.printf "?"

let rec print t =
  match !t with
  | Emp -> Printf.printf "emp\n"
  | Unknown -> Printf.printf "??\n"
  | Link s ->
      Printf.printf "=";
      print s
  | Push p ->
      print_h p.h;
      Printf.printf "::";
      print p.t

let print_pair_h msg h1 h2 =
  Printf.printf "%s\n" msg;
  print_h h1;
  print_h h2

let print_pair msg t1 t2 =
  Printf.printf "%s\n" msg;
  print t1;
  print t2

let fail () =
  Misc.fatal_error "Malformed trap stack: mismatched pop/push trap handlers."

let rec unify_h (h1 : h) (h2 : h) =
  match (!h1, !h2) with
  | Link h1, Link h2 -> unify_h h1 h2
  | Link h, _ -> unify_h h h2
  | _, Link h -> unify_h h1 h
  | Unknown, _ -> h1 := Link h2
  | _, Unknown -> h2 := Link h1
  | Label l1, Label l2 ->
      if l1 <> l2 then (
        print_pair_h "handler labels disagree:" h1 h2;
        fail () )

(* If there is a cycle, this won't terminate but that's easy to debug. If
   there is no cycle, it terminates because every step decreases the
   following well-founded order: (1) removes one unknown, (2) transforms one
   pop into push, and there is no rule that creates pop from push, or (3)
   reduces the length of the stack. *)
let rec unify s1 s2 =
  match (!s1, !s2) with
  | Emp, Emp -> ()
  | Link s1, Link s2 -> unify s1 s2
  | Link s, _ -> unify s s2
  | _, Link s -> unify s1 s
  | Unknown, _ -> s1 := Link s2
  | _, Unknown -> s2 := Link s1
  | Push p1, Push p2 ->
      unify_h p1.h p2.h;
      unify p1.t p2.t
  | Emp, _ | _, Emp ->
      print_pair "emp" s1 s2;
      fail ()

(* if t = push ?:: res then pop::t = pop :: push ?:: res = res *)
let pop t =
  let res = unknown () in
  let t' = push_unknown res in
  unify t t';
  res
