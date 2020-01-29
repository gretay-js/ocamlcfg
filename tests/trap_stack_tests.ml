(* CR mshinwell: Add file header *)

(* CR mshinwell: Make these into expect tests? *)

open Trap_stack

(* CR mshinwell for mshinwell: Read after the trap stack cycle detection has
   been implemented. *)

let () =
  try
    let k = push emp 1 in
    let t1 = pop k in
    let t2 = pop (push k 2) in
    unify t1 t2;
    assert false
  with _ -> ()

let () =
  let t1 = unknown () in
  let t2 = emp in
  unify t1 t2

let () =
  let t1 = unknown () in
  let t2 = push emp 1 in
  unify t1 t2

let () =
  let t1 = push (unknown ()) 1 in
  let t2 = push emp 1 in
  unify t1 t2

let () =
  let t1 = pop (unknown ()) in
  let t2 = pop (push emp 1) in
  unify t1 t2

let () =
  let t1 = pop (unknown ()) in
  let t2 = pop (push (pop (unknown ())) 1) in
  unify t1 t2;
  unify t2 t1

let () =
  let t1 = pop (push (unknown ()) 1) in
  Printf.printf "    t1 is ";
  print t1;
  Printf.printf "rep t1 is ";
  print (rep t1)

let () =
  let t1 = pop (pop (push (unknown ()) 1)) in
  Printf.printf "    t1 is ";
  print t1;
  Printf.printf "rep t1 is ";
  print (rep t1)

let () =
  let k = push emp 1 in
  let t1 = pop k in
  let t2 = pop (push k 2) in
  Printf.printf "    t2 is ";
  print t2;
  Printf.printf "rep of t2 is ";
  print t2;
  Printf.printf "    t1 is ";
  print t1;
  Printf.printf "rep t1 is ";
  print (rep t1);
  Printf.printf "now t2 is ";
  print t2
