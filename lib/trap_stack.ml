[@@@ocaml.warning "+a-30-40-41-42"]


module type D =
sig
  type t
  val equal : t -> t -> bool
  val to_string : t -> string
end

module type S =
sig
  type d
  type t
  exception Unresolved
  val empty : t
  val unknown : unit -> t
  val pop : t -> t
  val push : t -> d -> t
  val to_list_exn : t -> d list
  val top_exn : t -> d option
  val unify : t -> t -> unit
  val print : t -> unit
  val print_pair : string -> t -> t -> unit
end

module Make(D: D) =
struct
  type d = D.t
  type t = stack ref
  and h =  handler ref

  (* CR mshinwell for mshinwell: Re-read once cycle check implemented *)

  (* XCR mshinwell: As per conversation, add fast check for cycles, with
     the necessary identities on the nodes that require them.  (Perhaps using
     phys_equal might suffice for the identity, so long as the structure being
     tested is mutable.) *)

  (* XCR mshinwell: I suggest renaming Emp -> Empty (likewise for the .mli),
     it isn't obvious from the outside what "emp" might mean *)
  and stack =
    | Empty
    | Unknown
    | Link of t
    | Push of
        { h : h;
          t : t
        }

  and handler =
    | Unknown
    | Link of h
    | Label of d

  (* XCR xclerc: this comment looks outdated *)
  let rec rep (t : t) =
    match !t with
    | Link t -> rep t
    | Empty | Unknown | Push _ -> t

  let rec rep_h (h : h) =
    match !h with
    | Link h -> rep_h h
    | Unknown | Label _ -> h

  (* XCR xclerc: this seems quite dangerous; I would change the type of
     `emp` to `unit -> stack`.

     gyorsh: the ref is not exposed at the interface,
             as its type [t] is abstract. how is it dangerous? *)
  let empty = ref Empty

  let unknown () = ref (Unknown : stack)

  let push t h = ref (Push { h = ref (Label h); t = rep t })

  let push_unknown t = ref (Push { h = ref (Unknown : handler); t = rep t })

  exception Unresolved

  (* CR-someday xclerc: if performance is not a concern, we may want to
   * express `top_exn` in terms of `to_list` (to avoid code duplication).

     gyorsh:
     it is not the same because to_list_exn is recursive
     so it ensures no Uknowns anywhere, whereas top_exn only checks
     that the top of the stack is resolved.
     In any case, to_list_exn is not used anywhere any more, so I removed it.
  *)
  let top_exn t =
    match !(rep t) with
    | Empty -> None
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
    | Empty -> []
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
    | Label l -> Printf.printf "%s" (D.to_string l)
    | Link h' ->
      Printf.printf "=";
      print_h h'
    | Unknown -> Printf.printf "?"

  let rec print t =
    match !t with
    | Empty -> Printf.printf "empty\n"
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

  let link_h ~(src:h) ~(dst:h) =
    assert (!src = Unknown);
    (* check that there is no path from dst to link.
       it guarantees that the link from src to dst that we install is
       not going to close a cycle, which will cause non-termination
       of other operations on the stack. *)
    let rec loop cur =
      if cur == src then
        Misc.fatal_error "Trap_stack.unify created a cycle.";
      match !cur with
      | Unknown -> ()
      | Label _ -> ()
      | Link h -> loop h
    in
    loop dst;
    (* create  a link from src to dst. *)
    src := Link dst

  let rec unify_h (h1 : h) (h2 : h) =
    match (!h1, !h2) with
    | Link h1, Link h2 -> unify_h h1 h2
    | Link h, _ -> unify_h h h2
    | _, Link h -> unify_h h1 h
    | Unknown, _ -> link_h ~src:h1 ~dst:h2
    | _, Unknown -> link_h ~src:h2 ~dst:h1
    | Label l1, Label l2 ->
      (* XCR xclerc: I would rather use Label.equal (defensive). *)
      if not (D.equal l1 l2) then (
        print_pair_h "handler labels disagree:" h1 h2;
        fail () )

  let link ~(src:t) ~(dst:t) =
    assert (!src = Unknown);
    (* check that there is no path from dst to link.
       it guarantees that the link from src to dst that we install is
       not going to close a cycle, which will cause non-termination
       of other operations on the stack. *)
    let rec loop cur =
      if cur == src then
        Misc.fatal_error "Trap_stack.unify created a cycle.";
      match !cur with
      | Unknown -> ()
      | Empty -> ()
      | Link t -> loop t
      | Push {h=_;t;} -> loop t
    in
    loop dst;
    (* create  a link from src to dst. *)
    src := Link dst

  (* XCR-someday xclerc: that might be easy to debug, but at the same time
   * it might cause problems e.g. in a CI context. *)
  (* If there is a cycle, this won't terminate but that's easy to debug. If
     there is no cycle, it terminates because every step decreases the
     following well-founded order: (1) removes one unknown, (2) transforms one
     pop into push, and there is no rule that creates pop from push, or (3)
     reduces the length of the stack. *)
  let rec unify s1 s2 =
    match (!s1, !s2) with
    | Empty, Empty -> ()
    | Link s1, Link s2 -> unify s1 s2
    | Link s, _ -> unify s s2
    | _, Link s -> unify s1 s
    | Unknown, _ -> link ~src:s1 ~dst:s2
    | _, Unknown -> link ~src:s2 ~dst:s1
    | Push p1, Push p2 ->
      unify_h p1.h p2.h;
      unify p1.t p2.t
    | Empty, _ | _, Empty ->
      print_pair "empty" s1 s2;
      fail ()

  (* if t = push ?:: res then pop::t = pop :: push ?:: res = res *)
  let pop t =
    let res = unknown () in
    let t' = push_unknown res in
    unify t t';
    res
end
