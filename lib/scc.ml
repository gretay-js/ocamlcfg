include Scc_intf

(* SCC implementation based on "On finding the strongly connected components in a
   directed graph" by E. Nuutila, E. Soisalon-Soininen *)
module Make_solver (G: Graph) = struct
  module Node = G.Node

  type node =
    { mutable root: Node.t
    ; mutable in_component: bool
    ; mutable on_stack: bool
    }

  let solve t =
    let components = ref [] in
    let data = ref Node.Map.empty in
    let stk = ref [] in
    let rec visit node =
      match Node.Map.find node !data with
      | info -> info
      | exception Not_found ->
        let info = { root = node; in_component = false; on_stack = false } in
        data := Node.Map.add node info !data;
        let next = G.next t node in
        next |> Node.Set.iter (fun next ->
          let info' = visit next in
          if not info'.in_component && Node.compare info.root info'.root > 0 then
            info.root <- info'.root);
        if Node.compare info.root node = 0 then begin
          match !stk with
          | next :: stk' when Node.compare next node >= 0 ->
            let rec pop stk component =
              match stk with
              | next :: stk' when Node.compare next node >= 0 ->
                (Node.Map.find next !data).in_component <- true;
                pop stk' (next :: component)
              | _ ->
                stk, component
            in
            let stk', component = pop (next :: stk') [node] in
            components := Node.Set.of_list component :: !components;
            stk := stk'
          | _ ->
            (* The node is in its own SCC. *)
            info.in_component <- true;
            if Node.Set.mem node next then
              components := Node.Set.singleton node :: !components
        end else if not info.on_stack then begin
          info.on_stack <- true;
          stk := node :: !stk
        end;
        info
    in
    G.entries t |> Node.Set.iter (fun node -> ignore (visit node));
    !components
end
