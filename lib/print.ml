open Cfg

let print_terminator ppf ti =
  Format.fprintf ppf "\n";
  match ti.desc with
  | Branch successors ->
      Format.fprintf ppf "Branch with %d successors:\n"
        (List.length successors);
      List.iter
        (fun (c, l) ->
          match c with
          | Always -> Format.fprintf ppf "goto %d\n" l
          | Test c ->
              Format.fprintf ppf "if %a then goto %d\n" (Printmach.test c)
                ti.arg l)
        successors
  | Switch labels ->
      Format.fprintf ppf "switch %a of\n" Printmach.reg ti.arg.(0);
      for i = 0 to Array.length labels - 1 do
        Format.fprintf ppf "case %d: goto %d\n" i labels.(i)
      done
  | Return -> Format.fprintf ppf "Return\n"
  | Raise _ -> Format.fprintf ppf "Raise\n"
  | Tailcall (Self _) -> Format.fprintf ppf "Tailcall self\n"
  | Tailcall _ -> Format.fprintf ppf "Tailcall\n"

let print_block t ppf label b ~linearize_basic ~linearize_terminator =
  Format.fprintf ppf "\n%d:\n" label;
  let i = List.fold_right linearize_basic b.body Linear.end_instr in
  Printlinear.instr ppf i;
  Format.fprintf ppf "%d: " b.terminator.id;
  print_terminator ppf b.terminator;
  ( try
      let t = linearize_terminator b.terminator in
      Printlinear.instr ppf t
    with _ -> () );
  Format.fprintf ppf "\npredecessors:";
  LabelSet.iter (fun l -> Format.fprintf ppf " %d" l) b.predecessors;
  Format.fprintf ppf "\nsuccessors:";
  List.iter (fun l -> Format.fprintf ppf " %d" l) (successor_labels t b)

let print oc cfg layout ~linearize_basic ~linearize_terminator =
  let ppf = Format.formatter_of_out_channel oc in
  Printf.fprintf oc "\n%s\n" cfg.fun_name;
  Printf.fprintf oc "layout.length=%d\n" (List.length layout);
  Printf.fprintf oc "blocks.length=%d\n" (Hashtbl.length cfg.blocks);
  List.iter
    (fun label ->
      let b = Hashtbl.find cfg.blocks label in
      print_block cfg ppf label b ~linearize_basic ~linearize_terminator)
    layout

let print_dot oc cfg layout ~linearize_basic:_ ~linearize_terminator:_ =
  let name l = Printf.sprintf "\".L%d\"" l in
  let ppf = Format.formatter_of_out_channel oc in
  let print_block_dot label block index =
    let show_index = Option.value index ~default:(-1) in
    Printf.fprintf oc "\n%s [shape=box label=\".L%d:I%d:S%d" (name label)
      label show_index (List.length block.body);

    if !Cfg_builder.dot_show_instr then (
      (* This doesn't work because of special characters like { } that need
         to be escaped. Should use sexp to print, like in Eric's instr_freq. *)
      (* Printf.fprintf oc "\\n";
       * let i = List.fold_right linearize_basic block.body Linear.end_instr in
       * Printlinear.instr ppf i;
       * Format.pp_print_flush ppf () *)
      Printf.fprintf oc "\npreds:";
      LabelSet.iter (Printf.fprintf oc " %d") block.predecessors;
      Printf.fprintf oc "\\n(%d)" block.terminator.id;
      print_terminator ppf block.terminator;
      Format.pp_print_flush ppf () );
    Printf.fprintf oc "\"]\n";

    List.iter
      (fun l -> Printf.fprintf oc "%s->%s\n" (name label) (name l))
      (successor_labels cfg block)
  in
  Printf.fprintf oc "strict digraph \"%s\" {\n" cfg.fun_name;

  (* print all the blocks, even if they don't appear in the layout *)
  List.iteri
    (fun index label ->
      let block = Hashtbl.find cfg.blocks label in
      print_block_dot label block (Some index))
    layout;

  assert (List.length layout <= Hashtbl.length cfg.blocks);
  if List.length layout < Hashtbl.length cfg.blocks then
    Hashtbl.iter
      (fun label block ->
        match List.find_opt (fun lbl -> label = lbl) layout with
        | None -> print_block_dot label block None
        | _ -> ())
      cfg.blocks;
  Printf.fprintf oc "}\n"

let print msg oc cfg layout ~linearize_basic ~linearize_terminator =
  if !Cfg_builder.dot_format then (
    let filename =
      Printf.sprintf "%s%s%s.dot"
        (X86_proc.string_of_symbol "" cfg.fun_name)
        (if msg = "" then "" else ".")
        msg
    in
    Printf.printf "Writing cfg for %s to %s\n" msg filename;
    let dotc = open_out filename in
    print_dot dotc cfg layout ~linearize_basic ~linearize_terminator;
    close_out dotc )
  else (
    Printf.printf "cfg for %s\n" msg;
    print oc cfg layout ~linearize_basic ~linearize_terminator )
