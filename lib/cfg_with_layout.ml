[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  { cfg : Cfg.t;
    mutable layout : Label.t list;
    mutable new_labels : Label.Set.t;
    preserve_orig_labels : bool
  }

let create cfg ~layout ~preserve_orig_labels ~new_labels =
  { cfg; layout; new_labels; preserve_orig_labels }

let cfg t = t.cfg

let layout t = t.layout

let preserve_orig_labels t = t.preserve_orig_labels

let new_labels t = t.new_labels

let set_layout t layout =
  let cur_layout = Label.Set.of_list t.layout in
  let new_layout = Label.Set.of_list layout in
  if
    not
      ( Label.Set.equal cur_layout new_layout
      && Label.equal (List.hd layout) t.cfg.entry_label )
  then
    Misc.fatal_error
      "Cfg set_layout: new layout is not a permutation of the current \
       layout, or first label is not entry";
  t.layout <- layout

let remove_block t label =
  Cfg.remove_block_exn t.cfg label;
  t.layout <- List.filter (fun l -> not (Label.equal l label)) t.layout;
  t.new_labels <- Label.Set.remove label t.new_labels

let split_edge t ~start_edge ~end_edge =
  let label, id = Cfg.split_edge_exn t.cfg ~start_edge ~end_edge in
  let rec add_before_end labels =
    match labels with
    | [] -> Misc.fatal_error "Cfg.split_edge: end edge not in layout"
    | l :: _ when Label.equal l end_edge -> label :: labels
    | l :: labels' -> l :: add_before_end labels'
  in
  t.layout <- add_before_end t.layout;
  t.new_labels <- Label.Set.add label t.new_labels;
  label, id

let is_trap_handler t label =
  let block = Cfg.get_block_exn t.cfg label in
  block.is_trap_handler

(* Printing utilities for debug *)

let print t oc msg =
  Format.fprintf oc "cfg for %s\n" msg;
  Format.fprintf oc "%s\n" t.cfg.fun_name;
  Format.fprintf oc "layout.length=%d\n" (List.length t.layout);
  Format.fprintf oc "blocks.length=%d\n" (Label.Tbl.length t.cfg.blocks);
  let print_block label =
    let block = Label.Tbl.find t.cfg.blocks label in
    Format.fprintf oc "\n%d:\n" label;
    List.iter (fun inst -> Format.fprintf oc "  %a\n" Cfg.print_basic inst) block.body;
    Format.fprintf oc "  ";
    Cfg.print_terminator oc block.terminator;
    Format.fprintf oc "\npredecessors:";
    Label.Set.iter (Format.fprintf oc " %d") block.predecessors;
    Format.fprintf oc "\nsuccessors:";
    Label.Set.iter (Format.fprintf oc " %d")
      (Cfg.successor_labels ~normal:true ~exn:false t.cfg block);
    Format.fprintf oc "\nexn-successors:";
    Label.Set.iter (Format.fprintf oc " %d")
      (Cfg.successor_labels ~normal:false ~exn:true t.cfg block)
  in
  List.iter print_block t.layout;
  Format.fprintf oc "\n"

let print_dot t ?(show_instr = true) ?(show_exn = true) ?annotate_block
    ?annotate_succ oc =
  Format.fprintf oc "strict digraph \"%s\" {\n" t.cfg.fun_name;
  let annotate_block label =
    match annotate_block with
    | None -> ""
    | Some f -> Format.sprintf "\n%s" (f label)
  in
  let annotate_succ l1 l2 =
    match annotate_succ with
    | None -> ""
    | Some f -> Format.sprintf " label=\"%s\"" (f l1 l2)
  in
  let print_block_dot label (block : Cfg.basic_block) index =
    let name l = Printf.sprintf "\".L%d\"" l in
    let show_index = Option.value index ~default:(-1) in
    Format.fprintf oc "\n%s [shape=box label=\".L%d:I%d:S%d%s%s" (name label)
      label show_index (List.length block.body)
      (if block.is_trap_handler then ":eh" else "")
      (annotate_block label);
    if show_instr then (
      (* CR-someday gyorsh: Printing instruction using Printlinear doesn't
         work because of special characters like { } that need to be escaped.
         Should use sexp to print or implement a special printer. *)
      Format.fprintf oc "\npreds:";
      Label.Set.iter (Format.fprintf oc " %d") block.predecessors;
      Format.fprintf oc "\\l";
      List.iter
        (fun i ->
          Cfg.print_basic oc i;
          Format.fprintf oc "\\l")
        block.body;
      Cfg.print_terminator oc ~sep:"\\l" block.terminator;
      Format.fprintf oc "\\l" );
    Format.fprintf oc "\"]\n";
    Label.Set.iter
      (fun l ->
        Format.fprintf oc "%s->%s[%s]\n" (name label) (name l)
          (annotate_succ label l))
      (Cfg.successor_labels ~normal:true ~exn:false t.cfg block);
    if show_exn then (
      Label.Set.iter
        (fun l ->
          Format.fprintf oc "%s->%s [style=dashed %s]\n" (name label)
            (name l) (annotate_succ label l))
        (Cfg.successor_labels ~normal:false ~exn:true t.cfg block);
      if block.can_raise_interproc then
        Format.fprintf oc "%s->%s [style=dashed]\n" (name label)
          "placeholder" )
  in
  (* print all the blocks, even if they don't appear in the layout *)
  List.iteri
    (fun index label ->
      let block = Label.Tbl.find t.cfg.blocks label in
      print_block_dot label block (Some index))
    t.layout;
  if List.length t.layout < Label.Tbl.length t.cfg.blocks then
    Label.Tbl.iter
      (fun label block ->
        match List.find_opt (fun lbl -> Label.equal label lbl) t.layout with
        | None -> print_block_dot label block None
        | _ -> ())
      t.cfg.blocks;
  Format.fprintf oc "}\n"

let save_as_dot t ?show_instr ?show_exn ?annotate_block ?annotate_succ msg =
  let filename =
    Format.sprintf "%s%s%s.dot"
      (* some of all the special characters that confuse assemblers also
         confuse dot. get rid of them.*)
      (X86_proc.string_of_symbol "" t.cfg.fun_name)
      (if msg = "" then "" else ".")
      msg
  in
  if !Cfg.verbose then
    Format.printf "Writing cfg for %s to %s\n" msg filename;
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  Misc.try_finally
    (fun () ->
      print_dot t ?show_instr ?show_exn ?annotate_block ?annotate_succ fmt)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun _exn -> Misc.remove_file filename)
