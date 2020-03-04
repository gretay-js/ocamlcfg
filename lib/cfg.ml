(* XCR mshinwell: Discuss licence, ensure headers are consistent and check
   that author attributions are correct. *)

[@@@ocaml.warning "+a-30-40-41-42"]

let verbose = ref false

include Cfg_intf.S

type basic_block =
  { start : Label.t;
    mutable body : basic instruction list;
    mutable terminator : terminator instruction;
    mutable predecessors : Label.Set.t;
    trap_depth : int;
    mutable exns : Label.Set.t;
    mutable can_raise : bool;
    mutable can_raise_interproc : bool;
    mutable is_trap_handler : bool;
    mutable dead : bool
  }

type t =
  { blocks : basic_block Label.Tbl.t;
    fun_name : string;
    entry_label : Label.t;
    mutable fun_tailrec_entry_point_label : Label.t
  }

let create ~fun_name ~fun_tailrec_entry_point_label =
  { fun_name;
    entry_label = 1;
    blocks = Label.Tbl.create 31;
    fun_tailrec_entry_point_label
  }

let mem_block t label = Label.Tbl.mem t.blocks label

let successor_labels_normal t ti =
  match ti.desc with
  | Tailcall (Self _) -> Label.Set.singleton t.fun_tailrec_entry_point_label
  | Switch labels -> Array.to_seq labels |> Label.Set.of_seq
  | Return | Raise _ | Tailcall (Func _) -> Label.Set.empty
  | Never -> Label.Set.empty
  | Always l -> Label.Set.singleton l
  | Is_even { ifso; ifnot } | Is_true { ifso; ifnot } ->
      Label.Set.singleton ifso |> Label.Set.add ifnot
  | Float_test { lt; gt; eq; uo } ->
      Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
      |> Label.Set.add uo
  | Int_test { lt; gt; eq; imm = _; is_signed = _ } ->
      Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq

let successor_labels t ~normal ~exn block =
  (* XCR mshinwell: We need to resolve or defer all CRs before this can be
     used in production---what is happening with this one?

     gyorsh: This should have been a comment, not a CR, but it is not
     relevant any more in the new representation. I removed the comment. *)
  match (normal, exn) with
  | false, false -> Label.Set.empty
  | true, false -> successor_labels_normal t block.terminator
  | false, true -> block.exns
  | true, true ->
      Label.Set.union block.exns (successor_labels_normal t block.terminator)

let predecessor_labels block = Label.Set.elements block.predecessors

let replace_successor_labels t ~normal ~exn block ~f =
  (* Check that the new labels are in [t] *)
  let f src =
    let dst = f src in
    if not (mem_block t dst) then
      Misc.fatal_errorf
        "Cfg.replace_successor_labels: \n\
         new successor %d not found in the cfg" dst;
    dst
  in
  if exn then block.exns <- Label.Set.map f block.exns;
  if normal then
    let desc =
      match block.terminator.desc with
      | Never -> Never
      | Always l -> Always (f l)
      | Is_even { ifso; ifnot } -> Is_even { ifso = f ifso; ifnot = f ifnot }
      | Is_true { ifso; ifnot } -> Is_true { ifso = f ifso; ifnot = f ifnot }
      | Int_test { lt; eq; gt; is_signed; imm } ->
          Int_test { lt = f lt; eq = f eq; gt = f gt; is_signed; imm }
      | Float_test { lt; eq; gt; uo } ->
          Float_test { lt = f lt; eq = f eq; gt = f gt; uo = f uo }
      | Switch labels -> Switch (Array.map f labels)
      | Tailcall (Self _) ->
          (* XCR mshinwell: It seems odd that this function will change the
             entry point label in [t] no matter which [block] we have... In
             fact, when this function lived in disconnect_block.ml, there was
             the following check: assert (Label.equal being_disconnected
             cfg.fun_tailrec_entry_point_label);

             gyorsh: the check is still here, when [f] is applied to
             t.fun_tailrec_entry_point_label and [f] itself is defined in
             disconnect_block.ml as before. *)
          t.fun_tailrec_entry_point_label <-
            f t.fun_tailrec_entry_point_label;
          block.terminator.desc
      | Return | Raise _ | Tailcall (Func _) -> block.terminator.desc
    in
    block.terminator <- { block.terminator with desc }

let remove_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found ->
      Misc.fatal_errorf "Cfg.remove_block: block %d not found" label
  | _ -> Label.Tbl.remove t.blocks label

let get_block t label = Label.Tbl.find_opt t.blocks label

let get_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found -> Misc.fatal_errorf "Block %d not found" label
  | block -> block

let fun_name t = t.fun_name

let entry_label t = t.entry_label

let fun_tailrec_entry_point_label t = t.fun_tailrec_entry_point_label

let set_fun_tailrec_entry_point_label t label =
  if not (mem_block t label) then
    Misc.fatal_errorf
      "Cfg.set_fun_tailrec_entry_point_label: \n\
       label %d not found in the cfg" label;
  t.fun_tailrec_entry_point_label <- label

let iter_blocks t ~f = Label.Tbl.iter f t.blocks

let equal t1 t2 =
  if (Label.Tbl.length t1.blocks) <> (Label.Tbl.length t2.blocks) then false
  else if not (Label.equal t1.entry t2.entry) then false
  else (
    let h = Label.Tbl.create (Label.Tbl.length t1.blocks) in
    let s = Label.Set.empty in
    let rec block_eq b1 b2 =
      match Label.Tbl.find_opt h b1.start with
      | Some l -> if not (Label.equal l b2.start) then false
      | None ->
        Labels
        if _labels b1.start
           not Label.equal b1.start b2.start then false
      else

    in

    block_equal (t1.entry) (t2.entry)

  else


    Label.Tbl.iter  (fun l1 b1 ->
      match Label.Tbl.find_opt t2.blocks l1 with
      | None -> false
      | Some b2 ->
    )
      t1.blocks
  )


(* XCR mshinwell: Rename to [can_raise_interproc]?

   gyorsh: removed this function, we don't seem to use it anywhere. *)

(* Printing for debug *)

(* The next 2 functions are copied almost as is from asmcomp/printmach.ml
   because there is no interface to call them. Eventually this won't be
   needed when we change cfg to have its own types rather than referring back
   to mach and cmm. *)
(* CR-someday gyorsh: implement desc printing, and args/res/dbg, etc,
   properly, with regs, use the dreaded Format. *)

let intcomp (comp : Mach.integer_comparison) =
  match comp with
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.integer_comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.integer_comparison c)

let intop (op : Mach.integer_operation) =
  match op with
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh -> " *h "
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior -> " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Icomp cmp -> intcomp cmp
  | Icheckbound _ -> assert false

let print_op oc = function
  | Move -> Printf.fprintf oc "mov"
  | Spill -> Printf.fprintf oc "spill"
  | Reload -> Printf.fprintf oc "reload"
  | Const_int n -> Printf.fprintf oc "const_int %nd" n
  | Const_float f -> Printf.fprintf oc "const_float %Ld" f
  | Const_symbol s -> Printf.fprintf oc "const_symbol %s" s
  | Stackoffset n -> Printf.fprintf oc "stackoffset %d" n
  | Load _ -> Printf.fprintf oc "load"
  | Store _ -> Printf.fprintf oc "store"
  | Intop op -> Printf.fprintf oc "intop %s" (intop op)
  | Intop_imm (op, n) -> Printf.fprintf oc "intop %s %d" (intop op) n
  | Negf -> Printf.fprintf oc "negf"
  | Absf -> Printf.fprintf oc "absf"
  | Addf -> Printf.fprintf oc "addf"
  | Subf -> Printf.fprintf oc "subf"
  | Mulf -> Printf.fprintf oc "mulf"
  | Divf -> Printf.fprintf oc "divf"
  | Floatofint -> Printf.fprintf oc "floattoint"
  | Intoffloat -> Printf.fprintf oc "intoffloat"
  | Specific _ -> Printf.fprintf oc "specific"
  | Name_for_debugger _ -> Printf.fprintf oc "name_for_debugger"

let print_call oc = function
  | P prim_call -> (
      match prim_call with
      | External { func_symbol : string; _ } ->
          Printf.fprintf oc "external %s" func_symbol
      | Alloc { bytes : int; _ } -> Printf.fprintf oc "alloc %d" bytes
      | Checkbound _ -> Printf.fprintf oc "checkbound" )
  | F func_call -> (
      match func_call with
      | Indirect _ -> Printf.fprintf oc "indirect"
      | Direct { func_symbol : string; _ } ->
          Printf.fprintf oc "direct %s" func_symbol )

let print_basic oc i =
  Printf.fprintf oc "%d: " i.id;
  match i.desc with
  | Op op -> print_op oc op
  | Call call ->
      Printf.fprintf oc "Call ";
      print_call oc call
  | Reloadretaddr -> Printf.fprintf oc "Reloadretaddr"
  | Pushtrap { lbl_handler } ->
      Printf.fprintf oc "Pushtrap handler=%d" lbl_handler
  | Poptrap -> Printf.fprintf oc "Poptrap"
  | Prologue -> Printf.fprintf oc "Prologue"

let print_terminator oc ?(sep = "\n") ti =
  Printf.fprintf oc "%d: " ti.id;
  match ti.desc with
  | Never -> Printf.fprintf oc "deadend%s" sep
  | Always l -> Printf.fprintf oc "goto %d%s" l sep
  | Is_even { ifso; ifnot } ->
      Printf.fprintf oc "if even goto %d%sif odd goto %d%s" ifso sep ifnot
        sep
  | Is_true { ifso; ifnot } ->
      Printf.fprintf oc "if true goto %d%sif false goto %d%s" ifso sep ifnot
        sep
  | Float_test { lt; eq; gt; uo } ->
      Printf.fprintf oc "if < goto %d%s" lt sep;
      Printf.fprintf oc "if = goto %d%s" eq sep;
      Printf.fprintf oc "if > goto %d%s" gt sep;
      Printf.fprintf oc "if uo goto %d%s" uo sep
  | Int_test { lt; eq; gt; is_signed; imm } ->
      let cmp =
        Printf.sprintf " %s%s"
          (if is_signed then "s" else "u")
          ( match imm with
          | None -> ""
          | Some i -> " " ^ Int.to_string i )
      in
      Printf.fprintf oc "if <%s goto %d%s" cmp lt sep;
      Printf.fprintf oc "if =%s goto %d%s" cmp eq sep;
      Printf.fprintf oc "if >%s goto %d%s" cmp gt sep
  | Switch labels ->
      Printf.fprintf oc "switch%s" sep;
      for i = 0 to Array.length labels - 1 do
        Printf.fprintf oc "case %d: goto %d%s" i labels.(i) sep
      done
  | Return -> Printf.fprintf oc "Return%s" sep
  | Raise _ -> Printf.fprintf oc "Raise%s" sep
  | Tailcall (Self _) -> Printf.fprintf oc "Tailcall self%s" sep
  | Tailcall (Func _) -> Printf.fprintf oc "Tailcall%s" sep
