(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*                         based on the work of                           *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-soon gyorsh: store label after separately and update after reordering. *)
include Cfg_intf.S

(* CR-soon gyorsh: Switch has successors but currently no way to attach
   User_data to them. Can be fixed by translating Switch to Branch. *)

type basic_block =
  { start : Label.t;
    mutable body : basic instruction list;
    mutable terminator : terminator instruction;
    mutable predecessors : Label.Set.t
  }

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

let successors t block =
  match block.terminator.desc with
  | Branch successors -> successors
  | Tailcall (Self _) -> [(Always, t.fun_tailrec_entry_point_label)]
  | Switch labels ->
      Array.mapi
        (fun i label -> (Test (Iinttest_imm (Iunsigned Ceq, i)), label))
        labels
      |> Array.to_list
  | Return | Raise _ | Tailcall _ -> []

let successor_labels t block =
  let _, labels = List.split (successors t block) in
  labels

let mem_block t label = Label.Tbl.mem t.blocks label

let get_and_remove_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found -> Misc.fatal_errorf "Block %d not found" label
  | block ->
      Label.Tbl.remove t.blocks label;
      block

let get_block t label = Label.Tbl.find_opt t.blocks label

let get_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found -> Misc.fatal_errorf "Block %d not found" label
  | block -> block

let fun_name t = t.fun_name

let entry_label t = t.entry_label

let fun_tailrec_entry_point_label t = t.fun_tailrec_entry_point_label

let set_fun_tailrec_entry_point_label t label =
  t.fun_tailrec_entry_point_label <- label
