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
[@@@ocaml.warning "+a-4-30-40-41-42-44-45"]

let add_discriminator dbg file d =
  Debuginfo.make ~file ~line:d ~discriminator:d |> Debuginfo.concat dbg

(* CR gyorsh: for iterative fdo, renumber the instructions with fresh linear
   ids, as we may have removed some instructions and introduced new ones,
   for example when a fallthrough turned into a jump after reorder.*)
(* if extra_debug then add_linear_discriminators fnew else fnew *)
let rec add_linear_discriminator (i : Linear.instruction) file d =
  match i.desc with
  | Lend -> { i with next = i.next }
  | Llabel _ | Ladjust_trap_depth _ ->
      { i with next = add_linear_discriminator i.next file d }
  | _ ->
      {
        i with
        dbg = add_discriminator i.dbg file d;
        next = add_linear_discriminator i.next file (d + 1);
      }

(* CR gyorsh: This is the only machine dependent part. owee parser doesn't
   know how to handle /% that may appear in a function name, for example an
   Int operator. *)
let to_symbol name =
  let symbol_prefix =
    if X86_proc.system = X86_proc.S_macosx then "_" else ""
  in
  X86_proc.string_of_symbol symbol_prefix name

(* let to_symbol name = name *)

let linear_ext = ".cmir-linear"

let _add_linear_discriminators (f : Linear.fundecl) entry_id =
  (* Best guess for filename based on compilation unit name, because dwarf
     format (and assembler) require it, but only the line number or
     discriminator really matter, and it is per function. *)
  let file = to_symbol f.fun_name ^ linear_ext in
  {
    f with
    fun_dbg = add_discriminator f.fun_dbg file entry_id;
    fun_body = add_linear_discriminator f.fun_body file entry_id;
  }

let get_linear_file fun_name = to_symbol fun_name ^ linear_ext

let rec remove_discriminator = function
  | [] -> []
  | item :: t ->
      if Filename.extension item.Debuginfo.dinfo_file = linear_ext then t
      else item :: remove_discriminator t

let rec remove_linear_discriminator i =
  let open Linear in
  match i.desc with
  | Lend -> i
  | _ ->
      {
        i with
        dbg = remove_discriminator i.dbg;
        next = remove_linear_discriminator i.next;
      }

let remove_linear_discriminators f =
  let open Linear in
  {
    f with
    fun_dbg = remove_discriminator f.fun_dbg;
    fun_body = remove_linear_discriminator f.fun_body;
  }
