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
open Ocamlcfg
open Core

let verbose = ref false

let last_id = ref 0

let names = Hashtbl.create (module String)

let save_names = false

let get_id name =
  if save_names then
    match Hashtbl.find names name with
    | None ->
        Hashtbl.add_exn names ~key:name ~data:!last_id;
        incr last_id;
        !last_id - 1
    | Some id -> id
  else (
    incr last_id;
    !last_id - 1 )

(* Some names come out too long. This function shortens them or depending on
   whether they are write-only, or need to be reused. *)
let get_filename ~name ~title ~sub =
  let filename = sprintf "%s-%s.%s" name title sub in
  if String.length name < 255 then filename
  else
    sprintf "%s-%d-%s.%s" (String.prefix name 200) (get_id name) title sub

let with_outchannel ~name ~title ~sub printer x =
  let filename = get_filename ~name ~title ~sub in
  let out_channel = Out_channel.create filename in
  Misc.try_finally
    (fun () -> printer out_channel x)
    ~always:(fun () -> Out_channel.close out_channel)

let with_ppf ~name ~title ~sub formatter x =
  let filename = get_filename ~name ~title ~sub in
  let out_channel = Out_channel.create filename in
  let ppf = Format.formatter_of_out_channel out_channel in
  Misc.try_finally
    (fun () -> formatter ppf x)
    ~always:(fun () ->
      Format.pp_print_flush ppf ();
      Out_channel.close out_channel)

let report_linear ~name title f =
  with_ppf ~name ~title ~sub:"lin" Printlinear.fundecl f

let report_cfg ~name title cfg =
  with_outchannel ~name ~title ~sub:"lin" Cfg_builder.print cfg

let check_equal f ~new_body ~strict =
  let open Linear in
  let rec equal i1 i2 =
    (* Format.kasprintf prerr_endline "@;%a" Printlinear.instr i1;
     * Format.kasprintf prerr_endline "@;%a" Printlinear.instr i2; *)
    if
      i1.desc = i2.desc
      (* && i1.id = i2.id *)
      && Reg.array_equal i1.arg i2.arg
      && Reg.array_equal i1.res i2.res
      && Reg.Set.equal i1.live i2.live
      && Debuginfo.compare i1.dbg i2.dbg = 0
    then if i1.desc = Lend then true else equal i1.next i2.next
    else (
      Format.kasprintf prerr_endline "Equality failed in %s on:@;%a@;%a"
        f.fun_name Printlinear.instr i1 Printlinear.instr i2;
      false )
  in
  if not (equal f.fun_body new_body) then (
    let name = X86_proc.string_of_symbol "" f.fun_name in
    (* Separate files for before and after to make it easier to diff *)
    report_linear ~name "Before" f;
    report_linear ~name "After" { f with fun_body = new_body };
    if strict then
      failwithf
        "Conversion from linear to cfg and back to linear is not an \
         identity function %s.\n"
        name () )

let print_linear msg f =
  if !verbose then (
    printf "%s processing %s\n" f.Linear.fun_name msg;
    Format.kasprintf prerr_endline "@;%a" Printlinear.fundecl f )

let process transform file =
  let out_filename = file ^ "-new" in
  let open Linear_format in
  restore file
  |> List.map ~f:(function
       | Func d -> Func (transform d)
       | Data d -> Data d)
  |> save out_filename

let print_layout msg layout =
  if !verbose then (
    printf "%s:\n" msg;
    List.iter ~f:(fun lbl -> printf "%d " lbl) layout;
    printf "\n" )

let reorder cfg =
  (* Ensure entry exit invariants *)
  let original_layout = Cfg_builder.get_layout cfg in
  let new_layout =
    List.hd_exn original_layout
    :: List.permute ~random_state:Random.State.default
         (List.tl_exn original_layout)
  in
  print_layout "orig" original_layout;
  print_layout "after reorder:" new_layout;
  Cfg_builder.set_layout cfg new_layout

let transform f ~reorder_blocks ~extra_debug ~validate ~strict =
  print_linear "Before" f;
  let cfg = Cfg_builder.from_linear f ~preserve_orig_labels:false in
  (* eliminate fallthrough implies dead block elimination *)
  if not validate then Cfg_builder.eliminate_fallthrough_blocks cfg;
  let new_cfg = if reorder_blocks then reorder cfg else cfg in
  let new_body = Cfg_builder.to_linear new_cfg ~extra_debug in
  let fnew = { f with fun_body = new_body } in
  if validate then check_equal f ~new_body ~strict;
  print_linear "After" fnew;
  fnew

let main files ~reorder_blocks ~extra_debug ~validate ~strict =
  let transform f =
    transform f ~reorder_blocks ~extra_debug ~validate ~strict
  in
  List.iter files ~f:(process transform)

let main_command =
  Command.basic
    ~summary:"Demo for building CFG from Linear and block reordering."
    ~readme:(fun () -> "")
    Command.Let_syntax.(
      let%map_open v =
        flag "-verbose" ~aliases:[ "-v" ] no_arg ~doc:" verbose"
      and extra_debug =
        flag "-extra-debug" no_arg ~doc:" add extra debug info"
      and reorder_blocks =
        flag "-reorder-random" no_arg ~doc:" randomly reorder blocks"
      and validate =
        flag "-validate" no_arg
          ~doc:" check Linear->CFG->Linear is identity"
      and strict = flag "-strict" no_arg ~doc:" stop if validate fails"
      and files = anon (sequence ("input" %: Filename.arg_type)) in
      verbose := v;
      if !verbose && List.is_empty files then printf "No input files\n";
      if strict && not validate then
        failwith "Option -strict requires -validate";
      if validate then
        if reorder_blocks || extra_debug then
          failwith
            "Options -reorder-blocks and -extra-debug are incompatible \
             with -validate";
      fun () -> main files ~reorder_blocks ~extra_debug ~validate ~strict)

let () = Command.run main_command
