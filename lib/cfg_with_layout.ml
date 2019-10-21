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

type t = {
  cfg : Cfg.t;
  mutable layout : Label.t list;
  mutable trap_depths : int Label.Tbl.t;
  mutable trap_labels : Label.t Label.Tbl.t;
  preserve_orig_labels : bool;
  mutable new_labels : Label.Set.t;
}

let create cfg ~layout ~trap_depths ~trap_labels ~preserve_orig_labels
      ~new_labels =
  { cfg;
    layout;
    trap_depths;
    trap_labels;
    preserve_orig_labels;
    new_labels;
  }

let cfg t = t.cfg
let layout t = t.layout
let trap_depths t = t.trap_depths
let trap_labels t = t.trap_labels
let preserve_orig_labels t = t.preserve_orig_labels
let new_labels t = t.new_labels

let set_layout t ~layout = t.layout <- layout

let filter_trap_labels t ~f =
  Label.Tbl.filter_map_inplace (fun _trap_handler_lbl pushtrap_lbl ->
      if f ~pushtrap_lbl then Some pushtrap_lbl
      else None)
    t.trap_labels

let remove_from_trap_depths t label =
  if not (Label.Tbl.mem t.trap_depths label) then begin
    Misc.fatal_errorf "No trap depth was registered for label %a"
      Label.print label
  end;
  Label.Tbl.remove t.trap_depths label

let remove_from_new_labels t label =
  t.new_labels <- Label.Set.remove label t.new_labels

let is_trap_handler t label =
  Label.Tbl.mem t.trap_labels label