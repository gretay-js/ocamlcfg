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

module Cfg = struct
  include Cfg

  module Basic_block = struct
    type t = basic_block

    let start t = t.start
    let body t = t.body
    let terminator t = t.terminator
    let predecessors t = t.predecessors
  end

  let iter_blocks t ~f = Label.Tbl.iter f t.blocks
  let fun_name t = t.fun_name
  let entry_label t = t.entry_label
  let fun_tailrec_entry_point_label t = t.fun_tailrec_entry_point_label
end

module Cfg_with_layout = Cfg_with_layout
module Eliminate_dead_blocks = Eliminate_dead_blocks
module Eliminate_fallthrough_blocks = Eliminate_fallthrough_blocks