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
end

module Cfg_with_layout = struct
  include Cfg_with_layout

  let eliminate_dead_blocks = Eliminate_dead_blocks.run

  (* eliminate fallthrough implies dead block elimination *)
  let eliminate_fallthrough_blocks = Eliminate_fallthrough_blocks.run

  let of_linear = Linear_to_cfg.run

  let to_linear = Cfg_to_linear.run

  let add_extra_debug = Extra_debug.add
end

let verbose = Cfg.verbose
