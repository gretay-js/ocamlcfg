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

type labelled_insn =
  { label : Label.t;
    insn : Linear.instruction
  }

let labelled_insn_end = { label = -1; insn = Linear.end_instr }

(* CR-someday xclerc: it might make sense to have this utility function
   upstreamed (e.g. next to `Linear.has_fallthrough`), as there is a feeling
   it might get out of sync. *)
let rec has_label (i : Linear.instruction) =
  match i.desc with
  | Lend | Llabel _ ->
    (* CR xclerc for xclerc: does `Lprologue` have a label? *)
    true
  | Ladjust_trap_depth _ -> has_label i.next
  | _ -> false  (* CR mshinwell: Make this match exhaustive *)
