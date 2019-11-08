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

val verbose : bool ref

include module type of struct
  include Cfg_intf.S
end

type basic_block =
  { (* CR mshinwell: Put the trap stack information here as discussed. Add
       explicit successors for blocks containing instructions that can raise.
       Enforce the invariant that a block with an exception successor edge
       (which must always be to the label on the head of the trap stack) must
       have the same trap stack throughout its execution. *)
    start : Label.t;
    mutable body : basic instruction list;
    mutable terminator : terminator instruction;
    mutable predecessors : Label.Set.t
  }

(** Control Flow Graph of a function. *)
type t = private
  { blocks : basic_block Label.Tbl.t;  (** Map from labels to blocks *)
    fun_name : string;  (** Function name, used for printing messages *)
    entry_label : Label.t;
        (** This label must be the first in all layouts of this cfg. *)
    mutable fun_tailrec_entry_point_label : Label.t;
        (** When a [Prologue] is absent, this is the same as [entry_label].
            Otherwise, the [Prologue] falls through to this label. *)
    mutable id_to_label : Label.t Numbers.Int.Map.t
        (** Map id of instruction to label of the block that contains the
            instruction. Used for mapping perf data back to linear IR. *)
  }

val create : fun_name:string -> fun_tailrec_entry_point_label:Label.t -> t

val fun_name : t -> string

val entry_label : t -> Label.t

val fun_tailrec_entry_point_label : t -> Label.t

val successors : t -> basic_block -> successor list

val successor_labels : t -> basic_block -> Label.t list

val mem_block : t -> Label.t -> bool

val get_and_remove_block_exn : t -> Label.t -> basic_block

val get_block : t -> Label.t -> basic_block option

val get_block_exn : t -> Label.t -> basic_block

val set_fun_tailrec_entry_point_label : t -> Label.t -> unit

val iter_blocks : t -> f:(Label.t -> basic_block -> unit) -> unit

val compute_id_to_label : t -> unit

val id_to_label : t -> int -> Label.t option

(* printing *)

val print_terminator : out_channel -> terminator instruction -> unit

val print_basic : out_channel -> basic instruction -> unit
