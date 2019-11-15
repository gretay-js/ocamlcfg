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
  { (* CR mshinwell: Put the trap stack information here as discussed.
       Enforce the invariant that a block with an exception successor edge
       (which must always be to the label on the head of the trap stack) must
       have the same trap stack throughout its execution. *)
    start : Label.t;
    mutable body : basic instruction list;
    mutable terminator : terminator instruction;
    (* all predecessors: normal and exceptional path *)
    mutable predecessors : Label.Set.t;
    (* trap depth of the start of the block.

       CR-soon gyorsh: this can be derived from trap_stack below, except when
       the block is dead and the trap stack is not know, represented by None.
       Trap depth is used for cross checking the trap_stack and for emitting
       adjust trap on exit. *)
    trap_depth : int;
    (* All possible targets of raise in this block: subset of trap_handlers,
       based on instructions that can raise. Subset of trap_handlers. It does
       not account for interproc flow of exn.

       CR-soon gyorsh: After we split blocks, this will not be needed, it can
       be inferred from top of trap stack and can_raise of the block. *)
    mutable exns : Label.Set.t;
    (* Does this block contain any instruction that can raise, such as a
       call, bounds check, allocation, or a raise?

       CR-soon gyorsh: The current implementation with multiple pushtraps in
       each block means that raise can go to different trap_handlers
       associated with the block, depending on which one is on the top of the
       stack, so this an overapproximation. After we split the blocks, this +
       top of trap stack uniquely identifies the exn-successor of this block. *)
    mutable can_raise : bool;
    (* This block raises an exn that is not hanlderd in this function.
       can_raise_interproc implies can_raise but not necessarily vice versa. *)
    mutable can_raise_interproc : bool;
    (* is this block trap handler or not? i.e., is it an exn successor of
       another block? *)
    mutable is_trap_handler : bool
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

(** Does not account for exceptional flow from the block that go outside of
    the procedure. *)
val successor_labels :
  t -> normal:bool -> exn:bool -> basic_block -> Label.t list

(** In-place maps successor labels using [f]. Mutates the graph! *)
val replace_successor_labels :
  t ->
  normal:bool ->
  exn:bool ->
  basic_block ->
  f:(Label.t -> Label.t) ->
  unit

val mem_block : t -> Label.t -> bool

val get_and_remove_block_exn : t -> Label.t -> basic_block

val get_block : t -> Label.t -> basic_block option

val get_block_exn : t -> Label.t -> basic_block

val set_fun_tailrec_entry_point_label : t -> Label.t -> unit

val iter_blocks : t -> f:(Label.t -> basic_block -> unit) -> unit

val compute_id_to_label : t -> unit

val id_to_label : t -> int -> Label.t option

(** This function raises an exception that is not handled within that
    function. *)
val can_raise : t -> bool

(* printing *)

val print_terminator : out_channel -> terminator instruction -> unit

val print_basic : out_channel -> basic instruction -> unit
