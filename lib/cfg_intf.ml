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

(** Control flow graph structure types that are shared between the internal
    (mutable) and external (immutable) views of [Cfg]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module S = struct
  type func_call_operation =
    | Indirect of { label_after : Label.t }
    | Direct of
        { func_symbol : string;
          label_after : Label.t
        }

  type tail_call_operation =
    | Self of { label_after : Label.t }
    | Func of func_call_operation

  type prim_call_operation =
    | External of
        { func_symbol : string;
          alloc : bool;
          label_after : Label.t
        }
    | Alloc of
        { bytes : int;
          label_after_call_gc : Label.t option;
          spacetime_index : int
        }
    | Checkbound of
        { immediate : int option;
          label_after_error : Label.t option;
          spacetime_index : int
        }

  type operation =
    | Move
    | Spill
    | Reload
    | Const_int of nativeint
    | Const_float of int64
    | Const_symbol of string
    | Stackoffset of int
    | Load of Cmm.memory_chunk * Arch.addressing_mode
    | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
    | Intop of Mach.integer_operation
    | Intop_imm of Mach.integer_operation * int
    | Negf
    | Absf
    | Addf
    | Subf
    | Mulf
    | Divf
    | Floatofint
    | Intoffloat
    | Specific of Arch.specific_operation
    | Name_for_debugger of
        { ident : Ident.t;
          which_parameter : int option;
          provenance : unit option;
          is_assignment : bool
        }

  type call_operation =
    | P of prim_call_operation
    | F of func_call_operation

  type condition =
    | Always
    | Test of Mach.test

  type successor = condition * Label.t

  type 'a instruction =
    { desc : 'a;
      arg : Reg.t array;
      res : Reg.t array;
      dbg : Debuginfo.t;
      live : Reg.Set.t;
      trap_depth : int;
      id : int
    }

  type basic =
    | Op of operation
    | Call of call_operation
    | Reloadretaddr
    | Entertrap
    | Pushtrap of { lbl_handler : Label.t }
    | Poptrap
    | Prologue

  type terminator =
    | Branch of successor list
    | Switch of Label.t array
    | Return
    | Raise of Cmm.raise_kind
    | Tailcall of tail_call_operation
end

(* CR-soon gyorsh: Switch can be translated to Branch. *)
