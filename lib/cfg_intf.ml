(** Control flow graph structure types that are shared between the internal
    (mutable) and external (immutable) views of [Cfg]. *)

[@@@ocaml.warning "+a-30-40-41-42"]

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
    | Const_int of nativeint (* CR-someday xclerc: change to `Targetint.t` *)
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

  type bool_test =
    {
      ifso : Label.t; (* if test is true goto [ifso] label *)
      ifnot : Label.t; (* if test is false goto [ifnot] label *)
    }

  (* CR mshinwell: It's not actually clear what [x] and [y] are... *)
  type int_test =
    {
      lt : Label.t; (* if x < y  goto [lt] label *)
      eq : Label.t; (* if x = y  goto [eq] label *)
      gt : Label.t; (* if x > y  goto [gt] label *)
      is_signed: bool;
      imm : int option;
      (* CR mshinwell: Add comment explaining [imm] *)
    }

  (** For floats, it is not enough to check "=,<,>"
      because possible outcomes of comparison include "unordered"
      (see e.g. x86-64 emitter) when the arguments involve NaNs.
  *)
  type float_test =
    {
      lt : Label.t;
      eq : Label.t;
      gt : Label.t;
      uo : Label.t; (* if at least one of x or y is NaN *)
    }

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
    | Pushtrap of { lbl_handler : Label.t }
    | Poptrap
    | Prologue


  (* CR mshinwell: Add an example to the first point that clarifies what
     "tests of different types" are *)
  (* CR mshinwell: The "redundancy of labels" bullet point doesn't make
     sense.  Should it say "a test can lead" instead of "test leading"? *)
  (* CR mshinwell: There's at least one other redundancy too by the look
     of it (Switch with every label the same vs. Always) *)
  (** Properties of the representation of successors:
      - tests of different types are not mixed
      - total: all possible outcomes of a test have a defined target label
      - disjoint: at most one of the outcome of a test is true
      - redundancy of labels: more than one outcome of test leading
        to the same label
      - redundancy of representation of unconditional jump: (Always l)
        can be simplified to (Is_even {true_=l;false_=l})
  *)
  type terminator =
    | Never
    | Always of Label.t
    (* CR mshinwell: Is_even is a weird name, can this be improved? *)
    | Is_even of bool_test
    | Is_true of bool_test
    | Float_test of float_test
    | Int_test of int_test
    | Switch of Label.t array
    | Return
    | Raise of Cmm.raise_kind
    | Tailcall of tail_call_operation
end

(* CR-soon gyorsh: Switch can be translated to Branch. *)
