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

module C = Cfg

(* CR-soon gyorsh: Too complicated? This approach may be useful earlier in
   the compiler. Is there existing code in the compiler that does something
   similar? *)

module Rep : sig
  type t

  val disjunction : t -> t -> t

  type cint = private
    | Always
    | Cint of Cmm.integer_comparison

  val of_cint : Cmm.integer_comparison -> t

  val to_cint : t -> cint

  type cfloat = private
    | Always
    | Unordered (* for floating point comparisons not representable in Cmm *)
    | Cfloat of Cmm.float_comparison

  val of_cfloat : Cmm.float_comparison -> t

  val to_cfloat : t -> cfloat
end = struct
  (* We represent a powerset of {eq, lt, gt, uo} as a "bitvector" of length
     4. "uo" stands for "unordered". *)
  type bit =
    | T
    | F

  type t =
    { eq : bit;
      lt : bit;
      gt : bit;
      uo : bit
    }

  type cint =
    | Always
    | Cint of Cmm.integer_comparison

  type cfloat =
    | Always
    | Unordered
    | Cfloat of Cmm.float_comparison

  let disjunction c1 c2 =
    let ( || ) b1 b2 = if b1 = T || b2 = T then T else F in
    { eq = c1.eq || c2.eq;
      lt = c1.lt || c2.lt;
      gt = c1.gt || c2.gt;
      uo = c1.uo || c2.uo
    }

  let of_cint (comp : Cmm.integer_comparison) =
    match comp with
    | Ceq -> { eq = T; lt = F; gt = F; uo = F }
    | Clt -> { eq = F; lt = T; gt = F; uo = F }
    | Cgt -> { eq = F; lt = F; gt = T; uo = F }
    | Cne -> { eq = F; lt = T; gt = T; uo = F }
    | Cle -> { eq = T; lt = T; gt = F; uo = F }
    | Cge -> { eq = T; lt = F; gt = T; uo = F }

  let to_cint t : cint =
    match t with
    | { eq = T; lt = F; gt = F; uo = F } -> Cint Ceq
    | { eq = F; lt = T; gt = F; uo = F } -> Cint Clt
    | { eq = F; lt = F; gt = T; uo = F } -> Cint Cgt
    | { eq = F; lt = T; gt = T; uo = F } -> Cint Cne
    | { eq = T; lt = T; gt = F; uo = F } -> Cint Cle
    | { eq = T; lt = F; gt = T; uo = F } -> Cint Cge
    | { eq = T; lt = T; gt = T; uo = F } -> Always
    | _ -> assert false

  let of_cfloat (comp : Cmm.float_comparison) =
    match comp with
    | CFeq -> { eq = T; lt = F; gt = F; uo = F }
    | CFlt -> { eq = F; lt = T; gt = F; uo = F }
    | CFgt -> { eq = F; lt = F; gt = T; uo = F }
    | CFle -> { eq = T; lt = T; gt = F; uo = F }
    | CFge -> { eq = T; lt = F; gt = T; uo = F }
    | CFneq -> { eq = F; lt = T; gt = T; uo = T }
    | CFnlt -> { eq = T; lt = F; gt = T; uo = T }
    | CFngt -> { eq = T; lt = T; gt = F; uo = T }
    | CFnle -> { eq = F; lt = F; gt = T; uo = T }
    | CFnge -> { eq = F; lt = T; gt = F; uo = T }

  let to_cfloat t : cfloat =
    match t with
    | { eq = T; lt = F; gt = F; uo = F } -> Cfloat CFeq
    | { eq = F; lt = T; gt = F; uo = F } -> Cfloat CFlt
    | { eq = F; lt = F; gt = T; uo = F } -> Cfloat CFgt
    | { eq = T; lt = T; gt = F; uo = F } -> Cfloat CFle
    | { eq = T; lt = F; gt = T; uo = F } -> Cfloat CFge
    | { eq = F; lt = T; gt = T; uo = T } -> Cfloat CFneq
    | { eq = T; lt = F; gt = T; uo = T } -> Cfloat CFnlt
    | { eq = T; lt = T; gt = F; uo = T } -> Cfloat CFngt
    | { eq = F; lt = F; gt = T; uo = T } -> Cfloat CFnle
    | { eq = F; lt = T; gt = F; uo = T } -> Cfloat CFnge
    (* CR mshinwell: add comment about [Always] (see e.g. x86-64 emitter) *)
    | { eq = T; lt = T; gt = T; uo = T } -> Always
    | { eq = F; lt = F; gt = F; uo = T } -> Unordered
    (* CR mshinwell: What about ones that can't be expressed as a single
       comparison, e.g. CFle || CFge? Maybe return something like: Ok of ...
       | Cannot_simplify ? *)
    | _ -> assert false
end

(* Compute one comparison operator that is equivalent to the disjunction of
   the operators c1 and c2. The resulting comparison is mapped through [f],
   unless the result is "Always", which is not expressible as a comparison. *)
let simplify_disjunction_int c1 c2 f =
  let res = Rep.(to_cint (disjunction (of_cint c1) (of_cint c2))) in
  match res with
  | Cint c -> [f c]
  | Always -> [C.Always]

let simplify_disjunction_float c1 c2 =
  let res = Rep.(to_cfloat (disjunction (of_cfloat c1) (of_cfloat c2))) in
  match res with
  | Cfloat c -> [C.Test (Ifloattest c)]
  | Always -> [C.Always]
  | Unordered ->
      (* CR mshinwell: or "Cannot_simplify" *)
      (* There is no specific representation for [Unordered]. *)
      [C.Test (Ifloattest c1); C.Test (Ifloattest c2)]

let disjunction (cmp1 : C.condition) (cmp2 : C.condition) =
  match (cmp1, cmp2) with
  | Always, _ | _, Always ->
      (* This can happen only as a result of a previous simplification. *)
      [C.Always]
  | Test c1, Test c2 -> (
      if c1 = c2 then [C.Test c1]
      else if c1 = Linear.invert_test c2 then [C.Always]
      else
        match (c1, c2) with
        | Iinttest (Isigned cmp1), Iinttest (Isigned cmp2) ->
            simplify_disjunction_int cmp1 cmp2 (fun cmp ->
                C.Test (Iinttest (Isigned cmp)))
        | Iinttest (Iunsigned cmp1), Iinttest (Iunsigned cmp2) ->
            simplify_disjunction_int cmp1 cmp2 (fun cmp ->
                C.Test (Iinttest (Iunsigned cmp)))
        | Iinttest_imm (Isigned cmp1, n1), Iinttest_imm (Isigned cmp2, n2)
          when n1 = n2 ->
            simplify_disjunction_int cmp1 cmp2 (fun cmp ->
                C.Test (Iinttest_imm (Isigned cmp, n1)))
        | Iinttest_imm (Iunsigned cmp1, n1), Iinttest_imm (Iunsigned cmp2, n2)
          when n1 = n2 ->
            simplify_disjunction_int cmp1 cmp2 (fun cmp ->
                C.Test (Iinttest_imm (Iunsigned cmp, n1)))
        | Ifloattest cmp1, Ifloattest cmp2 ->
            simplify_disjunction_float cmp1 cmp2
        (* CR mshinwell: It isn't obvious from this function what hits this
           case; use [Misc.fatal_errorf]. *)
        | _ -> assert false )
