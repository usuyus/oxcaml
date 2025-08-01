(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]
(* Specific operations for the ARM processor, 64-bit mode *)

open! Int_replace_polymorphic_compare

open Format

let macosx = String.equal Config.system "macosx"

let is_asan_enabled = ref false

(* CR gyorsh: refactor to use [Arch.Extension] like amd64 *)
let feat_cssc = ref false

(* Machine-specific command-line options *)

let command_line_options = [
  "-fno-asan",
    Arg.Clear is_asan_enabled,
    " Disable AddressSanitizer. This is only meaningful if the compiler was \
     built with AddressSanitizer support enabled."
  ;

  "-fcssc",
    Arg.Set feat_cssc,
    " Enable the Common Short Sequence Compression (CSSC) instructions."
]

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int                     (* reg + displ *)
  | Ibased of string * int              (* global var + displ *)

(* We do not support the reg + shifted reg addressing mode, because
   what we really need is reg + shifted reg + displ,
   and this is decomposed in two instructions (reg + shifted reg -> tmp,
   then addressing tmp + displ). *)

(* Specific operations *)

type cmm_label = Label.t
  (* Do not introduce a dependency to Cmm *)

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

(* Specific operations, including [Simd], must not raise. *)
type specific_operation =
  | Ifar_poll
  | Ifar_alloc of { bytes : int; dbginfo : Cmm.alloc_dbginfo }
  | Ishiftarith of arith_operation * int
  | Imuladd       (* multiply and add *)
  | Imulsub       (* multiply and subtract *)
  | Inegmulf      (* floating-point negate and multiply *)
  | Imuladdf      (* floating-point multiply and add *)
  | Inegmuladdf   (* floating-point negate, multiply and add *)
  | Imulsubf      (* floating-point multiply and subtract *)
  | Inegmulsubf   (* floating-point negate, multiply and subtract *)
  | Isqrtf        (* floating-point square root *)
  | Ibswap of { bitwidth: bswap_bitwidth; } (* endianness conversion *)
  | Imove32       (* 32-bit integer move *)
  | Isignext of int (* sign extension *)
  | Isimd of Simd.operation

and arith_operation =
    Ishiftadd
  | Ishiftsub

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let size_vec128 = 16
let size_vec256 = 32
let size_vec512 = 64

let allow_unaligned_access = true

(* Behavior of division *)

let division_crashes_on_overflow = false

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  (* Resulting offset might not be representable, but that is the
     responsibility of the caller. *)
  match addr with
  | Iindexed i -> Iindexed (i + delta)
  | Ibased (sym, i) -> Ibased (sym, i + delta)

let num_args_addressing = function
  | Iindexed _ -> 1
  | Ibased _ -> 0

let addressing_displacement_for_llvmize addr =
  if not !Clflags.llvm_backend
  then
    Misc.fatal_error
      "Arch.displacement_addressing_for_llvmize: should only be called with \
        -llvm-backend"
  else
    match addr with
    | Iindexed d -> d
    | Ibased _ ->
      Misc.fatal_error
        "Arch.displacement_addressing_for_llvmize: unexpected addressing mode"

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Iindexed n ->
      printreg ppf arg.(0);
      if n <> 0 then fprintf ppf " + %i" n
  | Ibased(s, 0) ->
      fprintf ppf "\"%s\"" s
  | Ibased(s, n) ->
      fprintf ppf "\"%s\" + %i" s n

let int_of_bswap_bitwidth = function
  | Sixteen -> 16
  | Thirtytwo -> 32
  | Sixtyfour -> 64

let print_specific_operation printreg op ppf arg =
  match op with
  | Ifar_poll ->
    fprintf ppf "(far) poll"
  | Ifar_alloc { bytes; dbginfo = _ } ->
    fprintf ppf "(far) alloc %i" bytes
  | Ishiftarith(op, shift) ->
      let op_name = function
      | Ishiftadd -> "+"
      | Ishiftsub -> "-" in
      let shift_mark =
       if shift >= 0
       then sprintf "<< %i" shift
       else sprintf ">> %i" (-shift) in
      fprintf ppf "%a %s %a %s"
       printreg arg.(0) (op_name op) printreg arg.(1) shift_mark
  | Imuladd ->
      fprintf ppf "(%a * %a) + %a"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Imulsub ->
      fprintf ppf "-(%a * %a) + %a"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmulf ->
      fprintf ppf "-f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
  | Imuladdf ->
      fprintf ppf "%a +f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmuladdf ->
      fprintf ppf "(-f %a) -f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Imulsubf ->
      fprintf ppf "%a -f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmulsubf ->
      fprintf ppf "(-f %a) +f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Isqrtf ->
      fprintf ppf "sqrtf %a"
        printreg arg.(0)
  | Ibswap { bitwidth } ->
      let n = int_of_bswap_bitwidth bitwidth in
      fprintf ppf "bswap%i %a" n
        printreg arg.(0)
  | Imove32 ->
      fprintf ppf "move32 %a"
        printreg arg.(0)
  | Isignext n ->
      fprintf ppf "signext%d %a"
        n printreg arg.(0)
  | Isimd op ->
    Simd.print_operation printreg op ppf arg

let specific_operation_name : specific_operation -> string = fun op ->
  match op with
  | Ifar_poll -> "far poll"
  | Ifar_alloc { bytes; dbginfo = _ } ->
      Printf.sprintf "far alloc of %d bytes" bytes
  | Ishiftarith (op, shift) ->
      let op_name = function
        | Ishiftadd -> "+"
        | Ishiftsub -> "-" in
      let shift_mark =
        if shift >= 0
        then sprintf "<< %i" shift
        else sprintf ">> %i" (-shift) in
      Printf.sprintf "%s %s" (op_name op) shift_mark
  | Imuladd -> "muladd"
  | Imulsub -> "mulsub"
  | Inegmulf -> "negmulf"
  | Imuladdf -> "muladdf"
  | Inegmuladdf -> "negmuladdf"
  | Imulsubf -> "mulsubf"
  | Inegmulsubf -> "negmulsubf"
  | Isqrtf -> "sqrtf"
  | Ibswap _ -> "bswap"
  | Imove32 -> "move32"
  | Isignext _ -> "signext"
  | Isimd _ -> "simd"

let equal_addressing_mode left right =
  match left, right with
  | Iindexed left_int, Iindexed right_int ->
    Int.equal left_int right_int
  | Ibased (left_string, left_int), Ibased (right_string, right_int) ->
    String.equal left_string right_string
    && Int.equal left_int right_int
  | (Iindexed _ | Ibased _), _ -> false

let equal_arith_operation left right =
  match left, right with
  | Ishiftadd, Ishiftadd -> true
  | Ishiftsub, Ishiftsub -> true
  | (Ishiftadd | Ishiftsub), _ -> false

let equal_specific_operation left right =
  match left, right with
  | Ifar_alloc { bytes = left_bytes; dbginfo = _; },
    Ifar_alloc { bytes = right_bytes; dbginfo = _; } ->
    Int.equal left_bytes right_bytes
  | Ishiftarith (left_arith_operation, left_int),
    Ishiftarith (right_arith_operation, right_int) ->
    equal_arith_operation left_arith_operation right_arith_operation
    && Int.equal left_int right_int
  | Imuladd, Imuladd -> true
  | Imulsub, Imulsub -> true
  | Inegmulf, Inegmulf -> true
  | Imuladdf, Imuladdf -> true
  | Inegmuladdf, Inegmuladdf -> true
  | Imulsubf, Imulsubf -> true
  | Inegmulsubf, Inegmulsubf -> true
  | Isqrtf, Isqrtf -> true
  | Ibswap { bitwidth = left }, Ibswap { bitwidth = right } ->
    Int.equal (int_of_bswap_bitwidth left) (int_of_bswap_bitwidth right)
  | Imove32, Imove32 -> true
  | Isignext left, Isignext right -> Int.equal left right
  | Isimd left, Isimd right -> Simd.equal_operation left right
  | (Ifar_alloc _  | Ifar_poll  | Ishiftarith _
    | Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf | Imulsubf
    | Inegmulsubf | Isqrtf | Ibswap _ | Imove32 | Isignext _ | Isimd _), _ -> false

let isomorphic_specific_operation op1 op2 =
  equal_specific_operation op1 op2

(* Recognition of logical immediate arguments *)

(* An automaton to recognize ( 0+1+0* | 1+0+1* )

               0          1          0
              / \        / \        / \
              \ /        \ /        \ /
        -0--> [1] --1--> [2] --0--> [3]
       /
     [0]
       \
        -1--> [4] --0--> [5] --1--> [6]
              / \        / \        / \
              \ /        \ /        \ /
               1          0          1

The accepting states are 2, 3, 5 and 6. *)

let auto_table = [|   (* accepting?, next on 0, next on 1 *)
  (* state 0 *) (false, 1, 4);
  (* state 1 *) (false, 1, 2);
  (* state 2 *) (true,  3, 2);
  (* state 3 *) (true,  3, 7);
  (* state 4 *) (false, 5, 4);
  (* state 5 *) (true,  5, 6);
  (* state 6 *) (true,  7, 6);
  (* state 7 *) (false, 7, 7)   (* error state *)
|]

let rec run_automata nbits state input =
  let (acc, next0, next1) = auto_table.(state) in
  if nbits <= 0
  then acc
  else run_automata (nbits - 1)
                    (if Nativeint.equal (Nativeint.logand input 1n) 0n then next0 else next1)
                    (Nativeint.shift_right_logical input 1)

(* The following function determines a length [e]
   such that [x] is a repetition [BB...B] of a bit pattern [B] of length [e].
   [e] ranges over 64, 32, 16, 8, 4, 2.  The smaller [e] the better. *)

let logical_imm_length x =
  (* [test n] checks that the low [2n] bits of [x] are of the
     form [BB], that is, two occurrences of the same [n] bits *)
  let test n =
    let mask = Nativeint.(sub (shift_left 1n n) 1n) in
    let low_n_bits = Nativeint.(logand x mask) in
    let next_n_bits = Nativeint.(logand (shift_right_logical x n) mask) in
    Nativeint.equal low_n_bits next_n_bits in
  (* If [test n] fails, we know that the length [e] is
     at least [2n].  Hence we test with decreasing values of [n]:
     32, 16, 8, 4, 2. *)
  if not (test 32) then 64
  else if not (test 16) then 32
  else if not (test 8) then 16
  else if not (test 4) then 8
  else if not (test 2) then 4
  else 2

(* A valid logical immediate is
- neither [0] nor [-1];
- composed of a repetition [BBBBB] of a bit-pattern [B] of length [e]
- the low [e] bits of the number, that is, [B], match [0+1+0*] or [1+0+1*].
*)

let is_logical_immediate x =
  not (Nativeint.equal x 0n) && not (Nativeint.equal x (-1n)) && run_automata (logical_imm_length x) 0 x

(* Specific operations that are pure *)

let operation_is_pure : specific_operation -> bool = function
  | Ifar_alloc _ | Ifar_poll -> false
  | Ishiftarith _ -> true
  | Imuladd -> true
  | Imulsub -> true
  | Inegmulf -> true
  | Imuladdf -> true
  | Inegmuladdf -> true
  | Imulsubf -> true
  | Inegmulsubf -> true
  | Isqrtf -> true
  | Ibswap _ -> true
  | Imove32 -> true
  | Isignext _ -> true
  | Isimd op -> Simd.operation_is_pure op

(* Specific operations that can raise *)

let operation_allocates = function
  | Ifar_alloc _ -> true
  | Ifar_poll
  | Imuladd
  | Imulsub
  | Inegmulf
  | Imuladdf
  | Inegmuladdf
  | Imulsubf
  | Inegmulsubf
  | Isqrtf
  | Imove32
  | Ishiftarith (_, _)
  | Isignext _
  | Ibswap _
  | Isimd _ -> false

(* See `amd64/arch.ml`. *)
let equal_addressing_mode_without_displ (addressing_mode_1: addressing_mode)
      (addressing_mode_2 : addressing_mode) =
  match addressing_mode_1, addressing_mode_2 with
  | Iindexed _, Iindexed _ -> true
  | Ibased (var1, _), Ibased (var2, _) -> String.equal var1 var2
  | (Iindexed _ | Ibased _), _ -> false

let addressing_offset_in_bytes (_addressing_mode_1: addressing_mode)
      (_addressing_mode_2 : addressing_mode) ~arg_offset_in_bytes:_ _ _ =
  None
