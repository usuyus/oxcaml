(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
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

[@@@ocaml.warning "+a-40-41-42"]

(* Processor descriptions *)

(* Instruction selection *)
val word_addressed : bool

val phys_reg : Cmm.machtype_component -> int -> Reg.t

val precolored_regs : unit -> Reg.Set.t

(* If two registers have compatible types then we allow moves between them. Note
   that we never allow moves between different register classes or stack slot
   classes, so the following must hold: if [machtypes_are_compatible r1 r2] =
   true then [register_class r1] = [register_class r2] and [stack_class r1.typ]
   = [stack_class r2.typ]. *)
val types_are_compatible : Reg.t -> Reg.t -> bool

(* Calling conventions *)
val loc_arguments : Cmm.machtype -> Reg.t array * int

val loc_results_call : Cmm.machtype -> Reg.t array * int

val loc_parameters : Cmm.machtype -> Reg.t array

val loc_results_return : Cmm.machtype -> Reg.t array

(* Whether float/float32 arithmetic allows three operands. *)
val has_three_operand_float_ops : unit -> bool

(* For argument number [n] split across multiple registers, the target-specific
   implementation of [loc_external_arguments] must return [regs] such that
   [regs.(n).(0)] is to hold the part of the value at the lowest address. *)
val loc_external_arguments :
  Cmm.exttype list -> Reg.t array array * int * Cmm.stack_align

val loc_external_results : Cmm.machtype -> Reg.t array

val loc_exn_bucket : Reg.t

(* The maximum number of arguments of an OCaml to OCaml function call for which
   it is guaranteed there will be no arguments passed on the stack. (Above this
   limit, tail call optimization may be disabled.) N.B. The values for this
   parameter in the backends currently assume that no unboxed floats are passed
   using the OCaml calling conventions. *)
val max_arguments_for_tailcalls : int

(* Registers destroyed by operations *)
val destroyed_at_raise : Reg.t array

val destroyed_at_reloadretaddr : Reg.t array

val destroyed_at_pushtrap : Reg.t array

val destroyed_at_basic : Cfg_intf.S.basic -> Reg.t array

val destroyed_at_terminator : Cfg_intf.S.terminator -> Reg.t array

val is_destruction_point :
  more_destruction_points:bool -> Cfg_intf.S.terminator -> bool

(* Info for laying out the stack frame *)

val initial_stack_offset :
  num_stack_slots:int Stack_class.Tbl.t -> contains_calls:bool -> int

val trap_frame_size_in_bytes : int

val frame_required :
  fun_contains_calls:bool -> fun_num_stack_slots:int Stack_class.Tbl.t -> bool

val frame_size :
  stack_offset:int ->
  contains_calls:bool ->
  num_stack_slots:int Stack_class.Tbl.t ->
  int

type slot_offset = private
  | Bytes_relative_to_stack_pointer of int
  | Bytes_relative_to_domainstate_pointer of int

val slot_offset :
  Reg.stack_location ->
  stack_class:Stack_class.t ->
  stack_offset:int ->
  fun_contains_calls:bool ->
  fun_num_stack_slots:int Stack_class.Tbl.t ->
  slot_offset

(* Function prologues *)
val prologue_required :
  fun_contains_calls:bool -> fun_num_stack_slots:int Stack_class.Tbl.t -> bool

(** The DWARF register number corresponding to the stack pointer. *)
val stack_ptr_dwarf_register_number : int

(** The DWARF register number corresponding to the domainstate pointer. *)
val domainstate_ptr_dwarf_register_number : int

(* Calling the assembler *)
val assemble_file : string -> string -> int

(** [operation_supported op] returns true when [op]
    can be implemented directly with a hardware instruction.
    Used in Cmmgen when converting [@@builtin] external calls
    to primitive operations. *)
val operation_supported : Cmm.operation -> bool

(** [expression_supported exp] returns true when [exp] is a
    cmm expression supported by this architecture. *)
val expression_supported : Cmm.expression -> bool

(** The number of bytes each trap occupies on the stack. *)
val trap_size_in_bytes : int
