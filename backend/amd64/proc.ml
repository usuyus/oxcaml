(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

(* Description of the AMD64 processor *)

open! Int_replace_polymorphic_compare

open Misc
open Arch
open Cmm

let fp = Config.with_frame_pointers

(* Which ABI to use *)

let win64 = Arch.win64

(* Registers available for register allocation *)

(* Register map:
    rax         0
    rbx         1
    rdi         2
    rsi         3
    rdx         4
    rcx         5
    r8          6
    r9          7
    r12         8
    r13         9
    r10         10
    r11         11
    rbp         12
    r14         domain state pointer
    r15         allocation pointer

  xmm0 - xmm15  100 - 115 *)

(* Conventions:
     rax - r13: OCaml function arguments
     rax: OCaml and C function results
     xmm0 - xmm9: OCaml function arguments
     xmm0: OCaml and C function results
   Under Unix:
     rdi, rsi, rdx, rcx, r8, r9: C function arguments
     xmm0 - xmm7: C function arguments
     rbx, rbp, r12-r15 are preserved by C
     xmm registers are not preserved by C
   Under Win64:
     rcx, rdx, r8, r9: C function arguments
     xmm0 - xmm3: C function arguments
     rbx, rbp, rsi, rdi r12-r15 are preserved by C
     xmm6-xmm15 are preserved by C
   Note (PR#5707, GPR#1304): PLT stubs (used for dynamic resolution of symbols
     on Unix-like platforms) may clobber any register except those used for:
       1. C parameter passing;
       2. C return values;
       3. C callee-saved registers.
     This translates to the set { r10, r11 }.  These registers hence cannot
     be used for OCaml parameter passing and must also be marked as
     destroyed across [Ialloc] and [Ipoll] (otherwise a call to
     caml_call_gc@PLT might clobber these two registers before the assembly
     stub saves them into the GC regs block).
*)

let types_are_compatible (left : Reg.t)  (right : Reg.t) =
  match left.typ, right.typ with
  | (Int | Val | Addr), (Int | Val | Addr)
  | Float, Float
  | Float32, Float32
  | (Valx2 | Vec128), (Valx2 | Vec128) ->
    true
  | Vec256, Vec256 ->
    true
  | Vec512, Vec512 ->
    true
  | (Int | Val | Addr | Float | Float32 |
     Vec128 | Vec256 | Vec512 | Valx2), _ -> false

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 13 Reg.dummy in
  for i = 0 to 12 do v.(i) <- Reg.create_at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.make 16 Reg.dummy in
  for i = 0 to 15 do v.(i) <- Reg.create_at_location Float (Reg (100 + i)) done;
  v

let hard_vec128_reg =
  Array.map (fun r -> {r with Reg.typ = Vec128}) hard_float_reg
let hard_vec256_reg =
  Array.map (fun r -> {r with Reg.typ = Vec256}) hard_float_reg
let hard_vec512_reg =
  Array.map (fun r -> {r with Reg.typ = Vec512}) hard_float_reg
let hard_float32_reg =
  Array.map (fun r -> {r with Reg.typ = Float32}) hard_float_reg

let add_hard_vec256_regs list ~f =
  if Arch.Extension.enabled_vec256 ()
  then f hard_vec256_reg :: list else list

let add_hard_vec512_regs list ~f =
  if Arch.Extension.enabled_vec512 ()
  then f hard_vec512_reg :: list else list

let all_phys_regs =
  [hard_int_reg; hard_float_reg; hard_float32_reg; hard_vec128_reg]
  |> add_hard_vec256_regs ~f:(fun regs -> regs)
  |> add_hard_vec512_regs ~f:(fun regs -> regs)
  |> Array.concat

let phys_reg ty n =
  match (ty : machtype_component) with
  | Int | Addr | Val -> hard_int_reg.(n)
  | Float -> hard_float_reg.(n - 100)
  | Float32 -> hard_float32_reg.(n - 100)
  | Vec128 | Valx2 -> hard_vec128_reg.(n - 100)
  | Vec256 -> hard_vec256_reg.(n - 100)
  | Vec512 -> hard_vec512_reg.(n - 100)

let rax = phys_reg Int 0
let rdi = phys_reg Int 2
let rdx = phys_reg Int 4
let rcx = phys_reg Int 5
let r10 = phys_reg Int 10
let r11 = phys_reg Int 11
let rbp = phys_reg Int 12

(* CSE needs to know that all versions of xmm15 are destroyed. *)
let destroy_xmm =
  let types =
    ([ Float; Float32; Vec128 ] : machtype_component list)
    |> add_hard_vec256_regs ~f:(fun _ : machtype_component -> Vec256)
    |> add_hard_vec512_regs ~f:(fun _ : machtype_component -> Vec512)
    |> Array.of_list
  in
  fun n -> Array.map (fun t -> phys_reg t (100 + n)) types

let destroyed_by_plt_stub =
  if not X86_proc.use_plt then [| |] else [| r10; r11 |]

let destroyed_by_plt_stub_set = Reg.set_of_array destroyed_by_plt_stub

let stack_slot slot ty = Reg.create_at_location ty (Stack slot)

(* Instruction selection *)

let word_addressed = false

(* Calling conventions *)

let size_domainstate_args = 64 * size_int

let calling_conventions
      ~first_int
      ~last_int
      ~step_int
      ~first_float
      ~last_float
      ~make_stack
      ~first_stack
      arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref first_stack in
  let stack_vec256, stack_vec512 = ref false, ref false in
  (* A negative offset indicates a domainstate slot, which will
     generate unaligned moves. *)
  let align ofs size =
    if ofs <= -size then ofs
    else if ofs <= 0 then 0
    else Misc.align ofs size
  in
  for i = 0 to Array.length arg - 1 do
    match (arg.(i) : machtype_component) with
    | Val | Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg ty !int;
          int := !int + step_int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end;
        assert (not (Reg.Set.mem loc.(i) destroyed_by_plt_stub_set))
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg Float !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
    | Vec128 ->
      if !float <= last_float then begin
        loc.(i) <- phys_reg Vec128 !float;
        incr float
      end else begin
        ofs := align !ofs size_vec128;
        loc.(i) <- stack_slot (make_stack !ofs) Vec128;
        ofs := !ofs + size_vec128
      end
    | Vec256 ->
      if !float <= last_float then begin
        loc.(i) <- phys_reg Vec256 !float;
        incr float
      end else begin
        stack_vec256 := true;
        ofs := align !ofs size_vec256;
        loc.(i) <- stack_slot (make_stack !ofs) Vec256;
        ofs := !ofs + size_vec256
      end
    | Vec512 ->
      if !float <= last_float then begin
        loc.(i) <- phys_reg Vec512 !float;
        incr float
      end else begin
        stack_vec512 := true;
        ofs := align !ofs size_vec512;
        loc.(i) <- stack_slot (make_stack !ofs) Vec512;
        ofs := !ofs + size_vec512
      end
    | Valx2 ->
      Misc.fatal_error "Unexpected machtype_component Valx2"
    | Float32 ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg Float32 !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float32;
          (* float32 slots still take up a full word *)
          ofs := !ofs + size_float
        end
  done;
  let pre_align, post_align =
    if !stack_vec512 then Align_64, 64
    else if !stack_vec256 then Align_32, 32
    else Align_16, 16
  in
  (loc, Misc.align (max 0 !ofs) post_align, pre_align)

let incoming ofs : Reg.stack_location =
  if ofs >= 0
  then Incoming ofs
  else Domainstate (ofs + size_domainstate_args)
let outgoing ofs : Reg.stack_location =
  if ofs >= 0
  then Outgoing ofs
  else Domainstate (ofs + size_domainstate_args)
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  let (loc, ofs, _align) =
    calling_conventions
        ~first_int:0
        ~last_int:9
        ~step_int:1
        ~first_float:100
        ~last_float:109
        ~make_stack:outgoing
        ~first_stack:(- size_domainstate_args)
        arg
  in
  loc, ofs

let loc_parameters arg =
  let (loc, _ofs, _align) =
    calling_conventions
      ~first_int:0
      ~last_int:9
      ~step_int:1
      ~first_float:100
      ~last_float:109
      ~make_stack:incoming
      ~first_stack:(- size_domainstate_args)
      arg
  in
  loc

let loc_results_call res =
  let (loc, ofs, _align) =
    calling_conventions
      ~first_int:0
      ~last_int:9
      ~step_int:1
      ~first_float:100
      ~last_float:109
      ~make_stack:outgoing
      ~first_stack:(- size_domainstate_args)
      res
  in
  loc, ofs

let loc_results_return res =
  let (loc, _ofs, _align) =
    calling_conventions
      ~first_int:0
      ~last_int:9
      ~step_int:1
      ~first_float:100
      ~last_float:109
      ~make_stack:incoming
      ~first_stack:(- size_domainstate_args)
      res
  in loc

let max_arguments_for_tailcalls = 10 (* in regs *) + 64 (* in domain state *)

(* C calling conventions under Unix:
     first integer args in rdi, rsi, rdx, rcx, r8, r9
     first float args in xmm0 ... xmm7
     remaining args on stack
     return value in rax and rdx for integers, and xmm0 and xmm1 for floats.
  C calling conventions under Win64:
     first integer args in rcx, rdx, r8, r9
     first float args in xmm0 ... xmm3
     each integer arg consumes a float reg, and conversely
     remaining args on stack
     always 32 bytes reserved at bottom of stack.
     Return value in rax or xmm0. *)

let loc_external_results res =
  let (loc, _ofs, _align) =
    (* `~last_int:4 ~step_int:4` below is to get rdx as the second int register
       (See https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf, pages 21 and 22) *)
    calling_conventions
      ~first_int:0
      ~last_int:4
      ~step_int:4
      ~first_float:100
      ~last_float:101
      ~make_stack:not_supported
      ~first_stack:0
      res
  in loc

let unix_loc_external_arguments arg =
  calling_conventions
    ~first_int:2
    ~last_int:7
    ~step_int:1
    ~first_float:100
    ~last_float:107
    ~make_stack:outgoing
    ~first_stack:0
    arg

let win64_int_external_arguments =
  [| 5 (*rcx*); 4 (*rdx*); 6 (*r8*); 7 (*r9*) |]
let win64_float_external_arguments =
  [| 100 (*xmm0*); 101 (*xmm1*); 102 (*xmm2*); 103 (*xmm3*) |]

let win64_loc_external_arguments arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let reg = ref 0
  and ofs = ref (if Config.runtime5 then 0 else 32) in
  for i = 0 to Array.length arg - 1 do
    match (arg.(i) : machtype_component) with
    | Val | Int | Addr as ty ->
        if !reg < 4 then begin
          loc.(i) <- phys_reg ty win64_int_external_arguments.(!reg);
          incr reg
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !reg < 4 then begin
          loc.(i) <- phys_reg Float win64_float_external_arguments.(!reg);
          incr reg
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) Float;
          ofs := !ofs + size_float
        end
    | Float32 ->
        if !reg < 4 then begin
          loc.(i) <- phys_reg Float32 win64_float_external_arguments.(!reg);
          incr reg
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) Float32;
          (* float32 slots still take up a full word *)
          ofs := !ofs + size_float
        end
    | Vec128 | Vec256 | Vec512 ->
        (* CR mslater: (SIMD) win64 calling convention requires pass by reference *)
        Misc.fatal_error "SIMD external arguments are not supported on Win64"
    | Valx2 ->
      Misc.fatal_error "Unexpected machtype_component Valx2"
  done;
  (loc, Misc.align !ofs 16, Align_16) (* keep stack 16-aligned *)

let loc_external_arguments ty_args =
  let arg = Cmm.machtype_of_exttype_list ty_args in
  let loc, stack_ofs, align =
    if win64
    then win64_loc_external_arguments arg
    else unix_loc_external_arguments arg
  in
  Array.map (fun reg -> [|reg|]) loc, stack_ofs, align

let loc_exn_bucket = rax

let stack_ptr_dwarf_register_number = 7
let domainstate_ptr_dwarf_register_number = 14

(* Registers destroyed by operations *)

let int_regs_destroyed_at_c_call_win64 =
  if Config.runtime5 then [|0;1;4;5;6;7;10;11;12|] else [|0;4;5;6;7;10;11|]

let int_regs_destroyed_at_c_call =
  if Config.runtime5 then [|0;1;2;3;4;5;6;7;10;11|] else [|0;2;3;4;5;6;7;10;11|]

let destroyed_at_c_call_win64 =
  (* Win64: rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15 preserved *)
  [ Array.map (phys_reg Int) int_regs_destroyed_at_c_call_win64;
    Array.sub hard_float_reg 0 6;
    Array.sub hard_float32_reg 0 6;
    Array.sub hard_vec128_reg 0 6 ]
  |> add_hard_vec256_regs ~f:(fun regs -> Array.sub regs 0 6)
  |> add_hard_vec512_regs ~f:(fun regs -> Array.sub regs 0 6)
  |> Array.concat

let destroyed_at_c_call_unix =
  (* Unix: rbx, rbp, r12-r15 preserved *)
  [ Array.map (phys_reg Int) int_regs_destroyed_at_c_call;
    hard_float_reg;
    hard_float32_reg;
    hard_vec128_reg ]
  |> add_hard_vec256_regs ~f:(fun regs -> regs)
  |> add_hard_vec512_regs ~f:(fun regs -> regs)
  |> Array.concat

let destroyed_at_c_call =
  (* C calling conventions preserve rbx, but it is clobbered
     by the code sequence used for C calls in emit.ml, so it
     is marked as destroyed. *)
  if win64 then destroyed_at_c_call_win64 else destroyed_at_c_call_unix

let destroyed_at_alloc_or_poll =
  if X86_proc.use_plt then
    destroyed_by_plt_stub
  else
    [| r11 |]

let destroyed_at_pushtrap =
  [| r11 |]

let destroyed_at_large_memory_op =
  if Config.with_address_sanitizer then
    (* We need a scratch register [r11] to preform the address sanitizer check,
       and [rdi] might be destroyed in the event we need to call the ASAN error
       reporting function, since it's a C function accepting a single argument.
       No other registers are destroyed because the ASAN report wrappers use a
       special calling convention via the C attribute [__attribute__((preserve_all))]
       such that all registers except for [r11] are callee-saved, in order to minimize
       the amount of spilling we need to do. *)
    [| rdi; r11 |]
  else
    [||]
;;

let destroyed_at_small_memory_op =
  if Config.with_address_sanitizer then
    (* Everything stated above in the comment for [destroyed_at_large_memory_op]
       applies here too, but in addition we need one more scratch register [r10]
       in order to compute the additional [SlowPathCheck] for memory accesses that
       are smaller than one word. *)
    [| rdi; r10; r11 |]
  else
    [||]
;;

let destroyed_at_single_float64_store =
  if Config.with_address_sanitizer then
    Array.append destroyed_at_small_memory_op (destroy_xmm 15)
  else
    (destroy_xmm 15)
;;

let all_256bit_regs = []
  |> add_hard_vec256_regs ~f:(fun regs -> regs)
  |> add_hard_vec512_regs ~f:(fun regs -> regs)
  |> Array.concat

let all_simd_regs =
  [hard_float32_reg; hard_float_reg; hard_vec128_reg]
  |> add_hard_vec256_regs ~f:(fun regs -> regs)
  |> add_hard_vec512_regs ~f:(fun regs -> regs)
  |> Array.concat

let destroyed_by_simd_instr (instr : Simd.instr) =
  (* CR mslater: (SIMD) these don't effect regs 16-31 *)
  match[@warning "-4"] instr.id with
  | Vzeroupper -> all_256bit_regs
  | Vzeroall -> all_simd_regs
  | _ ->
    match instr.res with
    | First_arg -> [||]
    | Res { loc; _ } ->
      match Simd.loc_is_pinned loc with
      | Some RAX -> [|rax|]
      | Some RCX -> [|rcx|]
      | Some RDX -> [|rdx|]
      | Some XMM0 -> destroy_xmm 0
      | None -> [||]

let destroyed_by_simd_op (op : Simd.operation) =
  match op.instr with
  | Instruction instr -> destroyed_by_simd_instr instr
  | Sequence seq ->
    destroyed_by_simd_instr seq.instr
    |> Array.append
      (match seq.id with
       | Sqrtss | Sqrtsd | Roundss | Roundsd
       | Pcompare_string _ | Vpcompare_string _
       | Ptestz | Ptestc | Ptestnzc
       | Vptestz_X  | Vptestc_X  | Vptestnzc_X
       | Vptestz_Y  | Vptestc_Y  | Vptestnzc_Y -> [||])

let destroyed_by_simd_mem_op (instr : Simd.Mem.operation) =
  match instr with
  | Add_f32 | Sub_f32 | Mul_f32 | Div_f32
  | Add_f64 | Sub_f64 | Mul_f64 | Div_f64 -> [||]

let destroyed_at_raise = all_phys_regs

let destroyed_at_reloadretaddr = [| |]

let destroyed_at_basic (basic : Cfg_intf.S.basic) =
  match basic with
  | Reloadretaddr ->
    destroyed_at_reloadretaddr
  | Pushtrap _ ->
    destroyed_at_pushtrap
  | Op (Intop (Idiv | Imod)) | Op (Intop_imm ((Idiv | Imod), _)) ->
    [| rax; rdx |]
  | Op(Store(Single { reg = Float64 }, _, _)) ->
    destroyed_at_single_float64_store
  | Op (Store ((Byte_unsigned | Byte_signed | Sixteen_unsigned |
                Sixteen_signed | Thirtytwo_unsigned | Thirtytwo_signed |
                Single { reg = Float32 } ), _, _))
  | Op (Load { memory_chunk =
               (Byte_unsigned | Byte_signed | Sixteen_unsigned |
                Sixteen_signed | Thirtytwo_unsigned | Thirtytwo_signed |
                Single _); _ })
  | Op(Specific (Ifloatarithmem (Float32, _, _)))
  | Op(Intop_atomic _) ->
    destroyed_at_small_memory_op
  | Op(Store( (Word_int | Word_val | Double | Onetwentyeight_aligned |
               Onetwentyeight_unaligned | Twofiftysix_aligned |
               Twofiftysix_unaligned | Fivetwelve_aligned |
               Fivetwelve_unaligned), _, _))
  | Op(Load { memory_chunk =
                (Word_int | Word_val | Double | Onetwentyeight_aligned |
                 Onetwentyeight_unaligned | Twofiftysix_aligned |
                 Twofiftysix_unaligned | Fivetwelve_aligned |
                 Fivetwelve_unaligned); _})
  | Op(Specific (Istore_int _))
  | Op(Specific (Ifloatarithmem (Float64, _, _)))
  | Op(Specific (Iprefetch _ | Icldemote _)) ->
    destroyed_at_large_memory_op
  | Op(Intop(Imulh _ | Icomp _) | Intop_imm((Icomp _), _)) ->
    [| rax |]
  | Op (Specific (Irdtsc | Irdpmc)) ->
    [| rax; rdx |]
  | Op Poll -> destroyed_at_alloc_or_poll
  | Op (Alloc _) ->
    destroyed_at_alloc_or_poll
  | Op (Specific Ipackf32) -> [||]
  | Op (Specific (Isimd op)) ->
    destroyed_by_simd_op op
  | Op (Specific (Isimd_mem (op,_))) ->
    destroyed_by_simd_mem_op op
  | Op (Move | Spill | Reload
       | Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
       | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
       | Stackoffset _
       | Intop (Iadd | Isub | Imul | Iand | Ior | Ixor | Ilsl | Ilsr
               | Iasr | Ipopcnt | Iclz _ | Ictz _)
       | Intop_imm ((Iadd | Isub | Imul | Imulh _ | Iand | Ior | Ixor
                    | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _),_)
       | Floatop _
       | Csel _
       | Reinterpret_cast _
       | Static_cast _
       | Probe_is_enabled _
       | Opaque
       | Begin_region
       | End_region
       | Specific (Ilea _ | Ioffset_loc _ | Ibswap _
                  | Isextend32 | Izextend32
                  | Ilfence | Isfence | Imfence)
       | Name_for_debugger _ | Dls_get | Pause)
  | Poptrap _ | Prologue ->
    if fp then [| rbp |] else [||]
  | Stack_check _ ->
    assert false (* the instruction is added after register allocation *)

(* note: keep this function in sync with `is_destruction_point` below. *)
let destroyed_at_terminator (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Return | Raise _ | Tailcall_self  _ | Tailcall_func _
  | Prim {op = Probe _; _}
  ->
    if fp then [| rbp |] else [||]
  | Switch _ ->
    [| rax; rdx |]
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _;
                     stack_ofs; stack_align = _; effects = _; }
  | Prim {op = External { func_symbol = _; alloc; ty_res = _; ty_args = _;
                          stack_ofs; stack_align = _; effects = _; }; _} ->
    assert (stack_ofs >= 0);
    if alloc || stack_ofs > 0 then all_phys_regs else destroyed_at_c_call
  | Call {op = Indirect | Direct _; _} -> all_phys_regs

(* CR-soon xclerc for xclerc: consider having more destruction points.
   We current return `true` when `destroyed_at_terminator` returns
   `all_phys_regs` (which means we are conservative in the sense we will
   spill registers that would spill anyway); we could also return `true`
   when `destroyed_at_terminator` returns `destroyed_at_c_call` for instance. *)
(* note: keep this function in sync with `destroyed_at_terminator` above. *)
let is_destruction_point ~(more_destruction_points : bool) (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Return | Raise _ | Tailcall_self  _ | Tailcall_func _
  | Prim {op = Probe _; _} ->
    false
  | Switch _ ->
    false
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _; _ }
  | Prim {op = External { func_symbol = _; alloc; ty_res = _; ty_args = _; _ }; _} ->
    if more_destruction_points then
      true
    else
      if alloc then true else false
  | Call {op = Indirect | Direct _; _} ->
    true

(* Layout of the stack frame *)

let initial_stack_offset ~num_stack_slots:_ ~contains_calls:_ = 0

let trap_frame_size_in_bytes = 16

let frame_required ~fun_contains_calls ~fun_num_stack_slots =
  fp || fun_contains_calls ||
  Stack_class.Tbl.exists
    fun_num_stack_slots
    ~f:(fun _stack_class num -> num > 0)

let prologue_required ~fun_contains_calls ~fun_num_stack_slots =
  frame_required ~fun_contains_calls ~fun_num_stack_slots

(* returned size includes return address *)
let frame_size ~stack_offset ~contains_calls ~num_stack_slots =
  if frame_required ~fun_contains_calls:contains_calls
     ~fun_num_stack_slots:num_stack_slots
  then begin
    let sz =
      (stack_offset
       + 8
       + Stack_class.Tbl.total_size_in_bytes num_stack_slots
       + (if fp then 8 else 0))
    in Misc.align sz 16
  end else
    stack_offset + 8

type slot_offset =
  | Bytes_relative_to_stack_pointer of int
  | Bytes_relative_to_domainstate_pointer of int

(* CR mshinwell: standardise everywhere on e.g. [contains_calls] instead
   of [fun_contains_calls] *)

let slot_offset loc ~stack_class ~stack_offset ~fun_contains_calls
      ~fun_num_stack_slots =
  match ( loc : Reg.stack_location) with
  | Incoming n ->
      Bytes_relative_to_stack_pointer (
        frame_size ~stack_offset ~contains_calls:fun_contains_calls
          ~num_stack_slots:fun_num_stack_slots
        + n)
  | Local n ->
      Bytes_relative_to_stack_pointer (
        stack_offset + Stack_class.Tbl.offset_in_bytes fun_num_stack_slots ~stack_class ~slot:n
)
  | Outgoing n -> Bytes_relative_to_stack_pointer n
  | Domainstate n ->
      Bytes_relative_to_domainstate_pointer (
        n + Domainstate.(idx_of_field Domain_extra_params) * 8)

(* Calling the assembler *)

let assemble_file infile outfile =
  X86_proc.assemble_file infile outfile

(* Precolored_regs is not always the same as [all_phys_regs], as some physical registers
   may not be allocatable (e.g. rbp when frame pointers are enabled). *)
let precolored_regs () =
  let phys_regs = Reg.set_of_array all_phys_regs in
  if fp then Reg.Set.remove rbp phys_regs else phys_regs

let has_three_operand_float_ops () = Arch.Extension.enabled AVX

let operation_supported = function
  | Cpopcnt -> Arch.Extension.enabled POPCNT
  | Creinterpret_cast (V256_of_vec (Vec128 | Vec256) | V128_of_vec Vec256)
  | Cstatic_cast (V256_of_scalar _ | Scalar_of_v256 _) ->
    Arch.Extension.enabled_vec256 ()
  | Creinterpret_cast (V512_of_vec _ | V128_of_vec Vec512 | V256_of_vec Vec512)
  | Cstatic_cast (V512_of_scalar _ | Scalar_of_v512 _) ->
    Arch.Extension.enabled_vec512 ()
  | Cprefetch _ | Catomic _
  | Capply _ | Cextcall _ | Cload _ | Calloc _ | Cstore _
  | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccsel _
  | Cbswap _
  | Cclz _ | Cctz _
  | Ccmpi _ | Caddv | Cadda | Ccmpa _
  | Cnegf _ | Cabsf _ | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32
  | Ccmpf _
  | Craise _
  | Cprobe _ | Cprobe_is_enabled _ | Copaque | Cbeginregion | Cendregion
  | Ctuple_field _
  | Cdls_get
  | Cpoll
  | Cpause
  | Creinterpret_cast (Int_of_value | Value_of_int |
                       Int64_of_float | Float_of_int64 |
                       Float32_of_float | Float_of_float32 |
                       Float32_of_int32 | Int32_of_float32 |
                       V128_of_vec Vec128)
  | Cstatic_cast (Float_of_float32 | Float32_of_float |
                  Int_of_float Float32 | Float_of_int Float32 |
                  Float_of_int Float64 | Int_of_float Float64 |
                  V128_of_scalar _ | Scalar_of_v128 _) ->
    true

let expression_supported = function
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_vec128 _ | Cconst_symbol _  | Cvar _ | Clet _ | Cphantom_let _
  | Ctuple _ | Cop _ | Csequence _ | Cifthenelse _ | Cswitch _ | Ccatch _
  | Cexit _ -> true
  | Cconst_vec256 _ -> Arch.Extension.enabled_vec256 ()
  | Cconst_vec512 _ -> Arch.Extension.enabled_vec512 ()

let trap_size_in_bytes = 16
