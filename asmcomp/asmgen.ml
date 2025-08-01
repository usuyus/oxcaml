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

(* From lambda to assembly code *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Format
open Config
open Clflags
open Misc
open Cmm
module DLL = Oxcaml_utils.Doubly_linked_list
module String = Misc.Stdlib.String

type error =
  | Assembler_error of string
  | Mismatched_for_pack of Compilation_unit.Prefix.t
  | Asm_generation of string * Emitaux.error

exception Error of error

let cmm_invariants ppf fd_cmm =
  let print_fundecl =
    if !Clflags.dump_cmm
    then Printcmm.fundecl
    else fun ppf fdecl -> Format.fprintf ppf "%s" fdecl.fun_name.sym_name
  in
  if !Clflags.cmm_invariants && Cmm_invariants.run ppf fd_cmm
  then
    Misc.fatal_errorf "Cmm invariants failed on following fundecl:@.%a@."
      print_fundecl fd_cmm;
  fd_cmm

let cfg_invariants ppf cfg =
  let print_fundecl ppf c =
    if !Oxcaml_flags.dump_cfg
    then Cfg_with_layout.dump ppf c ~msg:"*** Cfg invariant check failed"
    else Format.fprintf ppf "%s" (Cfg_with_layout.cfg c).fun_name
  in
  if !Oxcaml_flags.cfg_invariants && Cfg_invariants.run ppf cfg
  then
    Misc.fatal_errorf "Cfg invariants failed on following fundecl:@.%a@."
      print_fundecl cfg;
  cfg

let pass_dump_linear_if ppf flag message phrase =
  if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let pass_dump_cfg_if ppf flag message c =
  if !flag
  then fprintf ppf "*** %s@.%a@." message (Cfg_with_layout.dump ~msg:"") c;
  c

let should_vectorize () =
  !Oxcaml_flags.vectorize && not (Flambda2_ui.Flambda_features.classic_mode ())

let start_from_emit = ref true

let should_save_before_emit () =
  should_save_ir_after Compiler_pass.Linearization && not !start_from_emit

let should_save_cfg_before_emit () =
  should_save_ir_after Compiler_pass.Simplify_cfg && not !start_from_emit

let should_save_cfg_before_regalloc () =
  should_save_ir_before Compiler_pass.Register_allocation

let linear_unit_info =
  { Linear_format.unit = Compilation_unit.dummy; items = [] }

let new_cfg_unit_info () =
  { Cfg_format.unit = Compilation_unit.dummy; items = [] }

let cfg_unit_info = new_cfg_unit_info ()

let cfg_before_regalloc_unit_info = new_cfg_unit_info ()

module Compiler_pass_map = Map.Make (Compiler_pass)

let (pass_to_cfg : Cfg_format.cfg_unit_info Compiler_pass_map.t) =
  Compiler_pass_map.empty
  |> Compiler_pass_map.add Compiler_pass.Selection (new_cfg_unit_info ())

let reset () =
  Zero_alloc_checker.reset_unit_info ();
  start_from_emit := false;
  Compiler_pass_map.iter
    (fun pass (cfg_unit_info : Cfg_format.cfg_unit_info) ->
      if should_save_ir_after pass || should_save_ir_before pass
      then (
        cfg_unit_info.unit <- Compilation_unit.get_current_or_dummy ();
        cfg_unit_info.items <- [];
        cfg_before_regalloc_unit_info.unit
          <- Compilation_unit.get_current_or_dummy ();
        cfg_before_regalloc_unit_info.items <- []))
    pass_to_cfg;
  if should_save_before_emit ()
  then (
    linear_unit_info.unit <- Compilation_unit.get_current_or_dummy ();
    linear_unit_info.items <- []);
  if should_save_cfg_before_emit ()
  then (
    cfg_unit_info.unit <- Compilation_unit.get_current_or_dummy ();
    cfg_unit_info.items <- [])

let save_data dl =
  Compiler_pass_map.iter
    (fun pass (cfg_unit_info : Cfg_format.cfg_unit_info) ->
      if should_save_ir_after pass && not !start_from_emit
      then cfg_unit_info.items <- Cfg_format.(Data dl) :: cfg_unit_info.items)
    pass_to_cfg;
  if should_save_before_emit ()
  then
    linear_unit_info.items <- Linear_format.(Data dl) :: linear_unit_info.items;
  if should_save_cfg_before_emit ()
  then cfg_unit_info.items <- Cfg_format.(Data dl) :: cfg_unit_info.items;
  dl

let save_linear f =
  if should_save_before_emit ()
  then
    linear_unit_info.items <- Linear_format.(Func f) :: linear_unit_info.items;
  f

let save_cfg f =
  if should_save_cfg_before_emit ()
  then cfg_unit_info.items <- Cfg_format.(Cfg f) :: cfg_unit_info.items;
  f

let save_cfg_before_regalloc (cfg_with_infos : Cfg_with_infos.t) =
  (if should_save_cfg_before_regalloc ()
  then
    (* CFGs and registers are mutable, so make sure what we will save is a
       snapshot of the current state. *)
    let copy x = Marshal.from_string (Marshal.to_string x []) 0 in
    cfg_before_regalloc_unit_info.items
      <- Cfg_format.(
           Cfg_before_regalloc
             { cfg_with_layout_and_relocatable_regs =
                 copy
                   ( Cfg_with_infos.cfg_with_layout cfg_with_infos,
                     Reg.all_relocatable_regs () );
               cmm_label = Cmm.cur_label ();
               reg_stamp = Reg.For_testing.get_stamp ()
             })
         :: cfg_before_regalloc_unit_info.items);
  cfg_with_infos

let write_ir prefix =
  Compiler_pass_map.iter
    (fun pass (cfg_unit_info : Cfg_format.cfg_unit_info) ->
      if should_save_ir_after pass && not !start_from_emit
      then (
        let filename = Compiler_pass.(to_output_filename pass ~prefix) in
        cfg_unit_info.items <- List.rev cfg_unit_info.items;
        Cfg_format.save filename cfg_unit_info))
    pass_to_cfg;
  if should_save_before_emit ()
  then (
    let filename = Compiler_pass.(to_output_filename Linearization ~prefix) in
    linear_unit_info.items <- List.rev linear_unit_info.items;
    Linear_format.save filename linear_unit_info);
  if should_save_cfg_before_emit ()
  then (
    let filename = Compiler_pass.(to_output_filename Simplify_cfg ~prefix) in
    cfg_unit_info.items <- List.rev cfg_unit_info.items;
    Cfg_format.save filename cfg_unit_info);
  if should_save_cfg_before_regalloc ()
  then (
    let filename =
      Compiler_pass.(to_output_filename Register_allocation ~prefix)
    in
    cfg_before_regalloc_unit_info.items
      <- List.rev cfg_before_regalloc_unit_info.items;
    Cfg_format.save filename cfg_before_regalloc_unit_info)

let should_emit () = not (should_stop_after Compiler_pass.Linearization)

(* note: `should_use_linscan` relies on the state of the `Reg` module, as the
   list of temporaries is retrieved to be compared to the threshold. *)
let should_use_linscan fd =
  !use_linscan
  || List.mem Cmm.Use_linscan_regalloc fd.fun_codegen_options
  || List.compare_length_with
       (Reg.all_relocatable_regs ())
       !Oxcaml_flags.regalloc_linscan_threshold
     > 0

let if_emit_do f x = if should_emit () then f x else ()

let emit_begin_assembly ~sourcefile unix =
  if !Clflags.llvm_backend
  then Llvmize.begin_assembly ~sourcefile
  else if_emit_do (fun () -> Emit.begin_assembly unix) ()

let emit_end_assembly ~sourcefile () =
  if !Clflags.llvm_backend
  then Llvmize.end_assembly ()
  else
    if_emit_do
      (fun () ->
        try Emit.end_assembly ()
        with Emitaux.Error e ->
          let sourcefile = Option.value ~default:"*none*" sourcefile in
          raise (Error (Asm_generation (sourcefile, e))))
      ()

let emit_data dl =
  if !Clflags.llvm_backend then Llvmize.data dl else if_emit_do Emit.data dl

let emit_fundecl f =
  if !Clflags.llvm_backend
  then Misc.fatal_error "Linear IR not supported with llvm backend";
  if_emit_do
    (fun (fundecl : Linear.fundecl) ->
      try Profile.record ~accumulate:true "emit" Emit.fundecl fundecl
      with Emitaux.Error e ->
        raise (Error (Asm_generation (fundecl.Linear.fun_name, e))))
    f

let count_duplicate_spills_reloads_in_block (block : Cfg.basic_block) =
  let count_per_inst
      ((dup_spills, dup_reloads, seen_spill_regs, seen_reload_regs) as acc)
      (inst : Cfg.basic Cfg.instruction) =
    match inst.desc with
    | Op Spill ->
      let reg = inst.res.(0) in
      let new_dup_spills =
        dup_spills + if Reg.Set.mem reg seen_spill_regs then 1 else 0
      in
      ( new_dup_spills,
        dup_reloads,
        Reg.Set.add reg seen_spill_regs,
        seen_reload_regs )
    | Op Reload ->
      let reg = inst.arg.(0) in
      let new_dup_reloads =
        dup_reloads + if Reg.Set.mem reg seen_reload_regs then 1 else 0
      in
      ( dup_spills,
        new_dup_reloads,
        seen_spill_regs,
        Reg.Set.add reg seen_reload_regs )
    | _ -> acc
  in
  let dup_spills, dup_reloads, _, _ =
    DLL.fold_left block.body ~f:count_per_inst
      ~init:(0, 0, Reg.Set.empty, Reg.Set.empty)
  in
  dup_spills, dup_reloads

let count_spills_reloads (block : Cfg.basic_block) =
  let f ((spills, reloads) as acc) (instr : Cfg.basic Cfg.instruction) =
    match instr.desc with
    | Op Spill -> spills + 1, reloads
    | Op Reload -> spills, reloads + 1
    | _ -> acc
  in
  DLL.fold_left ~f ~init:(0, 0) block.body

(** Returns all CFG counters that work on a single block and are summative over the
    blocks. *)
let cfg_block_counters block =
  let dup_spills, dup_reloads = count_duplicate_spills_reloads_in_block block in
  let spills, reloads = count_spills_reloads block in
  Profile.Counters.create ()
  |> Profile.Counters.set "block_duplicate_spill" dup_spills
  |> Profile.Counters.set "block_duplicate_reload" dup_reloads
  |> Profile.Counters.set "spill" spills
  |> Profile.Counters.set "reload" reloads

(** Returns all CFG counters that require the whole CFG to produce a count. *)
let whole_cfg_counters (_ : Cfg.t) = Profile.Counters.create ()

let cfg_profile to_cfg =
  let total_counters = ref (Profile.Counters.create ()) in
  let block_f label block =
    match !Clflags.profile_granularity with
    | Block_level ->
      let (_ : Cfg.basic_block) =
        Profile.record_with_counters ~accumulate:true
          ~counter_f:cfg_block_counters
          (Format.sprintf "block=%s" (Label.to_string label))
          Fun.id block
      in
      ()
    | File_level | Function_level ->
      (* Manual counter accumulation to circumvent needing to register block as
         pass *)
      total_counters
        := Profile.Counters.union !total_counters (cfg_block_counters block)
  in
  let counter_f x =
    let cfg = to_cfg x in
    Cfg.iter_blocks cfg ~f:block_f;
    Profile.Counters.union !total_counters (whole_cfg_counters cfg)
  in
  Profile.record_with_counters ~counter_f

let cfg_with_layout_profile ?accumulate pass f x =
  cfg_profile Cfg_with_layout.cfg ?accumulate pass f x

let cfg_with_infos_profile ?accumulate pass f x =
  cfg_profile Cfg_with_infos.cfg ?accumulate pass f x

let ( ++ ) x f = f x

let reorder_blocks_random ppf_dump cl =
  match !Oxcaml_flags.reorder_blocks_random with
  | None -> cl
  | Some seed ->
    (* Initialize random state based on user-provided seed and function name.
       Per-function random state (instead of per call to ocamlopt) is good for
       debugging: it gives us deterministic builds for each user-provided seed,
       regardless of the order of files on the command line. *)
    let fun_name = (Cfg_with_layout.cfg cl).fun_name in
    let random_state = Random.State.make [| seed; Hashtbl.hash fun_name |] in
    Cfg_with_layout.reorder_blocks_random ~random_state cl;
    pass_dump_cfg_if ppf_dump Oxcaml_flags.dump_cfg
      "After reorder_blocks_random" cl

type register_allocator =
  | GI
  | IRC
  | LS

let register_allocator fd : register_allocator =
  match String.lowercase_ascii !Oxcaml_flags.regalloc with
  | "" | "cfg" -> if should_use_linscan fd then LS else IRC
  | "gi" -> GI
  | "irc" -> IRC
  | "ls" -> LS
  | other -> Misc.fatal_errorf "unknown register allocator (%S)" other

let available_regs ~stack_slots ~f x =
  (* Skip DWARF variable range generation for complicated functions to avoid
     high compilation speed penalties *)
  let fun_num_stack_slots = stack_slots x in
  let total_num_stack_slots =
    Stack_class.Tbl.fold fun_num_stack_slots ~init:0
      ~f:(fun _stack_class num acc -> acc + num)
  in
  if total_num_stack_slots > !Dwarf_flags.dwarf_max_function_complexity
  then x
  else f x

let compile_cfg ppf_dump ~funcnames fd_cmm cfg_with_layout =
  let register_allocator = register_allocator fd_cmm in
  let module CSE = Cfg_cse.Cse_generic (CSE) in
  cfg_with_layout
  ++ (fun cfg_with_layout ->
       match should_vectorize () with
       | false -> cfg_with_layout
       | true ->
         cfg_with_layout
         ++ cfg_with_layout_profile ~accumulate:true "vectorize"
              (Vectorize.cfg ppf_dump)
         ++ pass_dump_cfg_if ppf_dump Oxcaml_flags.dump_cfg "After vectorize")
  ++ cfg_with_layout_profile ~accumulate:true "cfg_polling"
       (Cfg_polling.instrument_fundecl ~future_funcnames:funcnames)
  ++ cfg_with_layout_profile ~accumulate:true "cfg_zero_alloc_checker"
       (Zero_alloc_checker.cfg ~future_funcnames:funcnames ppf_dump)
  ++ cfg_with_layout_profile ~accumulate:true "cfg_comballoc" Cfg_comballoc.run
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cfg_combine
  ++ cfg_with_layout_profile ~accumulate:true "cfg_cse" CSE.cfg_with_layout
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cfg_cse
  ++ Cfg_with_infos.make
  ++ cfg_with_infos_profile ~accumulate:true "cfg_deadcode" Cfg_deadcode.run
  ++ save_cfg_before_regalloc
  ++ Profile.record ~accumulate:true "regalloc" (fun cfg_with_infos ->
         let cfg_description =
           Regalloc_validate.Description.create
             (Cfg_with_infos.cfg_with_layout cfg_with_infos)
         in
         cfg_with_infos
         ++ (match register_allocator with
            | GI ->
              cfg_with_infos_profile ~accumulate:true "cfg_gi" Regalloc_gi.run
            | IRC ->
              cfg_with_infos_profile ~accumulate:true "cfg_irc" Regalloc_irc.run
            | LS ->
              cfg_with_infos_profile ~accumulate:true "cfg_ls" Regalloc_ls.run)
         ++ Cfg_with_infos.cfg_with_layout
         ++ cfg_with_layout_profile ~accumulate:true "cfg_validate_description"
              (Regalloc_validate.run cfg_description))
  ++ Profile.record ~accumulate:true "cfg_available_regs"
       (available_regs
          ~stack_slots:(fun x ->
            (Cfg_with_layout.cfg x).Cfg.fun_num_stack_slots)
          ~f:Cfg_available_regs.run)
  ++ Profile.record ~accumulate:true "cfg_invariants" (cfg_invariants ppf_dump)
  ++ cfg_with_layout_profile ~accumulate:true "cfg_simplify"
       Regalloc_utils.simplify_cfg
  ++ Profile.record ~accumulate:true "cfg_invariants" (cfg_invariants ppf_dump)
  (* CR-someday gtulbalecu: The peephole optimizations must not affect liveness,
     otherwise we would have to recompute it here. Recomputing it here breaks
     the CI because the liveness_analysis algorithm does not work properly after
     register allocation. *)
  ++ cfg_with_layout_profile ~accumulate:true "peephole_optimize_cfg"
       Peephole_optimize.peephole_optimize_cfg
  ++ (fun (cfg_with_layout : Cfg_with_layout.t) ->
       match !Oxcaml_flags.cfg_stack_checks with
       | false -> cfg_with_layout
       | true -> Cfg_stack_checks.cfg cfg_with_layout)
  ++ cfg_with_layout_profile ~accumulate:true "save_cfg" save_cfg
  ++ cfg_with_layout_profile ~accumulate:true "cfg_reorder_blocks"
       (reorder_blocks_random ppf_dump)
  ++ Profile.record ~accumulate:true "cfg_invariants" (cfg_invariants ppf_dump)
  ++ Profile.record ~accumulate:true "cfg_to_linear" Cfg_to_linear.run

let compile_via_llvm ~ppf_dump ~funcnames cfg_with_layout =
  (* missing pass: stack checks *)
  cfg_with_layout
  ++ cfg_with_layout_profile ~accumulate:true "cfg_polling"
       (Cfg_polling.instrument_fundecl ~future_funcnames:funcnames)
  ++ cfg_with_layout_profile ~accumulate:true "cfg_zero_alloc_checker"
       (Zero_alloc_checker.cfg ~future_funcnames:funcnames ppf_dump)
  ++ cfg_with_layout_profile ~accumulate:true "cfg_comballoc" Cfg_comballoc.run
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cfg_combine
  ++ pass_dump_cfg_if ppf_dump Oxcaml_flags.dump_cfg "After comballoc"
  ++ Profile.record ~accumulate:true "save_cfg" save_cfg
  ++ Profile.record ~accumulate:true "llvmize" Llvmize.cfg

let compile_via_linear ~ppf_dump ~funcnames fd_cmm cfg_with_layout =
  cfg_with_layout
  ++ compile_cfg ppf_dump ~funcnames fd_cmm
  ++ pass_dump_linear_if ppf_dump dump_linear "Linearized code"
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Linear
  ++ Profile.record ~accumulate:true "save_linear" save_linear
  ++ (fun (fd : Linear.fundecl) ->
       match !Oxcaml_flags.cfg_stack_checks with
       | false -> Stack_check.linear fd
       | true -> fd)
  ++ Profile.record ~accumulate:true "emit_fundecl" emit_fundecl

let compile_fundecl ~ppf_dump ~funcnames fd_cmm =
  let module Cfg_selection = Cfg_selectgen.Make (Cfg_selection) in
  Reg.clear_relocatable_regs ();
  fd_cmm
  ++ Profile.record ~accumulate:true "cmm_invariants" (cmm_invariants ppf_dump)
  ++ (fun (fd_cmm : Cmm.fundecl) ->
       Cfg_selection.emit_fundecl ~future_funcnames:funcnames fd_cmm
       ++ pass_dump_cfg_if ppf_dump Oxcaml_flags.dump_cfg "After selection")
  ++ Profile.record ~accumulate:true "cfg_invariants" (cfg_invariants ppf_dump)
  ++ Profile.record ~accumulate:true "cfg" (fun cfg_with_layout ->
         if !Clflags.llvm_backend
         then compile_via_llvm ~ppf_dump ~funcnames cfg_with_layout
         else compile_via_linear ~ppf_dump ~funcnames fd_cmm cfg_with_layout)

let compile_data dl = dl ++ save_data ++ emit_data

let compile_phrases ~ppf_dump ps =
  let funcnames =
    List.fold_left
      (fun s p ->
        match p with
        | Cfunction fd -> String.Set.add fd.fun_name.sym_name s
        | Cdata _ -> s)
      String.Set.empty ps
  in
  let rec compile ~funcnames ps =
    match ps with
    | [] -> ()
    | p :: ps -> (
      if !dump_cmm then fprintf ppf_dump "%a@." Printcmm.phrase p;
      match p with
      | Cfunction fd ->
        (* Only profile if selected granularity is either function or block
           level *)
        let profile_wrapper =
          match !profile_granularity with
          | Function_level | Block_level ->
            Profile.record ~accumulate:true
              ("function=" ^ X86_proc.string_of_symbol "" fd.fun_name.sym_name)
          | File_level -> Fun.id
        in
        profile_wrapper (compile_fundecl ~ppf_dump ~funcnames) fd;
        compile ~funcnames:(String.Set.remove fd.fun_name.sym_name funcnames) ps
      | Cdata dl ->
        compile_data dl;
        compile ~funcnames ps)
  in
  compile ~funcnames ps

let compile_phrase ~ppf_dump p = compile_phrases ~ppf_dump [p]

(* For the native toplevel: generates generic functions unless they are already
   available in the process *)
let compile_genfuns ~ppf_dump f =
  List.iter
    (function
      | Cfunction { fun_name = name } as ph when f name.sym_name ->
        compile_phrase ~ppf_dump ph
      | _ -> ())
    (Generic_fns.compile ~cache:false ~shared:true
       (Generic_fns.Tbl.of_fns (Compilenv.current_unit_infos ()).ui_generic_fns))

let compile_unit ~output_prefix ~asm_filename ~keep_asm ~obj_filename
    ~may_reduce_heap ~ppf_dump gen =
  reset ();
  let create_asm =
    should_emit () && (keep_asm || not !Emitaux.binary_backend_available)
  in
  X86_proc.create_asm_file := create_asm;
  let remove_asm_file () =
    (* if [should_emit ()] is [false] then no assembly is generated, so the
       (empty) temporary file should be deleted. *)
    if (not create_asm) || not keep_asm then remove_file asm_filename
  in
  if !Clflags.llvm_backend then Llvmize.init ~output_prefix ~ppf_dump;
  let open_asm_file () =
    if create_asm
    then
      if !Clflags.llvm_backend
      then Llvmize.open_out ~asm_filename
      else Emitaux.output_channel := open_out asm_filename
  in
  let close_asm_file () =
    if create_asm
    then
      if !Clflags.llvm_backend
      then Llvmize.close_out ()
      else close_out !Emitaux.output_channel
  in
  let assemble_file () =
    if !Clflags.llvm_backend
    then Llvmize.assemble_file ~asm_filename ~obj_filename
    else if not (should_emit ())
    then 0
    else (
      if may_reduce_heap
      then
        Emitaux.reduce_heap_size ~reset:(fun () ->
            reset ();
            (* note: we need to preserve the persistent env, because it is used
               to populate fields of the record written as the cmx file
               afterwards. *)
            Typemod.reset ~preserve_persistent_env:true;
            Emitaux.reset ();
            Reg.clear_relocatable_regs ());
      Proc.assemble_file asm_filename obj_filename)
  in
  Misc.try_finally
    ~exceptionally:(fun () -> remove_file obj_filename)
    (fun () ->
      open_asm_file ();
      Misc.try_finally
        (fun () ->
          gen ();
          Zero_alloc_checker.record_unit_info ppf_dump;
          Compiler_hooks.execute Compiler_hooks.Check_allocations
            Zero_alloc_checker.iter_witnesses;
          write_ir output_prefix)
        ~always:(fun () -> close_asm_file ())
        ~exceptionally:remove_asm_file;
      let assemble_result = Profile.record_call "assemble" assemble_file in
      if assemble_result <> 0 then raise (Error (Assembler_error asm_filename));
      remove_asm_file ())

let end_gen_implementation unix ?toplevel ~ppf_dump ~sourcefile make_cmm =
  Emitaux.Dwarf_helpers.init ~disable_dwarf:false ~sourcefile;
  emit_begin_assembly ~sourcefile unix;
  ( make_cmm ()
  ++ (fun x ->
       if Clflags.should_stop_after Compiler_pass.Middle_end then exit 0 else x)
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cmm
  ++ Profile.record "compile_phrases" (compile_phrases ~ppf_dump)
  ++ fun () -> () );
  (match toplevel with None -> () | Some f -> compile_genfuns ~ppf_dump f);
  (* We add explicit references to external primitive symbols. This is to ensure
     that the object files that define these symbols, when part of a C library,
     won't be discarded by the linker. This is important if a module that uses
     such a symbol is later dynlinked. *)
  compile_phrase ~ppf_dump
    (Cmm_helpers.reference_symbols
       (List.filter_map
          (fun prim ->
            if not (Primitive.native_name_is_external prim)
            then None
            else Some (Cmm.global_symbol (Primitive.native_name prim)))
          !Translmod.primitive_declarations));
  emit_end_assembly ~sourcefile ()

type direct_to_cmm =
  ppf_dump:Format.formatter ->
  prefixname:string ->
  Lambda.program ->
  Cmm.phrase list

type pipeline = Direct_to_cmm of direct_to_cmm

let asm_filename output_prefix =
  if !keep_asm_file || !Emitaux.binary_backend_available
  then output_prefix ^ ext_asm
  else Filename.temp_file "camlasm" ext_asm

let compile_implementation unix ?toplevel ~pipeline ~sourcefile ~prefixname
    ~ppf_dump (program : Lambda.program) =
  compile_unit ~ppf_dump ~output_prefix:prefixname
    ~asm_filename:(asm_filename prefixname) ~keep_asm:!keep_asm_file
    ~obj_filename:(prefixname ^ ext_obj)
    ~may_reduce_heap:(Option.is_none toplevel) (fun () ->
      Compilation_unit.Set.iter Compilenv.require_global
        program.required_globals;
      Compilenv.record_external_symbols ();
      match pipeline with
      | Direct_to_cmm direct_to_cmm ->
        let cmm_phrases = direct_to_cmm ~ppf_dump ~prefixname program in
        end_gen_implementation unix ?toplevel ~ppf_dump ~sourcefile (fun () ->
            cmm_phrases))

let linear_gen_implementation unix filename =
  let open Linear_format in
  let linear_unit_info, _ = restore filename in
  let current_package = Compilation_unit.Prefix.from_clflags () in
  let saved_package = Compilation_unit.for_pack_prefix linear_unit_info.unit in
  if not (Compilation_unit.Prefix.equal current_package saved_package)
  then raise (Error (Mismatched_for_pack saved_package));
  let emit_item = function
    | Data dl -> emit_data dl
    | Func f -> emit_fundecl f
  in
  start_from_emit := true;
  (* CR mshinwell: set [sourcefile] properly; [filename] isn't a .ml file *)
  let sourcefile = Some filename in
  Emitaux.Dwarf_helpers.init ~disable_dwarf:false ~sourcefile;
  emit_begin_assembly ~sourcefile unix;
  Profile.record "Emit" (List.iter emit_item) linear_unit_info.items;
  emit_end_assembly ~sourcefile ()

let compile_implementation_linear unix output_prefix ~progname =
  compile_unit ~may_reduce_heap:true ~output_prefix
    ~asm_filename:(asm_filename output_prefix)
    ~keep_asm:!keep_asm_file ~obj_filename:(output_prefix ^ ext_obj) (fun () ->
      linear_gen_implementation unix progname)

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
    fprintf ppf "Assembler error, input left in file %a" Location.print_filename
      file
  | Mismatched_for_pack saved ->
    let msg prefix =
      if Compilation_unit.Prefix.is_empty prefix
      then "without -for-pack"
      else "with -for-pack " ^ Compilation_unit.Prefix.to_string prefix
    in
    fprintf ppf "This input file cannot be compiled %s: it was generated %s."
      (msg (Compilation_unit.Prefix.from_clflags ()))
      (msg saved)
  | Asm_generation (fn, err) ->
    fprintf ppf "Error producing assembly code for %s: %a" fn
      Emitaux.report_error err

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
