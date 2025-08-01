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

(* The interactive toplevel loop *)

#18 "toplevel/byte/topeval.ml"

open Format
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree
open Topcommon
module String = Misc.Stdlib.String

(* The table of toplevel value bindings and its accessors *)

let toplevel_value_bindings : Obj.t String.Map.t ref = ref String.Map.empty

let getvalue name =
  try
    String.Map.find name !toplevel_value_bindings
  with Not_found ->
    fatal_error (name ^ " unbound at toplevel")

let setvalue name v =
  toplevel_value_bindings := String.Map.add name v !toplevel_value_bindings

let implementation_label = ""

(* To print values *)

module EvalBase = struct

  let eval_compilation_unit cu =
    try
      Symtable.get_global_value (Symtable.Global.of_compilation_unit cu)
    with Symtable.Error (Undefined_global global) ->
      raise (Undefined_global (Symtable.Global.name global))

  let eval_ident id =
    let name = Translmod.toplevel_name id in
    try
      String.Map.find name !toplevel_value_bindings
    with Not_found ->
      raise (Undefined_global name)

end

include Topcommon.MakeEvalPrinter(EvalBase)

(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)

let load_lambda ppf lam =
  if !Clflags.dump_debug_uid_tables then Type_shape.print_debug_uid_tables ppf;
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;
  let blam = Blambda_of_lambda.blambda_of_lambda slam in
  if !Clflags.dump_blambda then fprintf ppf "%a@." Printblambda.blambda blam;
  let instrs, can_free = Bytegen.compile_phrase blam in
  if !Clflags.dump_instr then
    fprintf ppf "%a@."
    Printinstr.instrlist instrs;
  let (code, reloc, events) =
    Emitcode.to_memory instrs
  in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.check_global_initialized reloc;
  Symtable.update_global_table();
  let initial_bindings = !toplevel_value_bindings in
  let bytecode, closure = Meta.reify_bytecode code [| events |] None in
  match
    may_trace := true;
    closure ()
  with
  | retval ->
    may_trace := false;
    if can_free then Meta.release_bytecode bytecode;

    Result retval
  | exception x ->
    may_trace := false;
    record_backtrace ();
    if can_free then Meta.release_bytecode bytecode;

    toplevel_value_bindings := initial_bindings; (* PR#6211 *)
    Symtable.restore_state initial_symtable;
    Exception x

(* Print the outcome of an evaluation *)

let pr_item =
  Printtyp.print_items
    (fun env -> function
      | Sig_value(id, {val_kind = Val_reg; val_type}, _) ->
          Some (outval_of_value env (getvalue (Translmod.toplevel_name id))
                  val_type)
      | _ -> None
    )

(* Execute a toplevel phrase *)

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      let oldsig = !toplevel_sig in
      Typecore.reset_delayed_checks ();
      let (str, sg, sn, shape, newenv) =
        Typemod.type_toplevel_phrase oldenv oldsig sstr
      in
      if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
      let sg' = Typemod.Signature_names.simplify newenv sn sg in
      let modes = Includemod.modes_toplevel in
      Includemod.check_implementation oldenv ~modes sg sg';
      Typecore.force_delayed_checks ();
      let shape = Shape_reduce.local_reduce Env.empty shape in
      if !Clflags.dump_shape then Shape.print ppf shape;
      let lam = Translmod.transl_toplevel_definition str in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        toplevel_sig := List.rev_append sg' oldsig;
        let res = load_lambda ppf lam in
        let out_phr =
          match res with
          | Result v ->
              if print_outcome then
                Printtyp.wrap_printing_env ~error:false oldenv (fun () ->
                  match str.str_items with
                  | [] -> Ophr_signature []
                  | _ ->
                      match find_eval_phrase str with
                      | Some (exp, _, _, _) ->
                        let outv = outval_of_value newenv v exp.exp_type in
                        let ty = Printtyp.tree_of_type_scheme exp.exp_type in
                        Ophr_eval (outv, ty)
                      | None -> Ophr_signature (pr_item oldenv sg'))
              else Ophr_signature []
          | Exception exn ->
              toplevel_env := oldenv;
              toplevel_sig := oldsig;
              if exn = Out_of_memory then Gc.full_major();
              let outv =
                outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
              in
              Ophr_exception (exn, outv)
        in
        begin match out_phr with
        | Ophr_signature [] -> ()
        | _ ->
            Location.separate_new_message ppf;
            !print_out_phrase ppf out_phr;
        end;
        if Printexc.backtrace_status ()
        then begin
          match !backtrace with
            | None -> ()
            | Some b ->
                Location.separate_new_message ppf;
                pp_print_string ppf b;
                pp_print_flush ppf ();
                backtrace := None;
        end;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; toplevel_sig := oldsig; raise x
      end
  | Ptop_dir {pdir_name = {Location.txt = dir_name}; pdir_arg } ->
      try_run_directive ppf dir_name pdir_arg

let execute_phrase print_outcome ppf phr =
  try execute_phrase print_outcome ppf phr
  with exn ->
    Warnings.reset_fatal ();
    raise exn


(* Additional directives for the bytecode toplevel only *)

open Cmo_format

(* Loading files *)

exception Load_failed

let check_consistency ppf filename cu =
  try Env.import_crcs ~source:filename cu.cu_imports
  with Persistent_env.Consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    fprintf ppf "@[<hv 0>The files %s@ and %s@ \
                 disagree over interface %a@]@."
            user auth Compilation_unit.Name.print name;
    raise Load_failed

(* This is basically Dynlink.Bytecode.run with no digest *)
let load_compunit ic filename ppf compunit =
  check_consistency ppf filename compunit;
  seek_in ic compunit.cu_pos;
  let code = LongString.input_bytes ic compunit.cu_codesize in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code compunit.cu_reloc;
  Symtable.update_global_table();
  let events =
    if compunit.cu_debug = 0 then [| |]
    else begin
      seek_in ic compunit.cu_debug;
      [| input_value ic |]
    end in
  begin try
    may_trace := true;
    let _bytecode, closure = Meta.reify_bytecode code events None in
    ignore (closure ());
    may_trace := false;
  with exn ->
    record_backtrace ();
    may_trace := false;
    Symtable.restore_state initial_symtable;
    print_exception_outcome ppf exn;
    raise Load_failed
  end

let rec load_file recursive ppf name =
  let filename =
    try Some (Load_path.find name) with Not_found -> None
  in
  match filename with
  | None -> fprintf ppf "Cannot find file %s.@." name; false
  | Some filename ->
      let ic = open_in_bin filename in
      Misc.try_finally
        ~always:(fun () -> close_in ic)
        (fun () -> really_load_file recursive ppf name filename ic)

and really_load_file recursive ppf name filename ic =
  let buffer = really_input_string ic (String.length Config.cmo_magic_number) in
  try
    if buffer = Config.cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let cu : compilation_unit_descr = input_value ic in
      if recursive then
        List.iter
          (fun (reloc, _) -> match reloc with
            | Reloc_getcompunit cu
              when not (Symtable.is_global_defined
                (Symtable.Global.Glob_compunit cu)) ->
                let file =
                  (Compilation_unit.Name.to_string (Compilation_unit.name cu))
                  ^ ".cmo"
                in
                begin match Load_path.find_normalized file with
                | exception Not_found -> ()
                | file ->
                    if not (load_file recursive ppf file) then raise Load_failed
                end
            | Reloc_getcompunit _
            | Reloc_literal _ | Reloc_getpredef _ | Reloc_setcompunit _
            | Reloc_primitive _ -> ()
          )
          cu.cu_reloc;
      load_compunit ic filename ppf cu;
      true
    end else
      if buffer = Config.cma_magic_number then begin
        let toc_pos = input_binary_int ic in  (* Go to table of contents *)
        seek_in ic toc_pos;
        let lib = (input_value ic : library) in
        List.iter
          (fun dllib ->
            let name = Dll.extract_dll_name dllib in
            try Dll.open_dlls Dll.For_execution [name]
            with Failure reason ->
              fprintf ppf
                "Cannot load required shared library %s.@.Reason: %s.@."
                name reason;
              raise Load_failed)
          lib.lib_dllibs;
        List.iter (load_compunit ic filename ppf) lib.lib_units;
        true
      end else begin
        fprintf ppf "File %s is not a bytecode object file.@." name;
        false
      end
  with Load_failed -> false

external get_bytecode_sections : unit -> Symtable.bytecode_sections =
  "caml_dynlink_get_bytecode_sections"

let init () =
  let crc_intfs = Symtable.init_toplevel ~get_bytecode_sections in
  Compmisc.init_path ();
  Env.import_crcs ~source:Sys.executable_name crc_intfs;
  ()
