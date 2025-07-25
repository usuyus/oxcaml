(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*         Mehdi Dogguy, PPS laboratory, University Paris Diderot         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2010 Mehdi Dogguy                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR-someday lmaurer: This file should do no parsing or low-level binary I/O
   _whatsoever_. No magic numbers, no sections, and _especially_ no
   [input_value]. Any such code here is necessarily duplicated code, and worse,
   particularly fiddly duplicated code that segfaults rather than producing
   compile-time errors. *)

(* Dump info on .cmi, .cmo, .cmx, .cma, .cmxa, .cmxs files and on bytecode
   executables. *)

open Printf
open Cmo_format

(* Command line options to prevent printing approximation,
   function code and CRC
 *)
let quiet = ref false
let no_approx = ref false

let no_code = ref false

let no_crc = ref false
let shape = ref false
let index = ref false
let decls = ref false
let uid_deps = ref false

module Magic_number = Misc.Magic_number
module String = Misc.Stdlib.String

let dummy_crc = String.make 32 '-'

let null_crc = String.make 32 '0'

let string_of_crc crc = if !no_crc then null_crc else Digest.to_hex crc

let print_cu_without_prefix oc cu =
  (* Drop the pack prefix for backward compatibility, but keep the instance
     arguments *)
  let cu_without_prefix =
    Compilation_unit.with_for_pack_prefix cu Compilation_unit.Prefix.empty
  in
  Compilation_unit.output oc cu_without_prefix

let print_with_crc ~print_name name crco =
  let crc =
    match crco with
      None -> dummy_crc
    | Some crc -> string_of_crc crc
  in
    printf "\t%s\t%a\n" crc print_name name

let print_name_crc = print_with_crc ~print_name:Compilation_unit.Name.output

let print_cu_crc = print_with_crc ~print_name:print_cu_without_prefix

(* CR-someday mshinwell: consider moving to [Import_info.print] *)

let print_intf_import import =
  let name = Import_info.name import in
  let crco = Import_info.crc import in
  print_name_crc name crco

let print_impl_import import =
  let name = Import_info.cu import in
  let crco = Import_info.crc import in
  print_cu_crc name crco

let print_global_name_binding global =
  printf "\t%a\n" Global_module.With_precision.output global

let print_line name =
  printf "\t%s\n" name

let print_global_as_name_line glob =
  printf "\t%a\n" Global_module.Name.output (Global_module.to_name glob)

let print_parameter_name_line name =
  printf "\t%a\n" Global_module.Parameter_name.output name

let print_name_line cu =
  printf "\t%a\n" print_cu_without_prefix cu

let print_runtime_param p =
  match (p : Lambda.runtime_param) with
  | Rp_argument_block glob
  | Rp_main_module_block glob -> print_global_as_name_line glob
  | Rp_unit -> print_line "()"

let print_main_module_block_format mbf =
  match (mbf : Lambda.main_module_block_format) with
  | Mb_struct _ -> ()
  | Mb_instantiating_functor { mb_runtime_params = params; _ } ->
    print_string "Runtime parameters:\n";
    List.iter print_runtime_param params

let print_required_global id =
  printf "\t%a\n" Compilation_unit.output id

let print_arg_descr arg_descr =
  let ({ arg_param; arg_block_idx = _ } : Lambda.arg_descr) = arg_descr in
  printf "Parameter implemented: %a\n"
    Global_module.Parameter_name.output arg_param

let print_cmo_infos cu =
  printf "Unit name: %a\n" Compilation_unit.output cu.cu_name;
  Option.iter print_arg_descr cu.cu_arg_descr;
  print_string "Interfaces imported:\n";
  Array.iter print_intf_import cu.cu_imports;
  print_main_module_block_format cu.cu_format;
  print_string "Required globals:\n";
  List.iter print_required_global cu.cu_required_compunits;
  printf "Uses unsafe features: ";
  (match cu.cu_primitives with
    | [] -> printf "no\n"
    | l  ->
        printf "YES\n";
        printf "Primitives declared in this module:\n";
        List.iter print_line l);
  printf "Force link: %s\n" (if cu.cu_force_link then "YES" else "no")

let print_spaced_string s =
  printf " %s" s

let print_cma_infos (lib : Cmo_format.library) =
  printf "Force custom: %s\n" (if lib.lib_custom then "YES" else "no");
  printf "Extra C object files:";
  (* PR#4949: print in linking order *)
  List.iter print_spaced_string (List.rev lib.lib_ccobjs);
  printf "\nExtra C options:";
  List.iter print_spaced_string (List.rev lib.lib_ccopts);
  printf "\n";
  print_string "Extra dynamically-loaded libraries:";
  List.iter print_spaced_string (List.rev lib.lib_dllibs);
  printf "\n";
  List.iter print_cmo_infos lib.lib_units

let print_cmi_infos name crcs kind params global_name_bindings =
  if not !quiet then begin
    let open Cmi_format in
    printf "Unit name: %a\n" Compilation_unit.Name.output name;
    let is_param =
      match kind with
      | Normal _ -> false
      | Parameter -> true
    in
    printf "Is parameter: %s\n" (if is_param then "YES" else "no");
    print_string "Parameters:\n";
    List.iter print_parameter_name_line params;
    begin
      match kind with
      | Normal { cmi_arg_for = Some arg_for; _ } ->
        printf "Argument for parameter:\n";
        print_parameter_name_line arg_for
      | Normal _ | Parameter ->
        ()
    end;
    printf "Interfaces imported:\n";
    Array.iter print_intf_import crcs;
    printf "Globals in scope:\n";
    Array.iter print_global_name_binding global_name_bindings
  end

let print_cmt_infos cmt =
  let open Cmt_format in
  if not !quiet then begin
    printf "Cmt unit name: %a\n" Compilation_unit.output cmt.cmt_modname;
    print_string "Cmt interfaces imported:\n";
    Array.iter print_intf_import cmt.cmt_imports;
    printf "Source file: %s\n"
          (match cmt.cmt_sourcefile with None -> "(none)" | Some f -> f);
    printf "Compilation flags:";
    Array.iter print_spaced_string cmt.cmt_args;
    printf "\nLoad path:\n  Visible:";
    List.iter print_spaced_string cmt.cmt_loadpath.visible;
    printf "\n  Hidden:";
    List.iter print_spaced_string cmt.cmt_loadpath.hidden;
    printf "\n";
    printf "cmt interface digest: %s\n"
      (match cmt.cmt_interface_digest with
      | None -> ""
      | Some crc -> string_of_crc crc);
  end;
  if !shape then begin
    printf "Implementation shape: ";
    (match cmt.cmt_impl_shape with
    | None -> printf "(none)\n"
    | Some shape -> Format.printf "\n%a" Shape.print shape)
  end;
  if !index then begin
    printf "Indexed shapes:\n";
    Array.iter (fun (loc, item) ->
      let pp_loc fmt { Location.txt; loc } =
        Format.fprintf fmt "%a (%a)"
          Pprintast.longident txt Location.print_loc loc
      in
      Format.printf "@[<hov 2>%a:@ %a@]@;"
        Shape_reduce.print_result item pp_loc loc)
      cmt.cmt_ident_occurrences;
    Format.print_flush ()
  end;
  if !uid_deps then begin
    printf "\nUid dependencies:\n";
    let arr = Array.of_list cmt.cmt_declaration_dependencies in
    let () =
      Array.sort (fun (_tr, u1, u2) (_tr', u1', u2') ->
                    match Shape.Uid.compare u1 u1' with
                    | 0 -> Shape.Uid.compare u2 u2'
                    | n -> n) arr
    in
    Format.printf "@[<v>";
    Array.iter (fun (rk, u1, u2) ->
      let rk = match rk with
        | Definition_to_declaration -> "<-"
        | Declaration_to_declaration -> "<->"
      in
      Format.printf "@[<h>%a %s %a@]@;"
        Shape.Uid.print u1
        rk
        Shape.Uid.print u2) arr;
    Format.printf "@]";
  end;
  if !decls then begin
    printf "\nUid of decls:\n";
    let decls = Array.of_list (Shape.Uid.Tbl.to_list cmt.cmt_uid_to_decl) in
    Array.sort (fun (uid, _) (uid', _) -> Shape.Uid.compare uid uid') decls;
    Array.iter (fun (uid, item) ->
      let loc = match (item : Typedtree.item_declaration) with
        | Value vd -> vd.val_name
        | Value_binding vb ->
          let (_, name, _, _) =
            List.hd (Typedtree.let_bound_idents_full [vb])
          in
          name
        | Type td -> td.typ_name
        | Constructor cd -> cd.cd_name
        | Extension_constructor ec -> ec.ext_name
        | Label ld -> ld.ld_name
        | Module md ->
          { md.md_name with
            txt = Option.value md.md_name.txt ~default:"_" }
        | Module_substitution ms -> ms.ms_name
        | Module_binding mb ->
          { mb.mb_name with
            txt = Option.value mb.mb_name.txt ~default:"_" }
        | Module_type mtd -> mtd.mtd_name
        | Class cd -> cd.ci_id_name
        | Class_type ctd -> ctd.ci_id_name
      in
      let pp_loc fmt { Location.txt; loc } =
        Format.fprintf fmt "%s (%a)"
           txt Location.print_loc loc
      in
      Format.printf "@[<hov 2>%a:@ %a@]@;"
        Shape.Uid.print uid
        pp_loc loc)
      decls;
    Format.print_flush ()
  end

let print_cms_infos cms =
  let open Cms_format in
  printf "Cms unit name: %a\n" Compilation_unit.output cms.cms_modname;
  printf "Source file: %s\n"
    (match cms.cms_sourcefile with None -> "(none)" | Some f -> f)

let print_general_infos print_name name crc defines arg_descr mbf
    iter_cmi iter_cmx =
  printf "Name: %a\n" print_name name;
  printf "CRC of implementation: %s\n" (string_of_crc crc);
  printf "Globals defined:\n";
  List.iter print_name_line defines;
  Option.iter print_arg_descr arg_descr;
  printf "Interfaces imported:\n";
  iter_cmi print_intf_import;
  printf "Implementations imported:\n";
  iter_cmx print_impl_import;
  Option.iter print_main_module_block_format mbf

let print_global_table table =
  printf "Globals defined:\n";
  Symtable.iter_global_map (fun id _ -> print_line (Symtable.Global.name id))
    table

open Cmx_format
open Cmxs_format

let unique_arity_identifier arity =
  if List.for_all (function [|Cmm.Val|] -> true | _ -> false) arity then
    Int.to_string (List.length arity)
  else
    String.concat "_" (List.map Cmm_helpers.machtype_identifier arity)

let return_arity_identifier t =
  match t with
  | [|Cmm.Val|] -> ""
  | _ -> "_R" ^ Cmm_helpers.machtype_identifier t

let print_generic_fns gfns =
  let pr_afuns _ fns =
    let mode = function Cmx_format.Alloc_heap -> "" | Cmx_format.Alloc_local -> "L" in
    List.iter (fun (arity,result,m) ->
        printf " %s%s%s"
          (unique_arity_identifier arity)
          (return_arity_identifier result)
          (mode m)) fns in
  let pr_cfuns _ fns =
    List.iter (function
        | (Lambda.Curried {nlocal}, arity, result) ->
            printf " %s%sL%d"
              (unique_arity_identifier arity)
              (return_arity_identifier result)
              nlocal
        | (Lambda.Tupled, arity, result) ->
            printf " -%s%s"
              (unique_arity_identifier arity)
              (return_arity_identifier result)) fns in
  printf "Currying functions:%a\n" pr_cfuns gfns.curry_fun;
  printf "Apply functions:%a\n" pr_afuns gfns.apply_fun;
  printf "Send functions:%a\n" pr_afuns gfns.send_fun

let print_cmx_infos (uir, sections, crc) =
  print_general_infos Compilation_unit.output uir.uir_unit crc uir.uir_defines
    uir.uir_arg_descr (Some uir.uir_format)
    (fun f -> Array.iter f uir.uir_imports_cmi)
    (fun f -> Array.iter f uir.uir_imports_cmx);
  begin
    match uir.uir_export_info with
    | None ->
      printf "Flambda 2 unit (with no export information)\n"
    | Some _ when !no_code && !no_approx ->
      printf "Flambda 2 unit with export information\n"
    | Some cmx ->
      printf "Flambda 2 export information:\n";
      flush stdout;
      let print_typing_env = not !no_approx in
      let print_code = not !no_code in
      let print_offsets = print_code && print_typing_env in
      let cmx = Flambda2_cmx.Flambda_cmx_format.from_raw cmx ~sections in
      Format.printf "%a\n%!" (Flambda2_cmx.Flambda_cmx_format.print ~print_typing_env ~print_code ~print_offsets) cmx
  end;
  print_generic_fns uir.uir_generic_fns;
  printf "Force link: %s\n" (if uir.uir_force_link then "YES" else "no");
  if not (!no_code || !no_approx) then begin
    Zero_alloc_info.Raw.print uir.uir_zero_alloc_info
  end

let print_cmxa_infos (lib : Cmx_format.library_infos) =
  printf "Extra C object files:";
  List.iter print_spaced_string (List.rev lib.lib_ccobjs);
  printf "\nExtra C options:";
  List.iter print_spaced_string (List.rev lib.lib_ccopts);
  printf "\n";
  print_generic_fns lib.lib_generic_fns;
  let module B = Misc.Bitmap in
  lib.lib_units
  |> List.iter (fun u ->
        print_general_infos Compilation_unit.output u.li_name u.li_crc
          u.li_defines None None
          (fun f ->
            B.iter (fun i -> f lib.lib_imports_cmi.(i)) u.li_imports_cmi)
          (fun f ->
            B.iter (fun i -> f lib.lib_imports_cmx.(i)) u.li_imports_cmx);
        printf "Force link: %s\n" (if u.li_force_link then "YES" else "no"))

let print_cmxs_infos header =
  List.iter
    (fun ui ->
       print_general_infos
         Compilation_unit.output ui.dynu_name
         ui.dynu_crc
         ui.dynu_defines
         None
         None
         (fun f -> Array.iter f ui.dynu_imports_cmi)
         (fun f -> Array.iter f ui.dynu_imports_cmx))
    header.dynu_units

let p_title title = printf "%s:\n" title

let p_list title print = function
  | [] -> ()
  | l ->
      p_title title;
      List.iter print l

let dump_byte ic =
  let toc = Bytesections.read_toc ic in
  let all = Bytesections.all toc in
  List.iter
    (fun {Bytesections.name = section; len; _} ->
       try
         if len > 0 then match section with
           | CRCS ->
               let imported_units : Import_info.t list =
                 (Bytesections.read_section_struct toc ic section : Import_info.t array)
                 |> Array.to_list
               in
               p_list "Imported units" print_intf_import imported_units
           | DLLS ->
               let dlls =
                 Bytesections.read_section_string toc ic section
                 |> Misc.split_null_terminated in
               p_list "Used DLLs" print_line dlls
           | DLPT ->
               let dll_paths =
                 Bytesections.read_section_string toc ic section
                 |> Misc.split_null_terminated in
               p_list "Additional DLL paths" print_line dll_paths
           | PRIM ->
               let prims =
                 Bytesections.read_section_string toc ic section
                 |> Misc.split_null_terminated in
               p_list "Primitives used" print_line prims
           | SYMB ->
               let symb = Bytesections.read_section_struct toc ic section in
               print_global_table symb
           | _ -> ()
       with _ -> ()
    )
    all

let find_dyn_offset filename =
  match Binutils.read filename with
  | Ok t ->
      Binutils.symbol_offset t "caml_plugin_header"
  | Error _ ->
      None

let exit_err msg = print_endline msg; exit 2
let exit_errf fmt = Printf.ksprintf exit_err fmt

let exit_magic_msg msg =
  exit_errf
     "Wrong magic number:\n\
      this tool only supports object files produced by compiler version\n\
      \t%s\n\
      %s"
    Sys.ocaml_version msg

let exit_magic_error ~expected_kind err =
  exit_magic_msg Magic_number.(match err with
    | Parse_error err -> explain_parse_error expected_kind err
    | Unexpected_error err -> explain_unexpected_error err)

(* assume that 'ic' is already positioned at the right place
   depending on the format (usually right after the magic number,
   but Exec and Cmxs differ) *)
let dump_obj_by_kind filename ic obj_kind =
  let open Magic_number in
  match obj_kind with
    | Cmo ->
       let cu_pos = input_binary_int ic in
       seek_in ic cu_pos;
       let cu = input_value ic in
       close_in ic;
       print_cmo_infos cu
    | Cma ->
       let toc_pos = input_binary_int ic in
       seek_in ic toc_pos;
       let toc = (input_value ic : library) in
       close_in ic;
       print_cma_infos toc
    | Cmi | Cmt ->
       close_in ic;
       let cmi, cmt = Cmt_format.read filename in
       begin match cmi with
         | None -> ()
         | Some cmi ->
            print_cmi_infos cmi.Cmi_format.cmi_name cmi.Cmi_format.cmi_crcs
              cmi.Cmi_format.cmi_kind cmi.Cmi_format.cmi_params
              cmi.Cmi_format.cmi_globals
       end;
       begin match cmt with
         | None -> ()
         | Some cmt -> print_cmt_infos cmt
       end
    | Cms ->
      close_in ic;
      let cms = Cms_format.read filename in
      print_cms_infos cms
    | Cmx ->
       let uir = (input_value ic : unit_infos_raw) in
       let first_section_offset = pos_in ic in
       seek_in ic (first_section_offset + uir.uir_sections_length);
       let crc = Digest.input ic in
       (* This consumes ic *)
       let sections = Oxcaml_utils.File_sections.create
             uir.uir_section_toc filename ic ~first_section_offset in
       print_cmx_infos (uir, sections, crc)
    | Cmxa ->
       let li = (input_value ic : library_infos) in
       close_in ic;
       print_cmxa_infos li
    | Exec ->
       (* no assumptions on [ic] position,
          [dump_byte] will seek at the right place *)
       dump_byte ic;
       close_in ic
    | Cmxs ->
       (* we assume we are at the offset of the dynamic information,
          as returned by [find_dyn_offset]. *)
       let header = (input_value ic : dynheader) in
       close_in ic;
       print_cmxs_infos header;
    | Ast_impl | Ast_intf ->
       exit_errf "The object file type %S \
                  is currently unsupported by this tool."
         (human_name_of_kind obj_kind)

let dump_obj filename =
  let open Magic_number in
  let dump_standard ic =
    match read_current_info ~expected_kind:None ic with
      | Error ((Unexpected_error _) as err) ->
         exit_magic_error ~expected_kind:None err
      | Ok { kind; version = _ } ->
         dump_obj_by_kind filename ic kind;
         Ok ()
      | Error (Parse_error head_error) ->
         Error head_error
  and dump_exec ic =
    let pos_trailer = in_channel_length ic - Magic_number.magic_length in
    let _ = seek_in ic pos_trailer in
    let expected_kind = Some Exec in
    match read_current_info ~expected_kind ic with
      | Error ((Unexpected_error _) as err) ->
         exit_magic_error ~expected_kind err
      | Ok _ ->
         dump_obj_by_kind filename ic Exec;
         Ok ()
      | Error (Parse_error _)  ->
         Error ()
  and dump_cmxs ic =
    flush stdout;
    match find_dyn_offset filename with
      | None ->
         exit_errf "Unable to read info on %s %s."
           (human_name_of_kind Cmxs) filename
      | Some offset ->
         LargeFile.seek_in ic offset;
         let header = (input_value ic : dynheader) in
         let expected_kind = Some Cmxs in
         match parse header.dynu_magic with
           | Error err ->
              exit_magic_error ~expected_kind (Parse_error err)
           | Ok info ->
         match check_current Cmxs info with
           | Error err ->
              exit_magic_error ~expected_kind (Unexpected_error err)
           | Ok () ->
         LargeFile.seek_in ic offset;
         dump_obj_by_kind filename ic Cmxs;
         ()
  in
  if not !quiet then printf "File %s\n" filename;
  let ic = open_in_bin filename in
  match dump_standard ic with
    | Ok () -> ()
    | Error head_error ->
  match dump_exec ic with
    | Ok () -> ()
    | Error () ->
  if Filename.check_suffix filename ".cmxs"
  then dump_cmxs ic
  else exit_magic_error ~expected_kind:None (Parse_error head_error)

let arg_list = [
  "-quiet", Arg.Set quiet,
    " Only print explicitely required information";
  "-no-approx", Arg.Set no_approx,
    " Do not print module approximation information";
  "-no-code", Arg.Set no_code,
    " Do not print code from exported flambda functions";
  "-shape", Arg.Set shape,
    " Print the shape of the module";
  "-index", Arg.Set index,
    " Print a list of all usages of values, types, etc. in the module";
  "-decls", Arg.Set decls,
    " Print a list of all declarations in the module";
  "-uid-deps", Arg.Set uid_deps,
    " Print the declarations' uids dependencies of the module";
  "-null-crc", Arg.Set no_crc, " Print a null CRC for imported interfaces";
  "-args", Arg.Expand Arg.read_arg,
     "<file> Read additional newline separated command line arguments \n\
     \      from <file>";
  "-args0", Arg.Expand Arg.read_arg0,
     "<file> Read additional NUL separated command line arguments from \n\
     \      <file>";
]
let arg_usage =
   Printf.sprintf "%s [OPTIONS] FILES : give information on files" Sys.argv.(0)

let main () =
  Arg.parse_expand arg_list dump_obj arg_usage;
  exit 0

let _ = main ()
