(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2015 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree
open Ast_iterator

let err = Syntaxerr.ill_formed_ast

let empty_record loc = err loc "Records cannot be empty."
let invalid_tuple loc = err loc "Tuples must have at least 2 components."
let invalid_alias loc = err loc "Alias types must have a name or a jkind."
let empty_open_tuple_pat loc =
  err loc "Open tuple patterns must have at least one component."
let short_closed_tuple_pat loc =
  err loc "Closed tuple patterns must have at least 2 components."
let no_args loc = err loc "Function application with no argument."
let empty_let loc = err loc "Let with no bindings."
let mutable_rec_let loc = err loc "Mutable let binding cannot be recursive."
let multiple_mutable_let loc = err loc "Mutable let must have only one binding."
let mutable_let_bad_pat loc =
  err loc "Mutable let must have a variable on the left hand side."
let empty_type loc = err loc "Type declarations cannot be empty."
let complex_id loc = err loc "Functor application not allowed here."
let module_type_substitution_missing_rhs loc =
  err loc "Module type substitution with no right hand side"
let empty_comprehension loc = err loc "Comprehension with no clauses"
let function_without_value_parameters loc =
  err loc "Function without any value parameters"
let redundant_nested_constraints loc =
  err loc "Nested pattern constraints must all specify a type"
let empty_constraint loc =
  err loc "Constraint without type or mode"

let simple_longident id =
  let rec is_simple = function
    | Longident.Lident _ -> true
    | Longident.Ldot (id, _) -> is_simple id
    | Longident.Lapply _ -> false
  in
  if not (is_simple id.txt) then complex_id id.loc

let check_empty_constraint ~loc ty mode =
  match ty, mode with
  | None, [] -> empty_constraint loc
  | _ -> ()

(* Is this pattern a single variable, possibly with a type annotation? *)
let pat_is_var = function
| Ppat_var _
| Ppat_constraint ({ ppat_desc = Ppat_var _; _}, _, _) -> true
| _ -> false

let iterator =
  let super = Ast_iterator.default_iterator in
  let type_declaration self td =
    super.type_declaration self td;
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_record [] -> empty_record loc
    | _ -> ()
  in
  let typ self ty =
    super.typ self ty;
    let loc = ty.ptyp_loc in
    match ty.ptyp_desc with
    | Ptyp_tuple ([] | [_]) -> invalid_tuple loc
    | Ptyp_package (_, cstrs) ->
      List.iter (fun (id, _) -> simple_longident id) cstrs
    | Ptyp_alias (_, None, None) -> invalid_alias loc
    | _ -> ()
  in
  let pat self pat =
    begin match pat.ppat_desc with
    | Ppat_construct (_, Some (_, ({ppat_desc = Ppat_tuple _} as p)))
      when Builtin_attributes.explicit_arity pat.ppat_attributes ->
        super.pat self p (* allow unary tuple, see GPR#523. *)
    | _ ->
        super.pat self pat
    end;
    let loc = pat.ppat_loc in
    match pat.ppat_desc with
    | Ppat_tuple (lt, op) -> begin
        match lt, op with
        | ([], Open) -> empty_open_tuple_pat loc
        | (([] | [_]), Closed) ->
          short_closed_tuple_pat loc
        | _ -> ()
      end
    | Ppat_record ([], _) -> empty_record loc
    | Ppat_construct (id, _) -> simple_longident id
    | Ppat_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields
    | Ppat_constraint (pat', cty, mode) ->
      check_empty_constraint ~loc cty mode;
      begin match pat'.ppat_desc with
      | Ppat_constraint (_, cty', _) ->
        begin match cty, cty' with
        | None, Some _ | Some _, None ->
          redundant_nested_constraints loc
        | _ -> ()
        end
      | _ -> ()
      end
    | _ -> ()
  in
  let expr self exp =
    begin match exp.pexp_desc with
    | Pexp_construct (_, Some ({pexp_desc = Pexp_tuple _} as e))
      when Builtin_attributes.explicit_arity exp.pexp_attributes ->
        super.expr self e (* allow unary tuple, see GPR#523. *)
    | _ ->
        super.expr self exp
    end;
    let loc = exp.pexp_loc in
    match exp.pexp_desc with
    | Pexp_tuple ([] | [_]) -> invalid_tuple loc
    | Pexp_record ([], _) -> empty_record loc
    | Pexp_apply (_, []) -> no_args loc
    | Pexp_let (_, _, [], _) -> empty_let loc
    | Pexp_let (Mutable, Recursive, _, _) -> mutable_rec_let loc
    | Pexp_let (Mutable, _, l, _) when List.length l > 1 ->
      multiple_mutable_let loc
    | Pexp_let (Mutable, _, [{ pvb_pat = {ppat_desc; _}; _}], _)
      when not (pat_is_var ppat_desc) -> mutable_let_bad_pat loc
    | Pexp_ident id
    | Pexp_construct (id, _)
    | Pexp_field (_, id)
    | Pexp_setfield (_, id, _)
    | Pexp_new id -> simple_longident id
    | Pexp_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields
    | Pexp_function (params, _, Pfunction_body _) ->
        if
          List.for_all
            (function
              | { pparam_desc = Pparam_newtype _ } -> true
              | { pparam_desc = Pparam_val _ } -> false)
            params
        then function_without_value_parameters loc
    | Pexp_comprehension
        ( Pcomp_list_comprehension {pcomp_clauses = []}
        | Pcomp_array_comprehension (_, {pcomp_clauses = []}) )
      ->
        empty_comprehension loc
    | Pexp_constraint (_, ty, mode) -> check_empty_constraint ~loc ty mode
    | _ -> ()
  in
  let extension_constructor self ec =
    super.extension_constructor self ec;
    match ec.pext_kind with
    | Pext_rebind id -> simple_longident id
    | _ -> ()
  in
  let class_expr self ce =
    super.class_expr self ce;
    let loc = ce.pcl_loc in
    match ce.pcl_desc with
    | Pcl_apply (_, []) -> no_args loc
    | Pcl_constr (id, _) -> simple_longident id
    | _ -> ()
  in
  let module_type self mty =
    super.module_type self mty;
    match mty.pmty_desc with
    | Pmty_alias id -> simple_longident id
    | _ -> ()
  in
  let open_description self opn =
    super.open_description self opn
  in
  let with_constraint self wc =
    super.with_constraint self wc;
    match wc with
    | Pwith_type (id, _)
    | Pwith_module (id, _) -> simple_longident id
    | _ -> ()
  in
  let module_expr self me =
    super.module_expr self me;
    match me.pmod_desc with
    | Pmod_ident id -> simple_longident id
    | Pmod_constraint (_, ty, mode) -> check_empty_constraint ~loc:me.pmod_loc ty mode
    | _ -> ()
  in
  let structure_item self st =
    super.structure_item self st;
    let loc = st.pstr_loc in
    match st.pstr_desc with
    | Pstr_type (_, []) -> empty_type loc
    | Pstr_value (_, []) -> empty_let loc
    | _ -> ()
  in
  let signature_item self sg =
    super.signature_item self sg;
    let loc = sg.psig_loc in
    match sg.psig_desc with
    | Psig_type (_, []) -> empty_type loc
    | Psig_modtypesubst {pmtd_type=None; _ } ->
        module_type_substitution_missing_rhs loc
    | _ -> ()
  in
  let row_field self field =
    super.row_field self field;
    let loc = field.prf_loc in
    match field.prf_desc with
    | Rtag _ -> ()
    | Rinherit _ ->
      if field.prf_attributes = []
      then ()
      else err loc
          "In variant types, attaching attributes to inherited \
           subtypes is not allowed."
  in
  let object_field self field =
    super.object_field self field;
    let loc = field.pof_loc in
    match field.pof_desc with
    | Otag _ -> ()
    | Oinherit _ ->
      if field.pof_attributes = []
      then ()
      else err loc
          "In object types, attaching attributes to inherited \
           subtypes is not allowed."
  in
  let attribute self attr =
    (* The change to `self` here avoids registering attributes within attributes
       for the purposes of warning 53, while keeping all the other invariant
       checks for attribute payloads.  See comment on [current_phase] in
       [builtin_attributes.mli]. *)
    super.attribute { self with attribute = super.attribute } attr;
    Builtin_attributes.(register_attr Invariant_check attr.attr_name)
  in
  { super with
    type_declaration
  ; typ
  ; pat
  ; expr
  ; extension_constructor
  ; class_expr
  ; module_expr
  ; module_type
  ; open_description
  ; with_constraint
  ; structure_item
  ; signature_item
  ; row_field
  ; object_field
  ; attribute
  }

let structure st = iterator.structure iterator st
let signature sg = iterator.signature iterator sg
