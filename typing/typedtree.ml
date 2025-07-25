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

(* Abstract syntax tree after typing *)

open Asttypes
open Types
open Mode

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * Location.t * string option
  | Const_float of string
  | Const_float32 of string
  | Const_unboxed_float of string
  | Const_unboxed_float32 of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_unboxed_int32 of int32
  | Const_unboxed_int64 of int64
  | Const_unboxed_nativeint of nativeint

module Uid = Shape.Uid

(* Value expressions for the core language *)

type partial = Partial | Total

type attribute = Parsetree.attribute
type attributes = attribute list

type value = Value_pattern
type computation = Computation_pattern

type _ pattern_category =
| Value : value pattern_category
| Computation : computation pattern_category

module Unique_barrier = struct
  (* For added safety, we record the states in the life of a barrier:
     - Barriers start out as Not_computed
     - They are enabled by the uniqueness analysis
     - They are resolved in translcore
    This allows us to error if the barrier is not enabled or resolved. *)
  type barrier =
    | Enabled of Mode.Uniqueness.lr
    | Resolved of Mode.Uniqueness.Const.t
    | Not_computed

  type t = barrier ref

  let not_computed () = ref Not_computed

  let enable barrier = match !barrier with
    | Not_computed ->
      barrier := Enabled (Uniqueness.newvar ())
    | _ -> Misc.fatal_error "Unique barrier was enabled twice"

  (* Due to or-patterns a barrier may have several upper bounds. *)
  let add_upper_bound uniq barrier =
    match !barrier with
    | Enabled barrier -> Uniqueness.submode_exn barrier uniq
    | _ -> Misc.fatal_error "Unique barrier got an upper bound in the wrong state"

  let resolve barrier =
    match !barrier with
    | Enabled uniq ->
      let zapped = Uniqueness.zap_to_ceil uniq in
      barrier := Resolved zapped;
      zapped
    | Resolved barrier -> barrier
    | Not_computed ->
      (* CR uniqueness: The uniqueness analysis does not go into legacy
         language constructs such as objects; for those, we default to legacy.
         We should change the uniqueness analysis to also traverse these
         constructs. Then this case will be impossible to reach and we can fail
         here. Failing here will protect us when future language extensions are
         not traversing the uniqueness analysis. This ensures that the
         unique barriers will stay sound for future extensions. *)
      Uniqueness.Const.legacy

  let print ppf t =
    let open Format in
    let print = function
      | Enabled u -> fprintf ppf "Enabled(%a)" (Mode.Uniqueness.print ()) u
      | Resolved uc ->
        fprintf ppf "Resolved(%a)" Mode.Uniqueness.Const.print uc
      | Not_computed -> fprintf ppf "Not_computed"
    in
    print !t
end

type unique_use = Mode.Uniqueness.r * Mode.Linearity.l

let print_unique_use ppf (u,l) =
  let open Format in
  fprintf ppf "@[(%a,@ %a)@]"
    (Mode.Uniqueness.print ()) u
    (Mode.Linearity.print ()) l

type alloc_mode = {
  mode : Mode.Alloc.r;
  locality_context : Env.locality_context option;
}

type texp_field_boxing =
  | Boxing of alloc_mode * unique_use
  | Non_boxing of unique_use

let aliased_many_use =
  ( Mode.Uniqueness.disallow_left Mode.Uniqueness.aliased,
    Mode.Linearity.disallow_right Mode.Linearity.many )

type pattern = value general_pattern
and 'k general_pattern = 'k pattern_desc pattern_data

and 'a pattern_data =
  { pat_desc: 'a;
    pat_loc: Location.t;
    pat_extra : (pat_extra * Location.t * attribute list) list;
    pat_type: type_expr;
    pat_env: Env.t;
    pat_attributes: attribute list;
    pat_unique_barrier : Unique_barrier.t;
   }

and pat_extra =
  | Tpat_constraint of core_type
  | Tpat_type of Path.t * Longident.t loc
  | Tpat_open of Path.t * Longident.t loc * Env.t
  | Tpat_unpack

and 'k pattern_desc =
  (* value patterns *)
  | Tpat_any : value pattern_desc
  | Tpat_var : Ident.t * string loc * Uid.t * Mode.Value.l -> value pattern_desc
  | Tpat_alias :
      value general_pattern * Ident.t * string loc * Uid.t * Mode.Value.l
      * Types.type_expr -> value pattern_desc
  | Tpat_constant : constant -> value pattern_desc
  | Tpat_tuple : (string option * value general_pattern) list -> value pattern_desc
  | Tpat_unboxed_tuple :
      (string option * value general_pattern * Jkind.sort) list ->
      value pattern_desc
  | Tpat_construct :
      Longident.t loc * Types.constructor_description *
        value general_pattern list *
        ((Ident.t loc * Parsetree.jkind_annotation option) list * core_type)
          option ->
      value pattern_desc
  | Tpat_variant :
      label * value general_pattern option * row_desc ref ->
      value pattern_desc
  | Tpat_record :
      (Longident.t loc * label_description * value general_pattern) list *
        closed_flag ->
      value pattern_desc
  | Tpat_record_unboxed_product :
      (Longident.t loc * unboxed_label_description * value general_pattern) list
      * closed_flag ->
      value pattern_desc
  | Tpat_array :
      mutability * Jkind.sort * value general_pattern list -> value pattern_desc
  | Tpat_lazy : value general_pattern -> value pattern_desc
  (* computation patterns *)
  | Tpat_value : tpat_value_argument -> computation pattern_desc
  | Tpat_exception : value general_pattern -> computation pattern_desc
  (* generic constructions *)
  | Tpat_or :
      'k general_pattern * 'k general_pattern * row_desc option ->
      'k pattern_desc

and tpat_value_argument = value general_pattern

and expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_extra: (exp_extra * Location.t * attribute list) list;
    exp_type: type_expr;
    exp_env: Env.t;
    exp_attributes: attribute list;
   }

and exp_extra =
  | Texp_constraint of core_type
  | Texp_coerce of core_type option * core_type
  | Texp_poly of core_type option
  | Texp_newtype of Ident.t * string loc *
                    Parsetree.jkind_annotation option * Uid.t
  | Texp_stack
  | Texp_mode of Mode.Alloc.Const.Option.t

and arg_label = Types.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
  | Position of string

and expression_desc =
    Texp_ident of
      Path.t * Longident.t loc * Types.value_description * ident_kind * unique_use
  | Texp_constant of constant
  | Texp_let of rec_flag * value_binding list * expression
  | Texp_letmutable of value_binding * expression
  | Texp_function of
      { params : function_param list;
        body : function_body;
        ret_mode : Mode.Alloc.l;
        ret_sort : Jkind.sort;
        alloc_mode : alloc_mode;
        zero_alloc : Zero_alloc.t;
      }
  | Texp_apply of
      expression * (arg_label * apply_arg) list * apply_position *
        Mode.Locality.l * Zero_alloc.assume option
  | Texp_match of expression * Jkind.sort * computation case list * partial
  | Texp_try of expression * value case list
  | Texp_tuple of (string option * expression) list * alloc_mode
  | Texp_unboxed_tuple of (string option * expression * Jkind.sort) list
  | Texp_construct of
      Longident.t loc * constructor_description * expression list * alloc_mode option
  | Texp_variant of label * (expression * alloc_mode) option
  | Texp_record of {
      fields : ( Types.label_description * record_label_definition ) array;
      representation : Types.record_representation;
      extended_expression : (expression * Jkind.sort * Unique_barrier.t) option;
      alloc_mode : alloc_mode option
    }
  | Texp_record_unboxed_product of {
      fields :
        ( Types.unboxed_label_description * record_label_definition ) array;
      representation : Types.record_unboxed_product_representation;
      extended_expression : (expression * Jkind.sort) option;
    }
  | Texp_field of
      expression * Jkind.sort * Longident.t loc * label_description *
        texp_field_boxing * Unique_barrier.t
  | Texp_unboxed_field of
      expression * Jkind.sort * Longident.t loc * unboxed_label_description *
        unique_use
  | Texp_setfield of
      expression * Mode.Locality.l * Longident.t loc * label_description * expression
  | Texp_array of mutability * Jkind.Sort.t * expression list * alloc_mode
  | Texp_list_comprehension of comprehension
  | Texp_array_comprehension of mutability * Jkind.sort * comprehension
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * Jkind.sort * expression
  | Texp_while of {
      wh_cond : expression;
      wh_body : expression;
      wh_body_sort : Jkind.sort
    }
  | Texp_for of {
      for_id  : Ident.t;
      for_debug_uid: Shape.Uid.t;
      for_pat : Parsetree.pattern;
      for_from : expression;
      for_to   : expression;
      for_dir  : direction_flag;
      for_body : expression;
      for_body_sort : Jkind.sort;
    }
  | Texp_send of expression * meth * apply_position
  | Texp_new of
      Path.t * Longident.t loc * Types.class_declaration * apply_position
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_mutvar of Ident.t loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_setmutvar of Ident.t loc * Jkind.sort * expression
  | Texp_override of Path.t * (Ident.t * string loc * expression) list
  | Texp_letmodule of
      Ident.t option * string option loc * Types.module_presence * module_expr *
        expression
  | Texp_letexception of extension_constructor * expression
  | Texp_assert of expression * Location.t
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_letop of {
      let_ : binding_op;
      ands : binding_op list;
      param : Ident.t;
      param_debug_uid : Shape.Uid.t;
      param_sort : Jkind.sort;
      body : value case;
      body_sort : Jkind.sort;
      partial : partial;
    }
  | Texp_unreachable
  | Texp_extension_constructor of Longident.t loc * Path.t
  | Texp_open of open_declaration * expression
  | Texp_probe of { name:string; handler:expression; enabled_at_init:bool; }
  | Texp_probe_is_enabled of { name:string }
  | Texp_exclave of expression
  | Texp_src_pos
  | Texp_overwrite of expression * expression
  | Texp_hole of unique_use

and ident_kind =
  | Id_value
  | Id_prim of Mode.Locality.l option * Jkind.Sort.t option

and meth =
  | Tmeth_name of string
  | Tmeth_val of Ident.t
  | Tmeth_ancestor of Ident.t * Path.t

and comprehension =
  {
    comp_body : expression;
    comp_clauses : comprehension_clause list
  }

and comprehension_clause =
  | Texp_comp_for of comprehension_clause_binding list
  | Texp_comp_when of expression

and comprehension_clause_binding =
  {
    comp_cb_iterator : comprehension_iterator;
    comp_cb_attributes : attribute list
  }

and comprehension_iterator =
  | Texp_comp_range of
      { ident     : Ident.t
      ; ident_debug_uid : Shape.Uid.t
      ; pattern   : Parsetree.pattern
      ; start     : expression
      ; stop      : expression
      ; direction : direction_flag }
  | Texp_comp_in of
      { pattern  : pattern
      ; sequence : expression }

and 'k case =
    {
     c_lhs: 'k general_pattern;
     c_guard: expression option;
     c_rhs: expression;
    }

and function_curry =
  | More_args of { partial_mode : Mode.Alloc.l }
  | Final_arg

and function_param =
  {
    fp_arg_label: arg_label;
    fp_param: Ident.t;
    fp_param_debug_uid : Shape.Uid.t;
    fp_partial: partial;
    fp_kind: function_param_kind;
    fp_sort: Jkind.sort;
    fp_mode: Mode.Alloc.l;
    fp_curry: function_curry;
    fp_newtypes: (Ident.t * string loc *
                  Parsetree.jkind_annotation option * Uid.t) list;
    fp_loc: Location.t;
  }

and function_param_kind =
  | Tparam_pat of pattern
  | Tparam_optional_default of pattern * expression * Jkind.sort

and function_body =
  | Tfunction_body of expression
  | Tfunction_cases of function_cases

and function_cases =
  { fc_cases: value case list;
    fc_env : Env.t;
    fc_arg_mode: Mode.Alloc.l;
    fc_arg_sort: Jkind.sort;
    fc_ret_type : Types.type_expr;
    fc_partial: partial;
    fc_param: Ident.t;
    fc_param_debug_uid: Shape.Uid.t;
    fc_loc: Location.t;
    fc_exp_extra: exp_extra option;
    fc_attributes: attributes;
  }

and record_label_definition =
  | Kept of Types.type_expr * mutability * unique_use
  | Overridden of Longident.t loc * expression

and binding_op =
  {
    bop_op_path : Path.t;
    bop_op_name : string loc;
    bop_op_val : Types.value_description;
    bop_op_type : Types.type_expr;
    bop_op_return_sort : Jkind.sort;
    bop_exp : expression;
    bop_exp_sort : Jkind.sort;
    bop_loc : Location.t;
  }

and ('a, 'b) arg_or_omitted =
  | Arg of 'a
  | Omitted of 'b

and omitted_parameter =
  { mode_closure : Mode.Alloc.r;
    mode_arg : Mode.Alloc.l;
    mode_ret : Mode.Alloc.l;
    sort_arg : Jkind.sort;
    sort_ret : Jkind.sort }

and apply_arg = (expression * Jkind.sort, omitted_parameter) arg_or_omitted

and apply_position =
  | Tail
  | Nontail
  | Default

(* Value expressions for the class language *)

and class_expr =
    {
     cl_desc: class_expr_desc;
     cl_loc: Location.t;
     cl_type: Types.class_type;
     cl_env: Env.t;
     cl_attributes: attribute list;
    }

and class_expr_desc =
    Tcl_ident of Path.t * Longident.t loc * core_type list
  | Tcl_structure of class_structure
  | Tcl_fun of
      arg_label * pattern * (Ident.t * expression) list
      * class_expr * partial
  | Tcl_apply of class_expr * (arg_label * apply_arg) list
  | Tcl_let of rec_flag * value_binding list *
                  (Ident.t * expression) list * class_expr
  | Tcl_constraint of
      class_expr * class_type option * string list * string list * MethSet.t
    (* Visible instance variables, methods and concrete methods *)
  | Tcl_open of open_description * class_expr

and class_structure =
  {
   cstr_self: pattern;
   cstr_fields: class_field list;
   cstr_type: Types.class_signature;
   cstr_meths: Ident.t Meths.t;
  }

and class_field =
   {
    cf_desc: class_field_desc;
    cf_loc: Location.t;
    cf_attributes: attribute list;
  }

and class_field_kind =
  | Tcfk_virtual of core_type
  | Tcfk_concrete of override_flag * expression

and class_field_desc =
    Tcf_inherit of
      override_flag * class_expr * string option * (string * Ident.t) list *
        (string * Ident.t) list
    (* Inherited instance variables and concrete methods *)
  | Tcf_val of string loc * mutable_flag * Ident.t * class_field_kind * bool
  | Tcf_method of string loc * private_flag * class_field_kind
  | Tcf_constraint of core_type * core_type
  | Tcf_initializer of expression
  | Tcf_attribute of attribute

and held_locks = Env.locks * Longident.t * Location.t

and mode_with_locks = Mode.Value.l * held_locks option

(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_mode : mode_with_locks;
    mod_env: Env.t;
    mod_attributes: attribute list;
   }

and module_type_constraint =
  Tmodtype_implicit
| Tmodtype_explicit of module_type

and functor_parameter =
  | Unit
  | Named of Ident.t option * string option loc * module_type

and module_expr_desc =
    Tmod_ident of Path.t * Longident.t loc
  | Tmod_structure of structure
  | Tmod_functor of functor_parameter * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_apply_unit of module_expr
  | Tmod_constraint of
      module_expr * Types.module_type * module_type_constraint * module_coercion
  | Tmod_unpack of expression * Types.module_type

and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}

and structure_item =
  { str_desc : structure_item_desc;
    str_loc : Location.t;
    str_env : Env.t
  }

and structure_item_desc =
    Tstr_eval of expression * Jkind.sort * attributes
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of type_exception
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_declaration
  | Tstr_class of (class_declaration * string list) list
  | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list
  | Tstr_include of include_declaration
  | Tstr_attribute of attribute

and module_binding =
    {
     mb_id: Ident.t option;
     mb_name: string option loc;
     mb_uid: Uid.t;
     mb_presence: module_presence;
     mb_expr: module_expr;
     mb_attributes: attribute list;
     mb_loc: Location.t;
    }

and value_binding =
  {
    vb_pat: pattern;
    vb_expr: expression;
    vb_rec_kind: Value_rec_types.recursive_binding_kind;
    vb_sort: Jkind.sort;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list *
                         (Ident.t * int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of primitive_coercion
  | Tcoerce_alias of Env.t * Path.t * module_coercion

and module_type =
  { mty_desc: module_type_desc;
    mty_type : Types.module_type;
    mty_env : Env.t;
    mty_loc: Location.t;
    mty_attributes: attribute list;
   }

and module_type_desc =
    Tmty_ident of Path.t * Longident.t loc
  | Tmty_signature of signature
  | Tmty_functor of functor_parameter * module_type
  | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t * Longident.t loc
  | Tmty_strengthen of module_type * Path.t * Longident.t loc

(* Keep primitive type information for type-based lambda-code specialization *)
and primitive_coercion =
  {
    pc_desc: Primitive.description;
    pc_type: type_expr;
    pc_poly_mode: Mode.Locality.l option;
    pc_poly_sort: Jkind.Sort.t option;
    pc_env: Env.t;
    pc_loc : Location.t;
  }

and signature = {
  sig_items : signature_item list;
  sig_modalities : Mode.Modality.Value.Const.t;
  sig_type : Types.signature;
  sig_final_env : Env.t;
  sig_sloc : Location.t;
}

and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typesubst of type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of type_exception
  | Tsig_module of module_declaration
  | Tsig_modsubst of module_substitution
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of module_type_declaration
  | Tsig_modtypesubst of module_type_declaration
  | Tsig_open of open_description
  | Tsig_include of include_description * Mode.Modality.Value.Const.t
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list
  | Tsig_attribute of attribute

and module_declaration =
    {
     md_id: Ident.t option;
     md_name: string option loc;
     md_uid: Uid.t;
     md_presence: module_presence;
     md_type: module_type;
     md_modalities: Mode.Modality.Value.t;
     md_attributes: attribute list;
     md_loc: Location.t;
    }

and module_substitution =
    {
     ms_id: Ident.t;
     ms_name: string loc;
     ms_uid: Uid.t;
     ms_manifest: Path.t;
     ms_txt: Longident.t loc;
     ms_attributes: attributes;
     ms_loc: Location.t;
    }

and module_type_declaration =
    {
     mtd_id: Ident.t;
     mtd_name: string loc;
     mtd_uid: Uid.t;
     mtd_type: module_type option;
     mtd_attributes: attribute list;
     mtd_loc: Location.t;
    }

and 'a open_infos =
    {
     open_expr: 'a;
     open_bound_items: Types.signature;
     open_override: override_flag;
     open_env: Env.t;
     open_loc: Location.t;
     open_attributes: attribute list;
    }

and open_description = (Path.t * Longident.t loc) open_infos

and open_declaration = module_expr open_infos

and include_kind =
  | Tincl_structure
  | Tincl_functor of (Ident.t * module_coercion) list
  | Tincl_gen_functor of (Ident.t * module_coercion) list

and 'a include_infos =
    {
     incl_mod: 'a;
     incl_type: Types.signature;
     incl_loc: Location.t;
     incl_kind: include_kind;
     incl_attributes: attribute list;
    }

and include_description = module_type include_infos

and include_declaration = module_expr include_infos

and with_constraint =
    Twith_type of type_declaration
  | Twith_module of Path.t * Longident.t loc
  | Twith_modtype of module_type
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t * Longident.t loc
  | Twith_modtypesubst of module_type


and core_type =
(* mutable because of [Typeclass.declare_method] *)
  { mutable ctyp_desc : core_type_desc;
    mutable ctyp_type : type_expr;
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t;
    ctyp_attributes: attribute list;
   }

and core_type_desc =
  | Ttyp_var of string option * Parsetree.jkind_annotation option
  | Ttyp_arrow of arg_label * core_type * core_type
  | Ttyp_tuple of (string option * core_type) list
  | Ttyp_unboxed_tuple of (string option * core_type) list
  | Ttyp_constr of Path.t * Longident.t loc * core_type list
  | Ttyp_object of object_field list * closed_flag
  | Ttyp_class of Path.t * Longident.t loc * core_type list
  | Ttyp_alias of core_type * string loc option *
                  Parsetree.jkind_annotation option
  | Ttyp_variant of row_field list * closed_flag * label list option
  | Ttyp_poly of (string * Parsetree.jkind_annotation option) list * core_type
  | Ttyp_package of package_type
  | Ttyp_open of Path.t * Longident.t loc * core_type
  | Ttyp_of_kind of Parsetree.jkind_annotation
  | Ttyp_call_pos

and package_type = {
  pack_path : Path.t;
  pack_fields : (Longident.t loc * core_type) list;
  pack_type : Types.module_type;
  pack_txt : Longident.t loc;
}

and row_field = {
  rf_desc : row_field_desc;
  rf_loc : Location.t;
  rf_attributes : attributes;
}

and row_field_desc =
    Ttag of string loc * bool * core_type list
  | Tinherit of core_type

and object_field = {
  of_desc : object_field_desc;
  of_loc : Location.t;
  of_attributes : attributes;
}

and object_field_desc =
  | OTtag of string loc * core_type
  | OTinherit of core_type

and value_description =
  { val_id: Ident.t;
    val_name: string loc;
    val_desc: core_type;
    val_val: Types.value_description;
    val_prim: string list;
    val_loc: Location.t;
    val_attributes: attribute list;
    }

and type_declaration =
  { typ_id: Ident.t;
    typ_name: string loc;
    typ_params: (core_type * (variance * injectivity)) list;
    typ_type: Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_loc: Location.t;
    typ_attributes: attribute list;
    typ_jkind_annotation: Parsetree.jkind_annotation option;
   }

and type_kind =
    Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_record_unboxed_product of label_declaration list
  | Ttype_open

and label_declaration =
    {
     ld_id: Ident.t;
     ld_name: string loc;
     ld_uid: Uid.t;
     ld_mutable: mutability;
     ld_modalities: Modality.Value.Const.t;
     ld_type: core_type;
     ld_loc: Location.t;
     ld_attributes: attribute list;
    }

and constructor_declaration =
    {
     cd_id: Ident.t;
     cd_name: string loc;
     cd_uid: Uid.t;
     cd_vars: (string * Parsetree.jkind_annotation option) list;
     cd_args: constructor_arguments;
     cd_res: core_type option;
     cd_loc: Location.t;
     cd_attributes: attribute list;
    }

and constructor_argument =
  {
    ca_modalities: Modality.Value.Const.t;
    ca_type: core_type;
    ca_loc: Location.t;
  }

and constructor_arguments =
  | Cstr_tuple of constructor_argument list
  | Cstr_record of label_declaration list

and type_extension =
  {
    tyext_path: Path.t;
    tyext_txt: Longident.t loc;
    tyext_params: (core_type * (variance * injectivity)) list;
    tyext_constructors: extension_constructor list;
    tyext_private: private_flag;
    tyext_loc: Location.t;
    tyext_attributes: attribute list;
  }

and type_exception =
  {
    tyexn_constructor: extension_constructor;
    tyexn_loc: Location.t;
    tyexn_attributes: attribute list;
  }

and extension_constructor =
  {
    ext_id: Ident.t;
    ext_name: string loc;
    ext_type: Types.extension_constructor;
    ext_kind: extension_constructor_kind;
    ext_loc: Location.t;
    ext_attributes: attribute list;
  }

and extension_constructor_kind =
    Text_decl of (string * Parsetree.jkind_annotation option) list *
                 constructor_arguments *
                 core_type option
  | Text_rebind of Path.t * Longident.t loc

and class_type =
    {
     cltyp_desc: class_type_desc;
     cltyp_type: Types.class_type;
     cltyp_env: Env.t;
     cltyp_loc: Location.t;
     cltyp_attributes: attribute list;
    }

and class_type_desc =
    Tcty_constr of Path.t * Longident.t loc * core_type list
  | Tcty_signature of class_signature
  | Tcty_arrow of arg_label * core_type * class_type
  | Tcty_open of open_description * class_type

and class_signature = {
    csig_self: core_type;
    csig_fields: class_type_field list;
    csig_type: Types.class_signature;
  }

and class_type_field = {
    ctf_desc: class_type_field_desc;
    ctf_loc: Location.t;
    ctf_attributes: attribute list;
  }

and class_type_field_desc =
  | Tctf_inherit of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_method of (string * private_flag * virtual_flag * core_type)
  | Tctf_constraint of (core_type * core_type)
  | Tctf_attribute of attribute

and class_declaration =
  class_expr class_infos

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos

and 'a class_infos =
  { ci_virt: virtual_flag;
    ci_params: (core_type * (variance * injectivity)) list;
    ci_id_name: string loc;
    ci_id_class: Ident.t;
    ci_id_class_type: Ident.t;
    ci_id_object: Ident.t;
    ci_expr: 'a;
    ci_decl: Types.class_declaration;
    ci_type_decl: Types.class_type_declaration;
    ci_loc: Location.t;
    ci_attributes: attribute list;
   }

type argument_interface = {
  ai_signature: Types.signature;
  ai_coercion_from_primary: module_coercion;
}

type implementation = {
  structure: structure;
  coercion: module_coercion;
  signature: Types.signature;
  argument_interface: argument_interface option;
  shape: Shape.t;
}

type item_declaration =
  | Value of value_description
  | Value_binding of value_binding
  | Type of type_declaration
  | Constructor of constructor_declaration
  | Extension_constructor of extension_constructor
  | Label of label_declaration
  | Module of module_declaration
  | Module_substitution of module_substitution
  | Module_binding of module_binding
  | Module_type of module_type_declaration
  | Class of class_declaration
  | Class_type of class_type_declaration

(* Auxiliary functions over the a.s.t. *)

let as_computation_pattern (p : pattern) : computation general_pattern =
  {
    pat_desc = Tpat_value p;
    pat_loc = p.pat_loc;
    pat_extra = [];
    pat_type = p.pat_type;
    pat_env = p.pat_env;
    pat_attributes = [];
    pat_unique_barrier = p.pat_unique_barrier;
  }

let function_arity params body =
  List.length params +
  match body with
  | Tfunction_body _ -> 0
  | Tfunction_cases _ -> 1

let rec classify_pattern_desc : type k . k pattern_desc -> k pattern_category =
  function
  | Tpat_alias _ -> Value
  | Tpat_tuple _ -> Value
  | Tpat_unboxed_tuple _ -> Value
  | Tpat_construct _ -> Value
  | Tpat_variant _ -> Value
  | Tpat_record _ -> Value
  | Tpat_record_unboxed_product _ -> Value
  | Tpat_array _ -> Value
  | Tpat_lazy _ -> Value
  | Tpat_any -> Value
  | Tpat_var _ -> Value
  | Tpat_constant _ -> Value

  | Tpat_value _ -> Computation
  | Tpat_exception _ -> Computation

  | Tpat_or(p1, p2, _) ->
     begin match classify_pattern p1, classify_pattern p2 with
     | Value, Value -> Value
     | Computation, Computation -> Computation
     end

and classify_pattern
  : type k . k general_pattern -> k pattern_category
  = fun pat ->
  classify_pattern_desc pat.pat_desc

type pattern_action =
  { f : 'k . 'k general_pattern -> unit }
let shallow_iter_pattern_desc
  : type k . pattern_action -> k pattern_desc -> unit
  = fun f -> function
  | Tpat_alias(p, _, _, _, _, _) -> f.f p
  | Tpat_tuple patl -> List.iter (fun (_, p) -> f.f p) patl
  | Tpat_unboxed_tuple patl -> List.iter (fun (_, p, _) -> f.f p) patl
  | Tpat_construct(_, _, patl, _) -> List.iter f.f patl
  | Tpat_variant(_, pat, _) -> Option.iter f.f pat
  | Tpat_record (lbl_pat_list, _) ->
      List.iter (fun (_, _, pat) -> f.f pat) lbl_pat_list
  | Tpat_record_unboxed_product (lbl_pat_list, _) ->
      List.iter (fun (_, _, pat) -> f.f pat) lbl_pat_list
  | Tpat_array (_, _, patl) -> List.iter f.f patl
  | Tpat_lazy p -> f.f p
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _ -> ()
  | Tpat_value p -> f.f p
  | Tpat_exception p -> f.f p
  | Tpat_or(p1, p2, _) -> f.f p1; f.f p2

type pattern_transformation =
  { f : 'k . 'k general_pattern -> 'k general_pattern }
let shallow_map_pattern_desc
  : type k . pattern_transformation -> k pattern_desc -> k pattern_desc
  = fun f d -> match d with
  | Tpat_alias (p1, id, s, uid, m, ty) ->
      Tpat_alias (f.f p1, id, s, uid, m, ty)
  | Tpat_tuple pats ->
      Tpat_tuple (List.map (fun (label, pat) -> label, f.f pat) pats)
  | Tpat_unboxed_tuple pats ->
      Tpat_unboxed_tuple
        (List.map (fun (label, pat, sort) -> label, f.f pat, sort) pats)
  | Tpat_record (lpats, closed) ->
      Tpat_record (List.map (fun (lid, l,p) -> lid, l, f.f p) lpats, closed)
  | Tpat_record_unboxed_product (lpats, closed) ->
      Tpat_record_unboxed_product
        (List.map (fun (lid, l,p) -> lid, l, f.f p) lpats, closed)
  | Tpat_construct (lid, c, pats, ty) ->
      Tpat_construct (lid, c, List.map f.f pats, ty)
  | Tpat_array (am, arg_sort, pats) ->
      Tpat_array (am, arg_sort, List.map f.f pats)
  | Tpat_lazy p1 -> Tpat_lazy (f.f p1)
  | Tpat_variant (x1, Some p1, x2) ->
      Tpat_variant (x1, Some (f.f p1), x2)
  | Tpat_var _
  | Tpat_constant _
  | Tpat_any
  | Tpat_variant (_,None,_) -> d
  | Tpat_value p -> Tpat_value (f.f p)
  | Tpat_exception p -> Tpat_exception (f.f p)
  | Tpat_or (p1,p2,path) ->
      Tpat_or (f.f p1, f.f p2, path)

let rec iter_general_pattern
  : type k . pattern_action -> k general_pattern -> unit
  = fun f p ->
  f.f p;
  shallow_iter_pattern_desc
    { f = fun p -> iter_general_pattern f p }
    p.pat_desc

let iter_pattern (f : pattern -> unit) =
  iter_general_pattern
    { f = fun (type k) (p : k general_pattern) ->
          match classify_pattern p with
          | Value -> f p
          | Computation -> () }

type pattern_predicate = { f : 'k . 'k general_pattern -> bool }
let exists_general_pattern (f : pattern_predicate) p =
  let exception Found in
  match
    iter_general_pattern
      { f = fun p -> if f.f p then raise Found else () }
      p
  with
  | exception Found -> true
  | () -> false

let exists_pattern (f : pattern -> bool) =
  exists_general_pattern
    { f = fun (type k) (p : k general_pattern) ->
          match classify_pattern p with
          | Value -> f p
          | Computation -> false }


(* List the identifiers bound by a pattern or a let *)

let rec iter_bound_idents
  : type k . _ -> k general_pattern -> _
  = fun f pat ->
  match pat.pat_desc with
  | Tpat_var (id, s, uid, _mode) ->
     f (id,s,pat.pat_type, uid)
  | Tpat_alias(p, id, s, uid, _mode, ty) ->
      iter_bound_idents f p;
      f (id, s, ty, uid)
  | Tpat_or(p1, _, _) ->
      (* Invariant : both arguments bind the same variables *)
      iter_bound_idents f p1
  | d ->
     shallow_iter_pattern_desc
       { f = fun p -> iter_bound_idents f p }
       d

type 'sort full_bound_ident_action =
  Ident.t -> string loc -> type_expr -> Uid.t -> Mode.Value.l -> 'sort -> unit

let for_transl f =
  f ~of_sort:Jkind.Sort.default_for_transl_and_get ~of_const_sort:Fun.id

let for_typing f =
  f ~of_sort:Fun.id ~of_const_sort:Jkind.Sort.of_const

(* The intent is that the sort should be the sort of the type of the pattern.
   It's used to avoid computing jkinds from types.  `f` then gets passed
   the sorts of the variables.

   This is occasionally used in places where we don't actually know
   about the sort of the pattern but `f` doesn't care about the sorts.

   Because this should work both over [Jkind.Sort.t] and [Jkind.Sort.Const.t],
   this takes conversion functions [of_sort : Jkind.Sort.t -> 'sort] and
   [of_const_sort : Jkind.Sort.Const.t -> 'sort]. The need for these is somewhat
   unfortunate, but it's worth it to allow [Jkind.Sort.Const.t] to be used
   throughout the transl process. *)
let iter_pattern_full ~of_sort ~of_const_sort ~both_sides_of_or f sort pat =
  let value = of_const_sort Jkind.Sort.Const.value in
  let rec loop :
    type k . 'sort full_bound_ident_action -> 'sort -> k general_pattern -> _ =
    fun f sort pat ->
      match pat.pat_desc with
      (* Cases where we push the sort inwards: *)
      | Tpat_var (id, s, uid, mode) ->
          f id s pat.pat_type uid mode sort
      | Tpat_alias(p, id, s, uid, mode, ty) ->
          loop f sort p;
          f id s ty uid mode sort
      | Tpat_or (p1, p2, _) ->
        if both_sides_of_or then (loop f sort p1; loop f sort p2)
        else loop f sort p1
      | Tpat_value p -> loop f sort p
      (* Cases where we compute the sort of the inner thing from the pattern *)
      | Tpat_construct(_, cstr, patl, _) ->
          let sorts =
            match cstr.cstr_repr with
            | Variant_unboxed -> [ sort ]
            (* CR layouts v3.5: this hardcodes ['a or_null]. Fix when we allow
               users to write their own null constructors. *)
            | Variant_with_null when cstr.cstr_constant -> []
            (* CR layouts v3.3: allow all sorts. *)
            | Variant_with_null -> [ value ]
            | Variant_boxed _ | Variant_extensible ->
              (List.map (fun { ca_sort } -> of_const_sort ca_sort )
                 cstr.cstr_args)
          in
          List.iter2 (loop f) sorts patl
      | Tpat_record (lbl_pat_list, _) ->
          List.iter (fun (_, lbl, pat) ->
            (loop f) (of_const_sort lbl.lbl_sort) pat)
            lbl_pat_list
      | Tpat_record_unboxed_product (lbl_pat_list, _) ->
          List.iter (fun (_, lbl, pat) ->
            (loop f) (of_const_sort lbl.lbl_sort) pat)
            lbl_pat_list
      (* Cases where the inner things must be value: *)
      | Tpat_variant (_, pat, _) -> Option.iter (loop f value) pat
      | Tpat_tuple patl ->
        List.iter (fun (_, pat) -> loop f value pat) patl
        (* CR layouts v5: tuple case to change when we allow non-values in
           tuples *)
      | Tpat_unboxed_tuple patl ->
        List.iter (fun (_, pat, sort) -> loop f (of_sort sort) pat) patl
      | Tpat_array (_, arg_sort, patl) ->
        List.iter (loop f (of_sort arg_sort)) patl
      | Tpat_lazy p | Tpat_exception p -> loop f value p
      (* Cases without variables: *)
      | Tpat_any | Tpat_constant _ -> ()
  in
  loop f sort pat

let rev_pat_bound_idents_full ~of_sort ~of_const_sort sort pat =
  let idents_full = ref [] in
  let add id sloc typ uid _ sort =
    idents_full := (id, sloc, typ, uid, sort) :: !idents_full
  in
  iter_pattern_full
    ~both_sides_of_or:false ~of_sort ~of_const_sort
    add sort pat;
  !idents_full

let rev_only_idents idents_full =
  List.rev_map (fun (id,_,_,_,_) -> id) idents_full

let pat_bound_idents_full sort pat =
  List.rev (for_transl rev_pat_bound_idents_full sort pat)

(* In these two, we don't know the sort, but the sort information isn't used so
   it's fine to lie. *)
let pat_bound_idents pat =
  rev_only_idents
    (for_typing rev_pat_bound_idents_full Jkind.Sort.value pat)

let rev_let_bound_idents_full bindings =
  let idents_full = ref [] in
  let add id_full = idents_full := id_full :: !idents_full in
  List.iter (fun vb -> iter_bound_idents add vb.vb_pat) bindings;
  !idents_full

let let_bound_idents_with_modes_sorts_and_checks bindings =
  let modes_and_sorts = Ident.Tbl.create 3 in
  let f id sloc _ _uid mode sort =
    Ident.Tbl.add modes_and_sorts id (sloc.loc, mode, sort)
  in
  let checks =
    List.fold_left (fun checks vb ->
      for_typing iter_pattern_full
        ~both_sides_of_or:true
        f vb.vb_sort vb.vb_pat;
       match vb.vb_pat.pat_desc, vb.vb_expr.exp_desc with
       | Tpat_var (id, _, _, _), Texp_function fn ->
         let zero_alloc =
           match Zero_alloc.get fn.zero_alloc with
           | Default_zero_alloc ->
             (* We fabricate a "Check" attribute if a top-level annotation
                specifies that all functions should be checked for zero
                alloc. There is no need to update the zero_alloc variable on the
                function - if it remains [Default_zero_alloc], translcore adds
                the check. *)
             let arity = function_arity fn.params fn.body in
             if arity <= 0 then
               fn.zero_alloc
             else
               let create_const ~opt =
                 Zero_alloc.create_const
                   (Check { strict = false;
                            arity;
                            custom_error_msg = None;
                            loc = Location.none;
                            opt })
               in
               (match !Clflags.zero_alloc_assert with
                | Assert_default -> fn.zero_alloc
                | Assert_all -> create_const ~opt:false
                | Assert_all_opt -> create_const ~opt:true)
           | Ignore_assert_all | Check _ | Assume _ -> fn.zero_alloc
         in
         Ident.Map.add id zero_alloc checks
         (* CR ccasinghino: To keep the zero-alloc annotation info aliases, it
            may be enough to copy it if the vb_expr is an ident. *)
       | _ -> checks
    ) Ident.Map.empty bindings
  in
  List.rev_map
    (fun (id, _, _, _) ->
       let zero_alloc =
         Option.value (Ident.Map.find_opt id checks) ~default:Zero_alloc.default
       in
       id, List.rev (Ident.Tbl.find_all modes_and_sorts id), zero_alloc)
    (rev_let_bound_idents_full bindings)

let let_bound_idents_full bindings =
  List.rev (rev_let_bound_idents_full bindings)
let let_bound_idents pat =
  List.rev_map (fun (id,_,_,_) -> id) (rev_let_bound_idents_full pat)

let alpha_var env id = List.assoc id env

let rec alpha_pat
  : type k . _ -> k general_pattern -> k general_pattern
  = fun env p -> match p.pat_desc with
  | Tpat_var (id, s, uid, mode) -> (* note the ``Not_found'' case *)
      {p with pat_desc =
       try Tpat_var (alpha_var env id, s, uid, mode) with
       | Not_found -> Tpat_any}
  | Tpat_alias (p1, id, s, uid, mode, ty) ->
      let new_p =  alpha_pat env p1 in
      begin try
        {p with pat_desc =
           Tpat_alias (new_p, alpha_var env id, s, uid, mode, ty)}
      with
      | Not_found -> new_p
      end
  | d ->
     let pat_desc =
       shallow_map_pattern_desc { f = fun p -> alpha_pat env p } d in
     {p with pat_desc}

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc

let split_pattern pat =
  let combine_opts merge p1 p2 =
    match p1, p2 with
    | None, None -> None
    | Some p, None
    | None, Some p ->
        Some p
    | Some p1, Some p2 ->
        Some (merge p1 p2)
  in
  let into pat p1 p2 =
    (* The third parameter of [Tpat_or] is [Some _] only for "#typ"
       patterns, which we do *not* expand. Hence we can put [None] here. *)
    { pat with pat_desc = Tpat_or (p1, p2, None) } in
  let rec split_pattern cpat =
    match cpat.pat_desc with
    | Tpat_value p ->
        Some p, None
    | Tpat_exception p ->
        None, Some p
    | Tpat_or (cp1, cp2, _) ->
        let vals1, exns1 = split_pattern cp1 in
        let vals2, exns2 = split_pattern cp2 in
        combine_opts (into cpat) vals1 vals2,
        (* We could change the pattern type for exception patterns to
           [Predef.exn], but it doesn't really matter. *)
        combine_opts (into cpat) exns1 exns2
  in
  split_pattern pat

(* Expressions are considered nominal if they can be used as the subject of a
   sentence or action. In practice, we consider that an expression is nominal
   if they satisfy one of:
   - Similar to an identifier: words separated by '.' or '#'.
   - Do not contain spaces when printed.
  *)
let rec exp_is_nominal exp =
  match exp.exp_desc with
  | _ when exp.exp_attributes <> [] -> false
  | Texp_ident _ | Texp_instvar _ | Texp_constant _
  | Texp_variant (_, None)
  | Texp_construct (_, _, [], _) ->
      true
  | Texp_field (parent, _, _, _, _, _) | Texp_send (parent, _, _) ->
      exp_is_nominal parent
  | _ -> false

let loc_of_decl ~uid =
  let of_option { txt; loc } =
    match txt with
    | Some txt -> { txt; loc }
    | None -> { txt = ""; loc }
  in
  function
  | Value vd -> vd.val_name
  | Value_binding vb ->
    let bound_idents = let_bound_idents_full [vb] in
    let name = ListLabels.find_map
      ~f:(fun (_, name, _, uid') -> if uid = uid' then Some name else None)
      bound_idents in
    (match name with
    | Some name -> name
    | None ->
      (* The find_map will only fail if a bad uid was given. In that case, just
         use the location of the pattern on the left of the binding. *)
      { txt = ""; loc = vb.vb_pat.pat_loc })
  | Type td -> td.typ_name
  | Constructor cd -> cd.cd_name
  | Extension_constructor ec -> ec.ext_name
  | Label ld -> ld.ld_name
  | Module md -> of_option md.md_name
  | Module_binding mb -> of_option mb.mb_name
  | Module_type mtd -> mtd.mtd_name
  | Module_substitution msd -> msd.ms_name
  | Class cd -> cd.ci_id_name
  | Class_type ctd -> ctd.ci_id_name

let min_mode_with_locks = (Mode.Value.(disallow_right legacy), None)

let mode_without_locks_exn = function
  | (_, Some _) -> assert false
  | (m, None) -> m
