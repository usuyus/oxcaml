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

(* Operations on core types *)

open Asttypes
open Types
open Mode

exception Unify    of Errortrace.unification_error
exception Equality of Errortrace.equality_error
exception Moregen  of Errortrace.moregen_error
exception Subtype  of Errortrace.Subtype.error

exception Escape of type_expr Errortrace.escape

exception Tags of label * label
exception Cannot_expand
exception Cannot_apply
exception Incompatible
  (* Raised from [mcomp] *)

(* All the following wrapper functions revert to the original level,
   even in case of exception. *)
val with_local_level: ?post:('a -> unit) -> (unit -> 'a) -> 'a
        (* [with_local_level (fun () -> cmd) ~post] evaluates [cmd] at a
           raised level.
           If given, [post] is applied to the result, at the original level.
           It is expected to contain only level related post-processing. *)
val with_local_level_if: bool -> (unit -> 'a) -> post:('a -> unit) -> 'a
        (* Same as [with_local_level], but only raise the level conditionally.
           [post] also is only called if the level is raised. *)
val with_local_level_iter: (unit -> 'a * 'b list) -> post:('b -> unit) -> 'a
        (* Variant of [with_local_level], where [post] is iterated on the
           returned list. *)
val with_local_level_iter_if:
    bool -> (unit -> 'a * 'b list) -> post:('b -> unit) -> 'a
        (* Conditional variant of [with_local_level_iter] *)
val with_level: level: int -> (unit -> 'a) -> 'a
        (* [with_level ~level (fun () -> cmd)] evaluates [cmd] with
           [current_level] set to [level] *)
val with_level_if: bool -> level: int -> (unit -> 'a) -> 'a
        (* Conditional variant of [with_level] *)
val with_local_level_if_principal: (unit -> 'a) -> post:('a -> unit) -> 'a
val with_local_level_iter_if_principal:
    (unit -> 'a * 'b list) -> post:('b -> unit) -> 'a
        (* Applications of [with_local_level_if] and [with_local_level_iter_if]
           to [!Clflags.principal] *)

val with_local_level_for_class: ?post:('a -> unit) -> (unit -> 'a) -> 'a
        (* Variant of [with_local_level], where the current level is raised but
           the nongen level is not touched *)
val with_raised_nongen_level: (unit -> 'a) -> 'a
        (* Variant of [with_local_level],
           raises the nongen level to the current level *)

val reset_global_level: unit -> unit
        (* Reset the global level before typing an expression *)
val increase_global_level: unit -> int
val restore_global_level: int -> unit
        (* This pair of functions is only used in Typetexp *)

val create_scope : unit -> int

val newty: type_desc -> type_expr
val new_scoped_ty: int -> type_desc -> type_expr
val newvar: ?name:string -> jkind_lr -> type_expr

val new_rep_var
  : ?name:string
  -> why:Jkind.History.concrete_creation_reason
  -> unit
  -> type_expr * Jkind.sort
        (* Return a fresh representable variable, along with its sort *)
val newvar2: ?name:string -> int -> jkind_lr -> type_expr
        (* Return a fresh variable *)
val new_global_var: ?name:string -> jkind_lr -> type_expr
        (* Return a fresh variable, bound at toplevel
           (as type variables ['a] in type constraints). *)
val newobj: type_expr -> type_expr
val newconstr: Path.t -> type_expr list -> type_expr
val newmono : type_expr -> type_expr
val none: type_expr
        (* A dummy type expression *)

val object_fields: type_expr -> type_expr
val flatten_fields:
        type_expr -> (string * field_kind * type_expr) list * type_expr
(** Transform a field type into a list of pairs label-type.
    The fields are sorted.

    Beware of the interaction with GADTs:

    Due to the introduction of object indexes for GADTs, the row variable of
    an object may now be an expansible type abbreviation.
    A first consequence is that [flatten_fields] will not completely flatten
    the object, since the type abbreviation will not be expanded
    ([flatten_fields] does not receive the current environment).
    Another consequence is that various functions may be called with the
    expansion of this type abbreviation, which is a Tfield, e.g. during
    printing.

    Concrete problems have been fixed, but new bugs may appear in the
    future. (Test cases were added to typing-gadts/test.ml)
*)

val associate_fields:
        (string * field_kind * type_expr) list ->
        (string * field_kind * type_expr) list ->
        (string * field_kind * type_expr * field_kind * type_expr) list *
        (string * field_kind * type_expr) list *
        (string * field_kind * type_expr) list
val opened_object: type_expr -> bool
val set_object_name:
        Ident.t -> type_expr list -> type_expr -> unit
val remove_object_name: type_expr -> unit
val find_cltype_for_path: Env.t -> Path.t -> type_declaration * type_expr

val sort_row_fields: (label * row_field) list -> (label * row_field) list
val merge_row_fields:
        (label * row_field) list -> (label * row_field) list ->
        (label * row_field) list * (label * row_field) list *
        (label * row_field * row_field) list
val filter_row_fields:
        bool -> (label * row_field) list -> (label * row_field) list

val generalize: type_expr -> unit
        (* Generalize in-place the given type *)
val lower_contravariant: Env.t -> type_expr -> unit
        (* Lower level of type variables inside contravariant branches;
           to be used before generalize for expansive expressions *)
val lower_variables_only: Env.t -> int -> type_expr -> unit
        (* Lower all variables to the given level *)
val enforce_current_level: Env.t -> type_expr -> unit
        (* Lower whole type to !current_level *)
val generalize_structure: type_expr -> unit
        (* Generalize the structure of a type, lowering variables
           to !current_level *)
val generalize_class_type : class_type -> unit
        (* Generalize the components of a class type *)
val generalize_class_type_structure : class_type -> unit
       (* Generalize the structure of the components of a class type *)
val generalize_class_signature_spine : Env.t -> class_signature -> unit
       (* Special function to generalize methods during inference *)
val correct_levels: type_expr -> type_expr
        (* Returns a copy with decreasing levels *)
val limited_generalize: type_expr -> type_expr -> unit
        (* Only generalize some part of the type
           Make the remaining of the type non-generalizable *)
val limited_generalize_class_type: type_expr -> class_type -> unit
        (* Same, but for class types *)

val fully_generic: type_expr -> bool

val check_scope_escape : Env.t -> int -> type_expr -> unit
        (* [check_scope_escape env lvl ty] ensures that [ty] could be raised
           to the level [lvl] without any scope escape.
           Raises [Escape] otherwise *)

val instance: ?partial:bool -> type_expr -> type_expr
        (* Take an instance of a type scheme *)
        (* partial=None  -> normal
           partial=false -> newvar() for non generic subterms
           partial=true  -> newty2 ty.level Tvar for non generic subterms *)
val generic_instance: type_expr -> type_expr
        (* Same as instance, but new nodes at generic_level *)
val instance_list: type_expr list -> type_expr list
        (* Take an instance of a list of type schemes *)
val new_local_type:
        ?loc:Location.t -> ?manifest_and_scope:(type_expr * int) ->
        type_origin -> (allowed * 'r) jkind -> type_declaration

module Pattern_env : sig
  type t = private
    { mutable env : Env.t;
      equations_scope : int;
      (* scope for local type declarations *)
      allow_recursive_equations : bool;
      (* true iff checking counter examples *)
    }
  val make: Env.t -> equations_scope:int -> allow_recursive_equations:bool -> t
  val copy: ?equations_scope:int -> t -> t
  val set_env: t -> Env.t -> unit
end

type existential_treatment =
  | Keep_existentials_flexible
  | Make_existentials_abstract of Pattern_env.t

val instance_constructor: existential_treatment ->
        constructor_description ->
        Types.constructor_argument list * type_expr * type_expr list
        (* Same, for a constructor. Also returns existentials. *)
val instance_parameterized_type:
        ?keep_names:bool ->
        type_expr list -> type_expr -> type_expr list * type_expr
val instance_declaration: type_declaration -> type_declaration
val generic_instance_declaration: type_declaration -> type_declaration
        (* Same as instance_declaration, but new nodes at generic_level *)
val instance_class:
        type_expr list -> class_type -> type_expr list * class_type

val instance_poly:
        ?keep_names:bool ->
        type_expr list -> type_expr -> type_expr
        (* Take an instance of a type scheme containing free univars *)
val instance_poly_fixed:
        ?keep_names:bool ->
        type_expr list -> type_expr -> type_expr list * type_expr
        (* Take an instance of a type scheme containing free univars for
           checking that an expression matches this scheme. *)

val polyfy: Env.t -> type_expr -> type_expr list -> type_expr * bool
val instance_label:
        fixed:bool ->
        _ gen_label_description -> type_expr list * type_expr * type_expr
        (* Same, for a label *)
val prim_mode :
        (Mode.allowed * 'r) Mode.Locality.t option -> (Primitive.mode * Primitive.native_repr)
        -> (Mode.allowed * 'r) Mode.Locality.t
val instance_prim:
        Primitive.description -> type_expr ->
        type_expr * Mode.Locality.lr option
        * Mode.Yielding.lr option * Jkind.Sort.t option

(** Given (a @ m1 -> b -> c) @ m0, where [m0] and [m1] are modes expressed by
    user-syntax, [curry_mode m0 m1] gives the mode we implicitly interpret b->c
    to have. *)
val curry_mode : Alloc.Const.t -> Alloc.Const.t -> Alloc.Const.t

val apply:
        ?use_current_level:bool ->
        Env.t -> type_expr list -> type_expr -> type_expr list -> type_expr
        (* [apply [p1...pN] t [a1...aN]] applies the type function
           [fun p1 ... pN -> t] to the arguments [a1...aN] and returns the
           resulting instance of [t].
           New nodes default to generic level except if [use_current_level] is
           set to true.
           Exception [Cannot_apply] is raised in case of failure. *)

val try_expand_once_opt: Env.t -> type_expr -> type_expr
val try_expand_safe_opt: Env.t -> type_expr -> type_expr

val expand_head_once: Env.t -> type_expr -> type_expr
val expand_head: Env.t -> type_expr -> type_expr
val expand_head_opt: Env.t -> type_expr -> type_expr
(** The compiler's own version of [expand_head] necessary for type-based
    optimisations. *)

(** Expansion of types for error traces; lives here instead of in [Errortrace]
    because the expansion machinery lives here. *)

(** Create an [Errortrace.Diff] by expanding the two types *)
val expanded_diff :
  Env.t ->
  got:type_expr -> expected:type_expr ->
  (Errortrace.expanded_type, 'variant) Errortrace.elt

(** Create an [Errortrace.Diff] by *duplicating* the two types, so that each
    one's expansion is identical to itself.  Despite the name, does create
    [Errortrace.expanded_type]s. *)
val unexpanded_diff :
  got:type_expr -> expected:type_expr ->
  (Errortrace.expanded_type, 'variant) Errortrace.elt

val full_expand: may_forget_scope:bool -> Env.t -> type_expr -> type_expr

type typedecl_extraction_result =
  | Typedecl of Path.t * Path.t * type_declaration
    (* The original path of the types, and the first concrete
       type declaration found expanding it. *)
  | Has_no_typedecl
  | May_have_typedecl

val extract_concrete_typedecl:
        Env.t -> type_expr -> typedecl_extraction_result

val unify: Env.t -> type_expr -> type_expr -> unit
        (* Unify the two types given. Raise [Unify] if not possible. *)
val unify_gadt:
        Pattern_env.t -> type_expr -> type_expr -> Btype.TypePairs.t
        (* Unify the two types given and update the environment with the
           local constraints. Raise [Unify] if not possible.
           Returns the pairs of types that have been equated.  *)
val unify_var: Env.t -> type_expr -> type_expr -> unit
        (* Same as [unify], but allow free univars when first type
           is a variable. *)
val unify_delaying_jkind_checks :
  Env.t -> type_expr -> type_expr -> (type_expr * jkind_r) list
        (* Same as [unify], but don't check jkind compatibility.  Instead,
           return the checks that would have been performed.  For use in
           typedecl before well-foundedness checks have made jkind checking
           safe. *)

type filtered_arrow =
  { ty_arg : type_expr;
    arg_mode : Mode.Alloc.lr;
    ty_ret : type_expr;
    ret_mode : Mode.Alloc.lr
  }

val filter_arrow: Env.t -> type_expr -> arg_label -> force_tpoly:bool ->
                  filtered_arrow
        (* A special case of unification (with l:'a -> 'b). If
           [force_poly] is false then the usual invariant that the
           argument type be a [Tpoly] node is not enforced. Raises
           [Filter_arrow_failed] instead of [Unify].  *)
val filter_mono: type_expr -> type_expr
        (* A special case of unification (with Tpoly('a, [])). Can
           only be called on [Tpoly] nodes. Raises [Filter_mono_failed]
           instead of [Unify] *)
val filter_arrow_mono: Env.t -> type_expr -> arg_label -> filtered_arrow
        (* A special case of unification. Composition of [filter_arrow]
           with [filter_mono] on the argument type. Raises
           [Filter_arrow_mono_failed] instead of [Unify] *)
val filter_method: Env.t -> string -> type_expr -> type_expr
        (* A special case of unification (with {m : 'a; 'b}).  Raises
           [Filter_method_failed] instead of [Unify]. *)
val occur_in: Env.t -> type_expr -> type_expr -> bool
val deep_occur_list: type_expr -> type_expr list -> bool
        (* Check whether a type occurs structurally within any type from
           a list of types. *)
val deep_occur: type_expr -> type_expr -> bool
        (* Check whether a type occurs structurally within another. *)
val moregeneral: Env.t -> bool -> type_expr -> type_expr -> unit
        (* Check if the first type scheme is more general than the second. *)
val is_moregeneral: Env.t -> bool -> type_expr -> type_expr -> bool
val all_distinct_vars: Env.t -> type_expr list -> bool
        (* Check those types are all distinct type variables *)

type matches_result =
  | Unification_failure of Errortrace.unification_error
  | Jkind_mismatch of { original_jkind : jkind_lr; inferred_jkind : jkind_lr
                      ; ty : type_expr }
  | All_good
val matches: expand_error_trace:bool -> Env.t ->
  type_expr -> type_expr -> matches_result
        (* Same as [moregeneral false], implemented using the two above
           functions and backtracking. Ignore levels. The [expand_error_trace]
           flag controls whether the error raised performs expansion; this
           should almost always be [true]. *)

val reify_univars : Env.t -> Types.type_expr -> Types.type_expr
        (* Replaces all the variables of a type by a univar. *)

(* Exceptions for special cases of unify *)

type filter_arrow_failure =
  | Unification_error of Errortrace.unification_error
  | Label_mismatch of
      { got           : arg_label
      ; expected      : arg_label
      ; expected_type : type_expr
      }
  | Not_a_function
  | Jkind_error of type_expr * Jkind.Violation.t

exception Filter_arrow_failed of filter_arrow_failure

exception Filter_mono_failed
exception Filter_arrow_mono_failed

type filter_method_failure =
  | Unification_error of Errortrace.unification_error
  | Not_a_method
  | Not_an_object of type_expr
  | Not_a_value of Jkind.Violation.t

exception Filter_method_failed of filter_method_failure

type class_match_failure =
    CM_Virtual_class
  | CM_Parameter_arity_mismatch of int * int
  | CM_Type_parameter_mismatch of int * Env.t * Errortrace.equality_error
  | CM_Class_type_mismatch of Env.t * class_type * class_type
  | CM_Parameter_mismatch of int * Env.t * Errortrace.moregen_error
  | CM_Val_type_mismatch of string * Env.t * Errortrace.comparison_error
  | CM_Meth_type_mismatch of string * Env.t * Errortrace.comparison_error
  | CM_Non_mutable_value of string
  | CM_Non_concrete_value of string
  | CM_Missing_value of string
  | CM_Missing_method of string
  | CM_Hide_public of string
  | CM_Hide_virtual of string * string
  | CM_Public_method of string
  | CM_Private_method of string
  | CM_Virtual_method of string

val match_class_types:
    ?trace:bool -> Env.t -> class_type -> class_type -> class_match_failure list
        (* Check if the first class type is more general than the second. *)
val equal: ?do_jkind_check:bool ->
  Env.t -> bool -> type_expr list -> type_expr list -> unit
        (* [equal env [x1...xn] tau [y1...yn] sigma]
           checks whether the parameterized types
           [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)
val is_equal : Env.t -> bool -> type_expr list -> type_expr list -> bool
val equal_private : Env.t -> type_expr -> type_expr -> unit
(* [equal_private env t1 t2] checks that [t1] equals [t2] but it is allowed to
   expand [t1] if it is a private abbreviation. No renaming is allowed, but
   jkinds are checked. *)

val match_class_declarations:
        Env.t -> type_expr list -> class_type -> type_expr list ->
        class_type -> class_match_failure list
        (* Check if the first class type is more general than the second. *)

val enlarge_type: Env.t -> type_expr -> type_expr * bool
        (* Make a type larger, flag is true if some pruning had to be done *)
val subtype: Env.t -> type_expr -> type_expr -> unit -> unit
        (* [subtype env t1 t2] checks that [t1] is a subtype of [t2].
           It accumulates the constraints the type variables must
           enforce and returns a function that enforces this
           constraints. *)

(* This module allows a type to become "rigid": after unifying such a type,
   we can check to see whether any of its variables were unified, issuing
   an error in such a case. *)
module Rigidify : sig
  type t

  type matches_result =
    (* A variable with name [name] has been unified with a type [ty]. *)
    | Unification_failure of
        { name : string option
        ; ty : type_expr }

    (* A variable [ty] started with an [original_jkind] but now has a
       [inferred_jkind]. *)
    | Jkind_mismatch of
        { original_jkind : jkind_lr; inferred_jkind : jkind_lr; ty : type_expr }

    (* No problems *)
    | All_good

  (* Mark all the variables in the types given as rigid; these will be tracked
     for unification. Every call to this function should be paired with a call
     to [all_distinct_vars_with_original_jkinds]. *)
  val rigidify_list : type_expr list -> t

  (* Check that no variables in a [t] have actually been unified. *)
  val all_distinct_vars_with_original_jkinds : Env.t -> t -> matches_result
end

(* Operations on class signatures *)

val new_class_signature : unit -> class_signature
val add_dummy_method : Env.t -> scope:int -> class_signature -> unit

type add_method_failure =
  | Unexpected_method
  | Type_mismatch of Errortrace.unification_error

exception Add_method_failed of add_method_failure

val add_method : Env.t ->
  label -> private_flag -> virtual_flag -> type_expr -> class_signature -> unit

type add_instance_variable_failure =
  | Mutability_mismatch of mutable_flag
  | Type_mismatch of Errortrace.unification_error

exception Add_instance_variable_failed of add_instance_variable_failure

val add_instance_variable : strict:bool -> Env.t ->
  label -> mutable_flag -> virtual_flag -> type_expr -> class_signature -> unit

type inherit_class_signature_failure =
  | Self_type_mismatch of Errortrace.unification_error
  | Method of label * add_method_failure
  | Instance_variable of label * add_instance_variable_failure

exception Inherit_class_signature_failed of inherit_class_signature_failure

val inherit_class_signature : strict:bool -> Env.t ->
  class_signature -> class_signature -> unit

val update_class_signature :
  Env.t -> class_signature -> label list * label list

val hide_private_methods : Env.t -> class_signature -> unit

val close_class_signature : Env.t -> class_signature -> bool

exception Nondep_cannot_erase of Ident.t

val nondep_type: Env.t -> Ident.t list -> type_expr -> type_expr
        (* Return a type equivalent to the given type but without
           references to any of the given identifiers.
           Raise [Nondep_cannot_erase id] if no such type exists because [id],
           in particular, could not be erased. *)
val nondep_type_decl:
        Env.t -> Ident.t list -> bool -> type_declaration -> type_declaration
        (* Same for type declarations. *)
val nondep_extension_constructor:
        Env.t -> Ident.t list -> extension_constructor ->
        extension_constructor
          (* Same for extension constructor *)
val nondep_class_declaration:
        Env.t -> Ident.t list -> class_declaration -> class_declaration
        (* Same for class declarations. *)
val nondep_cltype_declaration:
  Env.t -> Ident.t list -> class_type_declaration -> class_type_declaration
        (* Same for class type declarations. *)
(*val correct_abbrev: Env.t -> Path.t -> type_expr list -> type_expr -> unit*)
val is_contractive: Env.t -> Path.t -> bool
val normalize_type: type_expr -> unit

val remove_mode_and_jkind_variables: type_expr -> unit
        (* Ensure mode and jkind variables are fully determined *)

val nongen_vars_in_schema: Env.t -> type_expr -> Btype.TypeSet.t option
        (* Return any non-generic variables in the type scheme.  Also ensures
           mode variables are fully determined. *)

val nongen_vars_in_class_declaration:class_declaration -> Btype.TypeSet.t option
        (* Return any non-generic variables in the class type.  Also ensures
           mode variables are fully determined. Uses the empty environment.  *)

type variable_kind = Row_variable | Type_variable
type closed_class_failure = {
  free_variable: type_expr * variable_kind;
  meth: string;
  meth_ty: type_expr;
}

val free_variables: ?env:Env.t -> type_expr -> type_expr list
        (* If env present, then check for incomplete definitions too;
           returns both normal variables and row variables*)
val free_non_row_variables_of_list: type_expr list -> type_expr list
        (* gets only non-row variables *)
val free_variable_set_of_list: Env.t -> type_expr list -> Btype.TypeSet.t
        (* post-condition: all elements in the set are Tvars *)

val exists_free_variable : (type_expr -> jkind_lr -> bool) -> type_expr -> bool
        (* Check if there exists a free variable that satisfies the
           given predicate. *)

val closed_type_expr: ?env:Env.t -> type_expr -> bool
        (* If env present, expand abbreviations to see if expansion
           eliminates the variable *)

val closed_type_decl: type_declaration -> type_expr option
val closed_extension_constructor: extension_constructor -> type_expr option
val closed_class:
        type_expr list -> class_signature ->
        closed_class_failure option
        (* Check whether all type variables are bound *)

val unalias: type_expr -> type_expr

val arity: type_expr -> int
        (* Return the arity (as for curried functions) of the given type. *)

val collapse_conj_params: Env.t -> type_expr list -> unit
        (* Collapse conjunctive types in class parameters *)

val get_current_level: unit -> int
val wrap_trace_gadt_instances: Env.t -> ('a -> 'b) -> 'a -> 'b

(* Stubs *)
val package_subtype :
    (Env.t -> Path.t -> (Longident.t * type_expr) list ->
      Path.t -> (Longident.t * type_expr) list -> bool) ref

(* Raises [Incompatible] *)
val mcomp : Env.t -> type_expr -> type_expr -> unit

(* represents a type that has been extracted from wrappers that
   do not change its runtime representation, such as [@@unboxed]
   types and [Tpoly]s *)
type unwrapped_type_expr =
  { ty : type_expr
  ; is_open : bool  (* are there any unbound variables in this type? *)
  ; modality : Mode.Modality.Value.Const.t }

val get_unboxed_type_representation :
  Env.t ->
  type_expr ->
  (unwrapped_type_expr, unwrapped_type_expr) result
    (* [get_unboxed_type_representation] attempts to fully expand the input
       type_expr, descending through [@@unboxed] types.  May fail in the case of
       circular types or very deeply nested unboxed types, in which case it
       returns the most expanded version it was able to compute. *)

val get_unboxed_type_approximation :
  Env.t -> type_expr -> unwrapped_type_expr
    (* [get_unboxed_type_approximation] does the same thing as
       [get_unboxed_type_representation], but doesn't indicate whether the type
       was fully expanded or not. *)

val contained_without_boxing : Env.t -> type_expr -> type_expr list
    (* Return all types that are directly contained without boxing
       (or "without indirection" or "flatly"); in the case of [@@unboxed]
       existentials, these types might have free variables*)


(* Cheap upper bound on jkind.  Will not expand unboxed types - call
   [type_jkind] if that's needed. *)
val estimate_type_jkind : Env.t ->  type_expr -> jkind_l

(* Get the jkind of a type, expanding it and looking through [[@@unboxed]]
   types. *)
val type_jkind : Env.t -> type_expr -> jkind_l

(* Get the jkind of a type, dropping any changes to types caused by
   expansion. *)
val type_jkind_purely : Env.t -> type_expr -> jkind_l

(* Like [type_jkind_purely], but returns [None] if the type is not
   principally known. Useful to instantiate [jkind_of_type] in various
   functions exported by [Jkind]. *)
val type_jkind_purely_if_principal : Env.t -> type_expr -> jkind_l option

(* Find a type's sort (if fixed is false: constraining it to be an
   arbitrary sort variable, if needed) *)
val type_sort :
  why:Jkind.History.concrete_creation_reason ->
  fixed:bool ->
  Env.t -> type_expr -> (Jkind.sort, Jkind.Violation.t) result

(* As [type_sort ~fixed:false], but constrain the jkind to be non-null.
   Used for checking array elements. *)
val type_legacy_sort :
  why:Jkind.History.concrete_legacy_creation_reason ->
  Env.t -> type_expr -> (Jkind.sort, Jkind.Violation.t) result

(* Jkind checking. [constrain_type_jkind] will update the jkind of type
   variables to make the check true, if possible.  [check_decl_jkind] and
   [check_type_jkind] won't, but will still instantiate sort variables.
 *)

(* These two functions check against an l-jkind. This is highly unusual,
   but correct: they are used to implement the module inclusion check, where
   we can be sure that the l-jkind has no undetermined variables. *)
val check_decl_jkind :
  Env.t -> type_declaration -> jkind_l -> (unit, Jkind.Violation.t) result
val constrain_decl_jkind :
  Env.t -> type_declaration -> jkind_l -> (unit, Jkind.Violation.t) result

(* Compare two types for equality, with no renaming. This is useful for
   the [type_equal] function that must be passed to certain jkind functions. *)
val type_equal: Env.t -> type_expr -> type_expr -> bool

val check_type_jkind :
  Env.t -> type_expr -> ('l * allowed) jkind -> (unit, Jkind.Violation.t) result
val constrain_type_jkind :
  Env.t -> type_expr -> ('l * allowed) jkind -> (unit, Jkind.Violation.t) result

(* Check whether a type's externality's upper bound is less than some target.
   Potentially cheaper than just calling [type_jkind], because this can stop
   expansion once it succeeds. *)
val check_type_externality :
  Env.t -> type_expr -> Jkind_axis.Externality.t -> bool

(* Check whether a type's nullability is less than some target.
   Uses get_nullability which is potentially cheaper than calling type_jkind
   if all with-bounds are irrelevant. *)
val check_type_nullability :
  Env.t -> type_expr -> Jkind_axis.Nullability.t -> bool

(* Check whether a type's separability is less than some target.
   Potentially cheaper than just calling [type_jkind], because this can stop
   expansion once it succeeds. *)
val check_type_separability :
  Env.t -> type_expr -> Jkind_axis.Separability.t -> bool

(* This function should get called after a type is generalized.

   It does two things:

   1. Update the jkind reason of all generalized type vars inside the
      given [type_expr]

   Consider some code like

    {[
      let f : ('a : immediate). 'a -> 'a = fun x -> x in
      let y = f "hello" in ...
    ]}

   This should be rejected, because a string is not immediate. But how should
   we explain how the requirement to pass an immediate arises? We could point
   the user to the [: immediate] annotation within the definition for [f]. But this
   definition might be arbitrarily far away (including in another file), and the inference
   to produce the fact that the type it works on must be immediate might be complex.

   The design decision here is not to look within well-typed definitions for further
   information about how jkind decisions arose. Other tooling -- such as merlin --
   is more well suited for discovering properties of well-typed definitions. Instead,
   once a definition is done being type-checked -- that is, once it is generalized --
   we update the histories of all of its types' jkinds to just refer to the definition
   itself.

   2. Performs an upstream-compatibility check around immediacy if
      [Language_extension.erasable_extensions_only ()] is [true].

   The check makes sure no generalized type variable can have jkind
   [immediate] or [immediate64]. An exception would be raised when
   the check fails.

   This prevents code such as:

   {|
     let f (x : (_ : immediate)) = x;;
   |}

   which doesn't have an equivalent representation upstream.

   *)
val check_and_update_generalized_ty_jkind :
  ?name:Ident.t -> loc:Location.t -> Env.t -> type_expr -> unit

(* False if running in principal mode and the type is not principal.
   True otherwise. *)
val is_principal : type_expr -> bool

(* For use with ocamldebug *)
type global_state
val global_state : global_state
val print_global_state : Format.formatter -> global_state -> unit

(** Get the crossing of a jkind  *)
val crossing_of_jkind : Env.t -> 'd Types.jkind -> Mode.Crossing.t

(** Get the crossing of a type wrapped in modalities. Non-principal types get
    trivial crossing. *)
val crossing_of_ty :
  Env.t ->
  ?modalities:Mode.Modality.Value.Const.t ->
  Types.type_expr ->
  Mode.Crossing.t

(** Cross a right mode according to a type wrapped in modalities. Non-principal
    types don't cross. *)
val cross_right :
  Env.t ->
  ?modalities:Mode.Modality.Value.Const.t ->
  Types.type_expr ->
  Mode.Value.r ->
  Mode.Value.r

(** Cross a left mode according to a type wrapped in modalities. Non-principal
    types don't cross. *)
val cross_left :
  Env.t ->
  ?modalities:Mode.Modality.Value.Const.t ->
  Types.type_expr ->
  Mode.Value.l ->
  Mode.Value.l

(** Similar to [cross_right] but for [Mode.Alloc]  *)
val cross_right_alloc :
  Env.t ->
  ?modalities:Mode.Modality.Value.Const.t ->
  Types.type_expr ->
  Mode.Alloc.r ->
  Mode.Alloc.r

(** Similar to [cross_left] but for [Mode.Alloc]  *)
val cross_left_alloc :
  Env.t ->
  ?modalities:Mode.Modality.Value.Const.t ->
  Types.type_expr ->
  Mode.Alloc.l ->
  Mode.Alloc.l

(** Zap a modality to floor if the [modes] extension is enabled at a level more
    immature than the given one. Zap to id otherwise. *)
val zap_modalities_to_floor_if_modes_enabled_at :
  Language_extension.maturity ->
  Mode.Modality.Value.t ->
  Mode.Modality.Value.Const.t

(** The mode crossing of the memory block of a structure. *)
val mode_crossing_structure_memaddr : Mode.Crossing.t

(** The mode crossing of a functor. *)
val mode_crossing_functor : Mode.Crossing.t

(** The mode crossing of any module. *)
val mode_crossing_module : Mode.Crossing.t

(** Zap a modality to floor if maturity allows, zap to id otherwise. *)
val zap_modalities_to_floor_if_at_least :
  Language_extension.maturity ->
  Mode.Modality.Value.t ->
  Mode.Modality.Value.Const.t
