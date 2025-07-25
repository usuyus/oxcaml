(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Acc : sig
  (** The type of accumulator for flow analysis *)
  type t

  (** printing *)
  val print : Format.formatter -> t -> unit

  (* {2 Creation and updates} *)

  (** Empty uses *)
  val empty : unit -> t

  (** Initialize the analysis so that the stack consists of a single toplevel
      continuation. *)
  val init_toplevel :
    dummy_toplevel_cont:Continuation.t -> Bound_parameters.t -> t -> t

  (** Add a new continuation on the stack. Used when entering a continuation
      handler. *)
  val enter_continuation :
    Continuation.t ->
    recursive:bool ->
    is_exn_handler:bool ->
    Bound_parameters.t ->
    t ->
    t

  (** Pop the current top of the stack. Used when exiting the current
      continuation handler. *)
  val exit_continuation : Continuation.t -> t -> t

  (** That variable is defined in the current handler *)
  val record_defined_var : Variable.t -> t -> t

  (** Add a variable binding from the current handler. *)
  val record_var_binding :
    Variable.t -> Name_occurrences.t -> generate_phantom_lets:bool -> t -> t

  (** Record a let-binding *)
  val record_let_binding :
    rewrite_id:Named_rewrite_id.t ->
    generate_phantom_lets:bool ->
    let_bound:Bound_pattern.t ->
    simplified_defining_expr:Simplified_named.t ->
    t ->
    t

  (** Add a variable binding to the symbol. Projections might get recorded
      multiple times. *)
  val record_symbol_projection : Variable.t -> Name_occurrences.t -> t -> t

  (** Add a symbol binding from the current handler. *)
  val record_symbol_binding : Symbol.t -> Name_occurrences.t -> t -> t

  (** Add a code id binding from the current handler. *)
  val record_code_id_binding : Code_id.t -> Name_occurrences.t -> t -> t

  (** Add a value slot from the current handler. *)
  val record_value_slot : Name.t -> Value_slot.t -> Name_occurrences.t -> t -> t

  (** Add name occurrences used in the body of the current continuation's
      handler, *excluding* uses in apply_cont expressions, which are tracked
      separately. *)
  val add_used_in_current_handler : Name_occurrences.t -> t -> t

  (** Add the given continuation as being used as the return continuation for a
      function call. *)
  val add_apply_conts :
    result_cont:(Apply_cont_rewrite_id.t * Continuation.t) option ->
    exn_cont:Apply_cont_rewrite_id.t * Exn_continuation.t ->
    result_arity:[`Unarized] Flambda_arity.t ->
    t ->
    t

  (** Add, for the current continuation handler, uses for an apply cont of the
      given continuation with given arguments occurrences. *)
  val add_apply_cont_args :
    rewrite_id:Apply_cont_rewrite_id.t ->
    Continuation.t ->
    Simple.t list ->
    t ->
    t

  (** Add extra params and args to a continuation. *)
  val add_extra_params_and_args :
    Continuation.t -> Continuation_extra_params_and_args.t -> t -> t
end

module Analysis : sig
  (** Perform flow analysis *)
  val analyze :
    ?speculative:bool ->
    ?print_name:string ->
    return_continuation:Continuation.t ->
    exn_continuation:Continuation.t ->
    code_age_relation:Code_age_relation.t ->
    used_value_slots:Name_occurrences.t Or_unknown.t ->
    code_ids_to_never_delete:Code_id.Set.t ->
    specialization_map:Continuation.t Continuation_callsite_map.t ->
    Acc.t ->
    Flow_types.Flow_result.t

  (** [true] iff the mutable unboxing pass actually did unbox things *)
  val did_perform_mutable_unboxing : Flow_types.Flow_result.t -> bool

  (* [true] iff an alias to something useful (e.g. a constant, a symbol with a
     known type) has been added in a loop. *)
  val added_useful_alias_in_loop :
    Typing_env.t -> Acc.t -> Flow_types.Flow_result.t -> bool
end
