(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** "Primitive" operations: those that perform computation but never affect
    control flow.

    Primitives that accept float, int32, int64 or nativeint values always take
    (or return) the unboxed versions.

    No primitive raises an exception. (Bounds checking is handled separately.) *)

module Lazy_block_tag : sig
  type t = Lambda.lazy_block_tag =
    | Lazy_tag
    | Forward_tag

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val to_tag : t -> Tag.t
end

module Block_kind : sig
  type t =
    | Values of Tag.Scannable.t * Flambda_kind.With_subkind.t list
    | Naked_floats
    | Mixed of Tag.Scannable.t * Flambda_kind.Mixed_block_shape.t

  val to_shape : t -> Tag.t * Flambda_kind.Block_shape.t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Array_kind : sig
  type t =
    | Immediates  (** An array consisting only of immediate values. *)
    | Values
        (** An array consisting of elements of kind [value]. With the float
            array optimisation enabled, such elements must never be [float]s. *)
    | Naked_floats
        (** An array consisting of naked floats, represented using
            [Double_array_tag]. *)
    | Naked_float32s
    | Naked_int32s
    | Naked_int64s
    | Naked_nativeints
    | Naked_vec128s
    | Naked_vec256s
    | Naked_vec512s
    | Unboxed_product of t list
        (** Accesses to arrays of unboxed products are unarized on the way into
            Flambda 2.  The float array optimization never applies for these
            arrays.  Vectors are not yet supported inside these arrays. *)

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val element_kinds : t -> Flambda_kind.With_subkind.t list

  val must_be_gc_scannable : t -> bool

  val width_in_scalars : t -> int
end

module Array_kind_for_length : sig
  type t =
    | Array_kind of Array_kind.t
    | Float_array_opt_dynamic

  val width_in_scalars : t -> int
end

module Init_or_assign : sig
  type t =
    | Initialization
    | Assignment of Alloc_mode.For_assignments.t

  val to_lambda : t -> Lambda.initialization_or_assignment
end

module Array_load_kind : sig
  type t =
    | Immediates  (** An array consisting only of immediate values. *)
    | Values
        (** An array consisting of elements of kind [value]. With the float
            array optimisation enabled, such elements must never be [float]s. *)
    | Naked_floats
        (** An array consisting of naked floats, represented using
            [Double_array_tag]. *)
    | Naked_float32s
    | Naked_int32s
    | Naked_int64s
    | Naked_nativeints
    | Naked_vec128s
    | Naked_vec256s
    | Naked_vec512s

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Array_set_kind : sig
  type t =
    | Immediates  (** An array consisting only of immediate values. *)
    | Values of Init_or_assign.t
        (** An array consisting of elements of kind [value]. With the float
        array optimisation enabled, such elements must never be [float]s. *)
    | Naked_floats
        (** An array consisting of naked floats, represented using
        [Double_array_tag]. *)
    | Naked_float32s
    | Naked_int32s
    | Naked_int64s
    | Naked_nativeints
    | Naked_vec128s
    | Naked_vec256s
    | Naked_vec512s

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Duplicate_block_kind : sig
  type t =
    | Values of
        { tag : Tag.Scannable.t;
          length : Targetint_31_63.t
        }
    | Naked_floats of { length : Targetint_31_63.t }
    | Mixed
        (** We could store tag/length (or other relevant fields) on [Mixed],
            but we don't because the fields of [t] are currently only used for
            printing.
        *)

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Duplicate_array_kind : sig
  type t =
    | Immediates
    | Values
    | Naked_floats of { length : Targetint_31_63.t option }
    | Naked_float32s of { length : Targetint_31_63.t option }
    | Naked_int32s of { length : Targetint_31_63.t option }
    | Naked_int64s of { length : Targetint_31_63.t option }
    | Naked_nativeints of { length : Targetint_31_63.t option }
    | Naked_vec128s of { length : Targetint_31_63.t option }
    | Naked_vec256s of { length : Targetint_31_63.t option }
    | Naked_vec512s of { length : Targetint_31_63.t option }

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Block_access_field_kind : sig
  (* CR mshinwell: For [Block_load] this is always [Any_value]. *)
  type t =
    | Any_value
    | Immediate

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Mixed_block_access_field_kind : sig
  type t =
    | Value_prefix of Block_access_field_kind.t
    | Flat_suffix of Flambda_kind.Flat_suffix_element.t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module Block_access_kind : sig
  type t =
    | Values of
        { tag : Tag.Scannable.t Or_unknown.t;
          size : Targetint_31_63.t Or_unknown.t;
          field_kind : Block_access_field_kind.t
        }
    | Naked_floats of { size : Targetint_31_63.t Or_unknown.t }
    | Mixed of
        { tag : Tag.Scannable.t Or_unknown.t;
          size : Targetint_31_63.t Or_unknown.t;
          field_kind : Mixed_block_access_field_kind.t;
          shape : Flambda_kind.Mixed_block_shape.t
        }

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val element_kind_for_load : t -> Flambda_kind.t

  val element_subkind_for_load : t -> Flambda_kind.With_subkind.t

  val to_block_shape : t -> Flambda_kind.Block_shape.t
end

(* CR-someday mshinwell: We should have unboxed arrays of int32, int64 and
   nativeint. *)

(* CR mshinwell: An old comment: the "bit test" primitive now needs to be
   compiled out in Lambda_to_flambda. It indexes into a string using a number of
   bits. (See cmmgen.ml) Something that is odd about this primitive is that it
   does not appear to have a bounds check. Maybe it should? *)

type string_or_bytes =
  | String
  | Bytes

type 'signed_or_unsigned comparison =
  | Eq
  | Neq
  | Lt of 'signed_or_unsigned
  | Gt of 'signed_or_unsigned
  | Le of 'signed_or_unsigned
  | Ge of 'signed_or_unsigned

type equality_comparison =
  | Eq
  | Neq

module Bigarray_kind : sig
  type t =
    | Float16
        (** This is analogous to [Float32] in that whilst storage is 16-bit,
            reading and writing goes via 64-bit floats. *)
    | Float32
    | Float32_t
        (** [Float32_t] is used for bigarrays that contain (unboxed) float32
            values and are read and written to using the [float32] type.  This
            is in contrast to [Float32] bigarrays, where the accesses are done
            at type [float]. *)
    | Float64
    | Sint8
    | Uint8
    | Sint16
    | Uint16
    | Int32
    | Int64
    | Int_width_int
    | Targetint_width_int
    | Complex32
    | Complex64

  val element_kind : t -> Flambda_kind.t

  val from_lambda : Lambda.bigarray_kind -> t option

  val to_lambda : t -> Lambda.bigarray_kind
end

module Bigarray_layout : sig
  type t =
    | C
    | Fortran

  val from_lambda : Lambda.bigarray_layout -> t option
end

type string_accessor_width =
  | Eight
  | Sixteen
  | Thirty_two
  | Single
  | Sixty_four
  | One_twenty_eight of { aligned : bool }
  | Two_fifty_six of { aligned : bool }
  | Five_twelve of { aligned : bool }

val kind_of_string_accessor_width : string_accessor_width -> Flambda_kind.t

val byte_width_of_string_accessor_width : string_accessor_width -> int

type float_bitwidth =
  | Float32
  | Float64

type string_like_value =
  | String
  | Bytes
  | Bigstring

type bytes_like_value =
  | Bytes
  | Bigstring

type num_dimensions = int

type signed_or_unsigned =
  | Signed
  | Unsigned

(** Primitives taking exactly zero arguments. *)
type nullary_primitive =
  (* CR mshinwell: try to remove [Invalid]; we probably shouldn't have two
     mechanisms for propagating this. *)
  | Invalid of Flambda_kind.t
      (** Used when rebuilding a primitive that turns out to be invalid. This is
          easier to use than turning a whole let-binding into Invalid (which
          might end up deleting code on the way up, resulting in a typing env
          out-of-sync with the generated code). *)
  | Optimised_out of Flambda_kind.t
      (** Used for phantom bindings for which there is not enough information
          remaining to build a meaningful value. Can only be used in a phantom
          let-binding. *)
  | Probe_is_enabled of { name : string }
      (** Returns a boolean saying whether the given tracing probe is enabled. *)
  | Enter_inlined_apply of { dbg : Inlined_debuginfo.t }
      (** Used in classic mode to denote the start of an inlined function body.
          This is then used in to_cmm to correctly add inlined debuginfo. *)
  | Dls_get  (** Obtain the domain-local state block. *)
  | Poll
      (** Poll for runtime actions. May run pending actions such as signal
          handlers, finalizers, memprof callbacks, etc, as well as GCs and
          GC slices, so should not be moved or optimised away. *)
  | Cpu_relax
      (** Arch-specific pause. If poll insertion is disabled, also acts
          as a polling point. *)

(** Untagged binary integer arithmetic operations.

    [Swap_byte_endianness] on a [Tagged_immediate] treats the immediate as
    encoding a 16-bit quantity (described in the least significant 16 bits of
    the immediate after untagging) and exchanges the two halves of the 16-bit
    quantity. The higher-order bits are zeroed. *)
type unary_int_arith_op = Swap_byte_endianness

(** Naked float unary arithmetic operations. *)
type unary_float_arith_op =
  | Abs
  | Neg

(** Reinterpretation operations for 64-bit words. *)
module Reinterpret_64_bit_word : sig
  type t =
    | Tagged_int63_as_unboxed_int64
    | Unboxed_int64_as_tagged_int63
    | Unboxed_int64_as_unboxed_float64
    | Unboxed_float64_as_unboxed_int64
end

(** Primitives taking exactly one argument. *)
type unary_primitive =
  | Block_load of
      { kind : Block_access_kind.t;
        mut : Mutability.t;
        field : Targetint_31_63.t
      }
  | Duplicate_block of { kind : Duplicate_block_kind.t }
      (** [Duplicate_block] may not be used to change the tag or the mutability
          of a block. *)
  | Duplicate_array of
      { kind : Duplicate_array_kind.t;
        source_mutability : Mutability.t;
        destination_mutability : Mutability.t
      }
  | Is_int of { variant_only : bool }
  | Is_null
  | Get_tag
  | Array_length of Array_kind_for_length.t
      (** The unarized length of an array.  So for an example an array of
          kind [Unboxed_product [tagged_immediate; tagged_immediate]] always
          has a length that is a multiple of two. *)
  | Bigarray_length of { dimension : int }
      (** This primitive is restricted by type-checking to bigarrays that have
          at least the correct number of dimensions. More specifically, they
          come from `%caml_ba_dim_x` primitives (for x=1,2,3), and only exposed
          in the Bigarray.ArrayX modules (incidentally, `dimension` should then
          be one of 1,2,3). *)
  (* CR mshinwell/xclerc: Invariant check: dimension >= 0 *)
  (* CR gbury: Invariant check: 0 < dimension <= 3 *)
  | String_length of string_or_bytes
  | Int_as_pointer of Alloc_mode.For_allocations.t
  | Opaque_identity of
      { middle_end_only : bool;
        kind : Flambda_kind.t
      }
  | Int_arith of Flambda_kind.Standard_int.t * unary_int_arith_op
  | Float_arith of float_bitwidth * unary_float_arith_op
  | Num_conv of
      { src : Flambda_kind.Standard_int_or_float.t;
        dst : Flambda_kind.Standard_int_or_float.t
      }
  (* CR gbury: check that chains of conversions are not simplified away by
     flambda, particularly those involving an int32. Indeed, a chain of
     conversions int64 -> int32 -> float that would be "simplified" into a int64
     -> float converison would be wrong given that the int64 -> int32 conversion
     needs to introduce a sign-extension, and simplifying it away would remove
     that sign_extension, leading to incorrect overflow behavior *)
  (* CR gbury: add test for this as soon as we can write tests in flambda *)
  | Boolean_not
  (* CR-someday mshinwell: We should maybe change int32.ml and friends to use a
     %-primitive instead of directly calling C stubs for conversions *)
  | Reinterpret_64_bit_word of Reinterpret_64_bit_word.t
  | Unbox_number of Flambda_kind.Boxable_number.t
  | Box_number of Flambda_kind.Boxable_number.t * Alloc_mode.For_allocations.t
  | Untag_immediate
  | Tag_immediate
  | Project_function_slot of
      { move_from : Function_slot.t;
        move_to : Function_slot.t
      }
      (** Project a function slot from a set of closures, which is actually
          achieved by providing a known function slot [move_from] and the
          desired function slot [move_to], which must be within the same set of
          closures. *)
  | Project_value_slot of
      { project_from : Function_slot.t;
        value_slot : Value_slot.t
      }
      (** Project a value slot from a set of closures -- in other words, read an
          entry from the closure environment (the captured variables). *)
  | Is_boxed_float
      (** Only valid when the float array optimisation is enabled. *)
  | Is_flat_float_array
      (** Only valid when the float array optimisation is enabled. *)
  | End_region of { ghost : bool }
      (** Ending delimiter of local allocation region, accepting a region name. *)
  | End_try_region of { ghost : bool }
      (** Corresponding delimiter for [Begin_try_region]. *)
  | Obj_dup  (** Corresponds to [Obj.dup]; see the documentation in obj.mli. *)
  | Get_header
      (** Get the header of a block. This primitive is invalid if provided with
          an immediate value. It should also not be used to read tags above
          [No_scan_tag].
          Note: The GC color bits in the header are not reliable except for
          checking if the value is locally allocated
          Invariant: never read the tag of a possibly-lazy value from
          ocamlopt-generated code. Tag reads that are allowed to be lazy tags
          (by the type system) should always go through caml_obj_tag, which is
          opaque to the compiler. *)
  | Peek of Flambda_kind.Standard_int_or_float.t
  | Make_lazy of Lazy_block_tag.t

(** Whether a comparison is to yield a boolean result, as given by a particular
    comparison operator, or whether it is to behave in the manner of "compare"
    functions that yield tagged immediates -1, 0 or 1. *)
type 'signed_or_unsigned comparison_behaviour =
  | Yielding_bool of 'signed_or_unsigned comparison
  | Yielding_int_like_compare_functions of 'signed_or_unsigned

(** Binary arithmetic operations on integers. *)
type binary_int_arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor

(** Shift operations on integers. *)
type int_shift_op =
  | Lsl
  | Lsr
  | Asr

(** Naked float binary arithmetic operations. *)
type binary_float_arith_op =
  | Add
  | Sub
  | Mul
  | Div

(** Primitives taking exactly two arguments. *)
type binary_primitive =
  | Block_set of
      { kind : Block_access_kind.t;
        init : Init_or_assign.t;
        field : Targetint_31_63.t
      }
  | Array_load of Array_kind.t * Array_load_kind.t * Mutability.t
      (** Unarized or SIMD array load.

          The array kind preserves unboxed product information but the load
          kind and index all correspond to the unarized representation.
          See also [Array_length], above. *)
  | String_or_bigstring_load of string_like_value * string_accessor_width
  | Bigarray_load of num_dimensions * Bigarray_kind.t * Bigarray_layout.t
  | Phys_equal of equality_comparison
      (** [Phys_equal] is only for things of kind [Value]. *)
  | Int_arith of Flambda_kind.Standard_int.t * binary_int_arith_op
  | Int_shift of Flambda_kind.Standard_int.t * int_shift_op
  | Int_comp of
      Flambda_kind.Standard_int.t * signed_or_unsigned comparison_behaviour
  | Float_arith of float_bitwidth * binary_float_arith_op
  | Float_comp of float_bitwidth * unit comparison_behaviour
  | Bigarray_get_alignment of int
  | Atomic_load_field of Block_access_field_kind.t
  (* CR mshinwell: consider putting atomicity onto [Peek] and [Poke] then
     deleting [Atomic_load_field] *)
  | Poke of Flambda_kind.Standard_int_or_float.t

(** Atomic arithmetic operations on integers. *)
type int_atomic_op =
  | Fetch_add
  | Add
  | Sub
  | And
  | Or
  | Xor

(** Primitives taking exactly three arguments. *)
type ternary_primitive =
  | Array_set of Array_kind.t * Array_set_kind.t
      (** Unarized array update, for mutable arrays.  See [Array_load] above
          for more details on the unarization. *)
  | Bytes_or_bigstring_set of bytes_like_value * string_accessor_width
  | Bigarray_set of num_dimensions * Bigarray_kind.t * Bigarray_layout.t
  | Atomic_field_int_arith of int_atomic_op
  | Atomic_set_field of Block_access_field_kind.t
  | Atomic_exchange_field of Block_access_field_kind.t

(** Primitives taking exactly four arguments. *)
type quaternary_primitive =
  | Atomic_compare_and_set_field of Block_access_field_kind.t
  | Atomic_compare_exchange_field of
      { atomic_kind : Block_access_field_kind.t;
            (** The kind of values which the atomic can hold. *)
        args_kind : Block_access_field_kind.t
            (** The kind of values which the compare-exchange operation is to
                be used with on this particular occasion.  Note that this might
                be [Immediate] even though the atomic is marked as [Any_value],
                for example. *)
      }

(** Primitives taking zero or more arguments. *)
type variadic_primitive =
  | Begin_region of { ghost : bool }
      (** Starting delimiter of local allocation region, returning a region
          name. For regions for the "try" part of a "try...with", use
          [Begin_try_region] (below) instead. *)
  | Begin_try_region of { ghost : bool }
      (** Starting delimiter of local allocation region, when used for a "try"
          body. *)
  | Make_block of Block_kind.t * Mutability.t * Alloc_mode.For_allocations.t
  | Make_array of Array_kind.t * Mutability.t * Alloc_mode.For_allocations.t
(* CR mshinwell: Invariant checks -- e.g. that the number of arguments matches
   [num_dimensions] *)

(** The application of a primitive to its arguments. *)
type t =
  | Nullary of nullary_primitive
  | Unary of unary_primitive * Simple.t
  | Binary of binary_primitive * Simple.t * Simple.t
  | Ternary of ternary_primitive * Simple.t * Simple.t * Simple.t
  | Quaternary of
      quaternary_primitive * Simple.t * Simple.t * Simple.t * Simple.t
  | Variadic of variadic_primitive * Simple.t list

type primitive_application = t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val args : t -> Simple.t list

val map_args : (Simple.t -> Simple.t) -> t -> t

(** Simpler version (e.g. for [Inlining_cost]), where only the actual primitive
    matters, not the arguments. *)
module Without_args : sig
  type t =
    | Nullary of nullary_primitive
    | Unary of unary_primitive
    | Binary of binary_primitive
    | Ternary of ternary_primitive
    | Quaternary of quaternary_primitive
    | Variadic of variadic_primitive

  val print : Format.formatter -> t -> unit

  (** Describe the effects and coeffects that the application of the given
      primitive may have. *)
  val effects_and_coeffects : t -> Effects_and_coeffects.t
end

(** A description of the kind of values which a unary primitive expects as its
    arguments. *)
val arg_kind_of_unary_primitive : unary_primitive -> Flambda_kind.t

val args_kind_of_binary_primitive :
  binary_primitive -> Flambda_kind.t * Flambda_kind.t

val args_kind_of_ternary_primitive :
  ternary_primitive -> Flambda_kind.t * Flambda_kind.t * Flambda_kind.t

val args_kind_of_quaternary_primitive :
  quaternary_primitive ->
  Flambda_kind.t * Flambda_kind.t * Flambda_kind.t * Flambda_kind.t

type arg_kinds =
  | Variadic_mixed of Flambda_kind.Mixed_block_shape.t
  | Variadic_all_of_kind of Flambda_kind.t
  | Variadic_zero_or_one of Flambda_kind.t
  | Variadic_unboxed_product of Flambda_kind.t list

val args_kind_of_variadic_primitive : variadic_primitive -> arg_kinds

(** A description of the kinds of values (or in the case of [Unit], the actual
    value) which a primitive returns. *)
type result_kind =
  | Singleton of Flambda_kind.t
      (** The primitive returns a single value of the given kind. *)
  | Unit  (** The primitive returns the constant unit value. *)

val result_kind_of_nullary_primitive : nullary_primitive -> result_kind

val result_kind_of_unary_primitive : unary_primitive -> result_kind

val result_kind_of_binary_primitive : binary_primitive -> result_kind

val result_kind_of_ternary_primitive : ternary_primitive -> result_kind

val result_kind_of_quaternary_primitive : quaternary_primitive -> result_kind

val result_kind_of_variadic_primitive : variadic_primitive -> result_kind

(** Describe the kind of the result of the given primitive. *)
val result_kind : t -> result_kind

(** Like the [result_kind]s, but returns the appropriate [Flambda_kind]. *)
val result_kind_of_nullary_primitive' : nullary_primitive -> Flambda_kind.t

val result_kind_of_unary_primitive' : unary_primitive -> Flambda_kind.t

val result_kind_of_binary_primitive' : binary_primitive -> Flambda_kind.t

val result_kind_of_ternary_primitive' : ternary_primitive -> Flambda_kind.t

val result_kind_of_quaternary_primitive' :
  quaternary_primitive -> Flambda_kind.t

val result_kind_of_variadic_primitive' : variadic_primitive -> Flambda_kind.t

val result_kind' : t -> Flambda_kind.t

(** Describe the effects and coeffects that the application of the given
    primitive may have. *)
val effects_and_coeffects : t -> Effects_and_coeffects.t

(** Returns [true] iff the given primitive has neither effects nor coeffects. *)
val no_effects_or_coeffects : t -> bool

val at_most_generative_effects : t -> bool

(** Returns [true] iff the given primitive has generative effects, and no other
    effects. *)
val only_generative_effects : t -> bool

module Eligible_for_cse : sig
  (** Primitive applications that may be replaced by a variable which is let
      bound to a single instance of such application. Primitives that are
      genuine projections (e.g. [Block_load], etc.) are not eligible, since the
      associated information is propagated through types, not CSE. *)
  type t

  include Contains_names.S with type t := t

  val create : primitive_application -> t option

  val create_exn : primitive_application -> t

  val create_is_int : variant_only:bool -> immediate_or_block:Name.t -> t

  val create_get_tag : block:Name.t -> t

  val eligible : primitive_application -> bool

  val to_primitive : t -> primitive_application

  val fold_args : t -> init:'a -> f:('a -> Simple.t -> 'a * Simple.t) -> 'a * t

  val filter_map_args : t -> f:(Simple.t -> Simple.t option) -> t option

  include Container_types.S with type t := t
end

include Container_types.S with type t := t

val equal : t -> t -> bool

val equal_nullary_primitive : nullary_primitive -> nullary_primitive -> bool

val equal_unary_primitive : unary_primitive -> unary_primitive -> bool

val equal_binary_primitive : binary_primitive -> binary_primitive -> bool

val equal_ternary_primitive : ternary_primitive -> ternary_primitive -> bool

val equal_quaternary_primitive :
  quaternary_primitive -> quaternary_primitive -> bool

val equal_variadic_primitive : variadic_primitive -> variadic_primitive -> bool

val compare_primitive_application :
  compare_simple:(Simple.t -> Simple.t -> int) -> t -> t -> int

val is_begin_or_end_region : t -> bool

val is_begin_region : t -> bool

val is_end_region : t -> Variable.t option
