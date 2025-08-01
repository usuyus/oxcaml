(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

let use_as_value : ('a : value) -> 'a = fun x -> x
let use_uncontended : 'a @ uncontended -> 'a = fun x -> x

(* Baseline: if the jkind doesn't match, we should get an error. *)
type t : value mod contended = { mutable contents : string }
[%%expect{|
val use_as_value : 'a -> 'a = <fun>
val use_uncontended : 'a -> 'a = <fun>
Line 5, characters 0-60:
5 | type t : value mod contended = { mutable contents : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod contended
         because of the annotation on the declaration of the type t.
|}]

(* On the other hand, if we set the attribute, we shouldn't get an error. *)
type t : value mod contended = { mutable contents : string }
[@@unsafe_allow_any_mode_crossing]
let f (x : t @ contended) = use_uncontended x
[%%expect{|
type t : value mod contended = { mutable contents : string; }
[@@unsafe_allow_any_mode_crossing]
val f : t @ contended -> t = <fun>
|}]

(* If we set the attribute but *don't* get a kind mismatch, we ought to be fine *)
type t : value mod many portable uncontended = string
[@@unsafe_allow_any_mode_crossing]
[%%expect{|
Lines 1-2, characters 0-34:
1 | type t : value mod many portable uncontended = string
2 | [@@unsafe_allow_any_mode_crossing]
Error: [@@unsafe_allow_any_mode_crossing] is not allowed on this kind of type declaration.
       Only records, unboxed products, and variants are supported.
|}]

(* The attribute shouldn't allow us to change the layout *)
type t : float64 mod contended = { mutable contents : string }
[@@unsafe_allow_any_mode_crossing]
[%%expect{|
Lines 1-2, characters 0-34:
1 | type t : float64 mod contended = { mutable contents : string }
2 | [@@unsafe_allow_any_mode_crossing]
Error: The layout of type "t" is value
         because it's a boxed record type.
       But the layout of type "t" must be a sublayout of float64
         because of the annotation on the declaration of the type t.
|}]

(* Annotations with with-bounds are allowed *)
type 'a t : value mod contended with 'a = { mutable contents : 'a }
[@@unsafe_allow_any_mode_crossing]
[%%expect{|
type 'a t : value mod contended with 'a = { mutable contents : 'a; }
[@@unsafe_allow_any_mode_crossing]
|}]

(* Abstract types in signatures should work with the unsafe kind *)
module M : sig
  type t : value mod contended
end = struct
  type t : value mod contended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f (x : t @ contended) = use_uncontended x
end
[%%expect{|
module M : sig type t : value mod contended end
|}]

(* Setting the attribute on an open or abstract type is not allowed *)
module type S = sig
  type abstract [@@unsafe_allow_any_mode_crossing]
end
[%%expect{|
Line 2, characters 2-50:
2 |   type abstract [@@unsafe_allow_any_mode_crossing]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@@unsafe_allow_any_mode_crossing] is not allowed on this kind of type declaration.
       Only records, unboxed products, and variants are supported.
|}]

type open_ = .. [@@unsafe_allow_any_mode_crossing]
[%%expect{|
Line 1, characters 0-50:
1 | type open_ = .. [@@unsafe_allow_any_mode_crossing]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@@unsafe_allow_any_mode_crossing] is not allowed on this kind of type declaration.
       Only records, unboxed products, and variants are supported.
|}]


module M1 : sig
  type t : value mod contended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t : value mod contended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f (x : t @ contended) = use_uncontended x
end
module M2 : sig
  type t : value mod contended = M1.t = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t : value mod contended = M1.t = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f (x : t @ contended) = use_uncontended x
end
[%%expect{|
module M1 :
  sig
    type t : value mod contended = { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M2 :
  sig
    type t : value mod contended = M1.t = { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
  end
|}]

(* Private types still require the attribute *)
module Private : sig
  type t : value mod contended = private { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t  : value mod contended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f (x : t @ contended) = use_uncontended x
end
[%%expect{|
module Private :
  sig
    type t : value mod contended = private { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
  end
|}]

(* Non-abstract types in signatures should work as long as they specify the attribute *)
module M : sig
  type t1 : value mod contended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  type t2 : value mod contended = private { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  type t3 : value mod contended =
    | Immut of string
    | Mut of { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t1 : value mod contended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  type t2 : value mod contended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  type t3 : value mod contended =
    | Immut of string
    | Mut of { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f1 (x : t1 @ contended) = use_uncontended x
  let f2 (x : t2 @ contended) = use_uncontended x
  let f3 (x : t3 @ contended) = use_uncontended x
end
[%%expect{|
module M :
  sig
    type t1 : value mod contended = { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
    type t2 : value mod contended = private { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
    type t3
      : value mod contended =
        Immut of string
      | Mut of { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
  end
|}]

(* [@@unsafe_allow_any_mode_crossing] should not allow you to weaken the modal bounds on a
   kind in module inclusion *)
module M : sig
  type t : value mod contended = { mutable x : int } [@@unsafe_allow_any_mode_crossing]
end = struct
  type t = { mutable x : int }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { mutable x : int }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int; } end
       is not included in
         sig
           type t : value mod contended = { mutable x : int; }
           [@@unsafe_allow_any_mode_crossing]
         end
       Type declarations do not match:
         type t = { mutable x : int; }
       is not included in
         type t : value mod contended = { mutable x : int; }
       [@@unsafe_allow_any_mode_crossing]
       They have different unsafe mode crossing behavior:
       the second has [@@unsafe_allow_any_mode_crossing], but the first does not
|}]


module type S = sig
  type t : value mod contended = { mutable x : int } [@@unsafe_allow_any_mode_crossing]
end

module M = struct
    type t = { mutable x : int }
end

module _ = (M : S)
[%%expect{|
module type S =
  sig
    type t : value mod contended = { mutable x : int; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M : sig type t = { mutable x : int; } end
Line 9, characters 12-13:
9 | module _ = (M : S)
                ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t = { mutable x : int; } end
       is not included in
         S
       Type declarations do not match:
         type t = M.t = { mutable x : int; }
       is not included in
         type t : value mod contended = { mutable x : int; }
       [@@unsafe_allow_any_mode_crossing]
       They have different unsafe mode crossing behavior:
       the second has [@@unsafe_allow_any_mode_crossing], but the first does not
|}]

module type S2 = S with type t = M.t
[%%expect{|
Line 1, characters 24-36:
1 | module type S2 = S with type t = M.t
                            ^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "M.t"
       They have different unsafe mode crossing behavior:
       this has [@@unsafe_allow_any_mode_crossing], but the original does not
|}]

(** The mod-bounds must be equal if the attribute is specified in both the sig and the
    struct *)
module M : sig
  type t : value mod contended = { mutable x : int }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t : value mod portable contended = { mutable x : int }
  [@@unsafe_allow_any_mode_crossing]
end
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t : value mod portable contended = { mutable x : int }
6 |   [@@unsafe_allow_any_mode_crossing]
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t : value mod contended portable = { mutable x : int; }
           [@@unsafe_allow_any_mode_crossing]
         end
       is not included in
         sig
           type t : value mod contended = { mutable x : int; }
           [@@unsafe_allow_any_mode_crossing]
         end
       Type declarations do not match:
         type t : value mod contended portable = { mutable x : int; }
       [@@unsafe_allow_any_mode_crossing]
       is not included in
         type t : value mod contended = { mutable x : int; }
       [@@unsafe_allow_any_mode_crossing]
       They have different unsafe mode crossing behavior:
       Both specify [@@unsafe_allow_any_mode_crossing], but their bounds are not equal
         the first has: mod portable contended
         but the second has: mod contended
|}]

module A : sig
  type t : value mod external_ global portable many uncontended unique unyielding
end = struct
  type t = int
end

module B = struct
  type t : value mod portable contended = { a : A.t }
  [@@unsafe_allow_any_mode_crossing]

  let a t = t.a
end
[%%expect{|
module A : sig type t : value mod global many portable external_ end
module B :
  sig
    type t : value mod contended portable = { a : A.t; }
    [@@unsafe_allow_any_mode_crossing]
    val a : t -> A.t
  end
|}]

(* Adding with-bounds using unsafe-allow-any *)

type ('a, 'k) imm : immutable_data with 'a = { inner : 'a }
type 'a t : immutable_data with 'a = P : ('a, 'k) imm -> 'a t
[@@unsafe_allow_any_mode_crossing]
[%%expect{|
type ('a, 'k) imm = { inner : 'a; }
type 'a t : immutable_data with 'a = P : ('a, 'k) imm -> 'a t
[@@unsafe_allow_any_mode_crossing]
|}]

let f (x : int t @ contended) = use_uncontended x
[%%expect{|
val f : int t @ contended -> int t = <fun>
|}]

let bad (x : int ref t @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 54-55:
1 | let bad (x : int ref t @ contended) = use_uncontended x
                                                          ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(* Reexporting after adding with-bounds *)
module B = struct
  type 'a t_reexported : immutable_data with 'a = 'a t = P : ('a, 'k) imm -> 'a t_reexported
  [@@unsafe_allow_any_mode_crossing]
end
[%%expect{|
module B :
  sig
    type 'a t_reexported
      : immutable_data with 'a =
      'a t =
        P : ('a, 'k) imm -> 'a t_reexported
    [@@unsafe_allow_any_mode_crossing]
  end
|}]

let f (x : int B.t_reexported @ contended) = use_uncontended x
[%%expect{|
val f : int B.t_reexported @ contended -> int B.t_reexported = <fun>
|}]

let bad (x : int ref B.t_reexported @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 67-68:
1 | let bad (x : int ref B.t_reexported @ contended) = use_uncontended x
                                                                       ^
Error: This value is "contended" but expected to be "uncontended".
|}]

type 'a bad_reexport : immutable_data = 'a t = P : ('a, 'k) imm -> 'a bad_reexport
[@@unsafe_allow_any_mode_crossing]
[%%expect{|
Lines 1-2, characters 0-34:
1 | type 'a bad_reexport : immutable_data = 'a t = P : ('a, 'k) imm -> 'a bad_reexport
2 | [@@unsafe_allow_any_mode_crossing]
Error: This variant or record definition does not match that of type "'a t"
       They have different unsafe mode crossing behavior:
       Both specify [@@unsafe_allow_any_mode_crossing], but their bounds are not equal
         the original has: mod unyielding many stateless portable immutable
         contended with 'a
         but this has: mod unyielding many stateless portable immutable
         contended
|}]

type ('a, 'b) arity_2 : immutable_data with 'b = { x : 'a }
[@@unsafe_allow_any_mode_crossing]

type ('a, 'b) bad_reexport_2 : immutable_data with 'a = ('a, 'b) arity_2 = { x : 'a }
[@@unsafe_allow_any_mode_crossing]
[%%expect{|
type ('a, 'b) arity_2 : immutable_data with 'b = { x : 'a; }
[@@unsafe_allow_any_mode_crossing]
Lines 4-5, characters 0-34:
4 | type ('a, 'b) bad_reexport_2 : immutable_data with 'a = ('a, 'b) arity_2 = { x : 'a }
5 | [@@unsafe_allow_any_mode_crossing]
Error: This variant or record definition does not match that of type
         "('a, 'b) arity_2"
       They have different unsafe mode crossing behavior:
       Both specify [@@unsafe_allow_any_mode_crossing], but their bounds are not equal
         the original has: mod unyielding many stateless portable immutable
         contended with 'b
         but this has: mod unyielding many stateless portable immutable
         contended with 'a
|}]

(* mcomp *)

type (_, _) eq = Refl : ('a, 'a) eq

module M1 = struct
  type 'a t : value mod contended = { x : 'a }
  [@@unsafe_allow_any_mode_crossing]
end

module M2 = struct
  type 'a t : value mod contended = { x : 'a }
  [@@unsafe_allow_any_mode_crossing]
end

module M3 = struct
  type 'a t : value mod portable = { x : 'a }
  [@@unsafe_allow_any_mode_crossing]
end

module M4 = struct
  type 'a t : value mod contended with 'a = { mutable x : 'a }
  [@@unsafe_allow_any_mode_crossing]
end

module M5 = struct
  type 'a t : value mod contended with 'a = { mutable x : 'a }
  [@@unsafe_allow_any_mode_crossing]
end

module M6 = struct
  type 'a t : immutable_data with 'a = { mutable x : 'a }
  [@@unsafe_allow_any_mode_crossing]
end

module M7 = struct
  type ('a, 'b) t : value mod contended with 'a = { mutable x : 'b }
  [@@unsafe_allow_any_mode_crossing]
end

module M8 = struct
  type ('a, 'b) t : value mod contended with 'a = { mutable x : 'b }
  [@@unsafe_allow_any_mode_crossing]
end

module M9 = struct
  type ('a, 'b) t : value mod contended with 'b = { mutable x : 'b }
  [@@unsafe_allow_any_mode_crossing]
end

[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module M1 :
  sig
    type 'a t : value mod contended = { x : 'a; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M2 :
  sig
    type 'a t : value mod contended = { x : 'a; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M3 :
  sig
    type 'a t : value mod portable = { x : 'a; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M4 :
  sig
    type 'a t : value mod contended with 'a = { mutable x : 'a; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M5 :
  sig
    type 'a t : value mod contended with 'a = { mutable x : 'a; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M6 :
  sig
    type 'a t : immutable_data with 'a = { mutable x : 'a; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M7 :
  sig
    type ('a, 'b) t : value mod contended with 'a = { mutable x : 'b; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M8 :
  sig
    type ('a, 'b) t : value mod contended with 'a = { mutable x : 'b; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M9 :
  sig
    type ('a, 'b) t : value mod contended with 'b = { mutable x : 'b; }
    [@@unsafe_allow_any_mode_crossing]
  end
|}]


let f (type a) (eq : (a M1.t, a M2.t) eq) = match eq with Refl -> ()
[%%expect{|
val f : ('a M1.t, 'a M2.t) eq -> unit = <fun>
|}]

let f (eq : ('a M1.t, 'a M3.t) eq) = match eq with _ -> .
[%%expect{|
val f : ('a M1.t, 'a M3.t) eq -> 'b = <fun>
|}]

let f (type a) (eq : (a M4.t, a M5.t) eq) = match eq with Refl -> ()
[%%expect{|
val f : ('a M4.t, 'a M5.t) eq -> unit = <fun>
|}]

let f (type a) (eq : (a M4.t, a M6.t) eq) = match eq with _ -> .
[%%expect{|
val f : ('a M4.t, 'a M6.t) eq -> 'b = <fun>
|}]

let f (type a b) (eq : ((a, b) M7.t, (a, b) M8.t) eq) = match eq with Refl -> ()
[%%expect{|
val f : (('a, 'b) M7.t, ('a, 'b) M8.t) eq -> unit = <fun>
|}]

let f (type a b) (eq : ((a, b) M7.t, (a, b) M9.t) eq) = match eq with Refl -> ()
[%%expect{|
val f : (('a, 'b) M7.t, ('a, 'b) M9.t) eq -> unit = <fun>
|}]
