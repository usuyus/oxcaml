(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Jane Street                               *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42-37"]

module CL = Cfg_with_layout
module DLL = Oxcaml_utils.Doubly_linked_list
module String = Misc.Stdlib.String

module List = struct
  include List
  include Misc.Stdlib.List
end

type error = Asm_generation of (string * int)

exception Error of error

(* Convenient ways to print - all nicely packed! *)
module F = struct
  include Format

  let pp_indent ppf () = fprintf ppf "  "

  let pp_comma ppf () = fprintf ppf ", "

  let pp_space ppf () = fprintf ppf " "

  let _pp_comment ppf s = fprintf ppf "; %s" s

  let pp_line ?(num_indents = 0) ?comment ppf =
    (pp_print_list pp_indent) ppf (List.init num_indents (fun _ -> ()));
    kfprintf
      (fun ppf ->
        match comment with
        | None -> fprintf ppf "\n"
        | Some comment -> fprintf ppf " %s\n" comment)
      ppf

  let pp_ins ppf = pp_line ~num_indents:1 ppf

  let pp_str_if str ppf b = if b then fprintf ppf "%s" str

  (* Comment insertion *)

  let do_if_comments_enabled f = if !Oxcaml_flags.dasm_comments then f ()

  let pp_dbginfo ppf dbg =
    if Debuginfo.is_none dbg
    then ()
    else fprintf ppf "[ %a ]" Debuginfo.print_compact dbg

  let pp_dbg_comment ~newline ppf name dbg =
    do_if_comments_enabled (fun () ->
        match newline with
        | true -> pp_line ppf "; %s %a" name pp_dbginfo dbg
        | false -> fprintf ppf "; %s %a" name pp_dbginfo dbg)

  let pp_dbg_instr_aux pp_instr ppf ins =
    do_if_comments_enabled (fun () ->
        (* Replace newlines since this is a line comment *)
        let instr_str =
          asprintf "%a" pp_instr ins
          |> String.map (function '\n' -> ' ' | c -> c)
        in
        fprintf ppf "; %s %a" instr_str pp_dbginfo ins.Cfg.dbg)

  let pp_dbg_instr_basic = pp_dbg_instr_aux Cfg.print_basic

  let pp_dbg_instr_terminator = pp_dbg_instr_aux Cfg.print_terminator
end

(* Convenient ways to fail - all nicely spilled! *)

let fail name = Misc.fatal_errorf "Llvmize.%s" name

let fail_msg ?name fmt =
  let name = match name with Some name -> "." ^ name | None -> "" in
  Format.kasprintf (fun msg -> Misc.fatal_errorf "Llvmize%s: %s" name msg) fmt

let fail_if_not ?msg name cond =
  if not cond
  then match msg with None -> fail name | Some msg -> fail_msg ~name "%s" msg

let not_implemented_aux print_ins ?msg i =
  fail_msg "Llvmize: unimplemented instruction: %a %a" print_ins i
    (Format.pp_print_option
       ~none:(fun ppf () -> Format.fprintf ppf "(no msg)")
       (fun ppf msg -> Format.fprintf ppf "(%s)" msg))
    msg

let not_implemented_basic = not_implemented_aux Cfg.print_basic

let not_implemented_terminator = not_implemented_aux Cfg.print_terminator

module Llvm_ir = struct
  module Type = struct
    type t =
      (* Integral types *)
      | Int of { width_in_bits : int }
      | Ptr of { addrspace : string option }
      (* Floating point types *)
      | Float (* 32-bit *)
      | Double (* 64 bit *)
      (* Aggregate types *)
      | Struct of t list
      | Array of
          { size : int;
            elem_type : t
          }
      | Vector of
          { size : int;
            elem_type : t
          }
      (* Non-first-class types *)
      | Label
      | Token
      | Metadata

    let i128 = Int { width_in_bits = 128 }

    let i64 = Int { width_in_bits = 64 }

    let i32 = Int { width_in_bits = 32 }

    let i16 = Int { width_in_bits = 16 }

    let i8 = Int { width_in_bits = 8 }

    let i1 = Int { width_in_bits = 1 }

    let float = Float

    let double = Double

    let ptr = Ptr { addrspace = None }

    let val_ptr = Ptr { addrspace = Some "1" }

    let doublex2 = Vector { size = 2; elem_type = double }

    let label = Label

    let metadata = Metadata

    let _token = Token

    let of_machtype_component (c : Cmm.machtype_component) =
      match c with
      | Int -> i64
      | Val -> val_ptr
      | Addr ->
        val_ptr
        (* We interpret [Addr]s as [val_ptr]s to let the RS4GC pass in LLVM to
           handle derived pointers for us. *)
      | Float -> double
      | Float32 -> float
      | Vec128 | Vec256 | Vec512 | Valx2 ->
        fail_msg ~name:"Type.of_machtype_component" "not_implemented"

    let of_reg (reg : Reg.t) = of_machtype_component reg.typ

    let of_float_width (width : Cmm.float_width) =
      match width with Float64 -> Double | Float32 -> Float

    let rec pp_t ppf t =
      let open F in
      match t with
      | Int { width_in_bits } -> fprintf ppf "i%d" width_in_bits
      | Float -> fprintf ppf "float"
      | Double -> fprintf ppf "double"
      | Ptr { addrspace } -> (
        fprintf ppf "ptr";
        match addrspace with
        | Some addrspace -> fprintf ppf " addrspace(%s)" addrspace
        | None -> ())
      | Struct typs ->
        fprintf ppf "{ %a }"
          (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_t)
          typs
      | Array { size; elem_type } ->
        fprintf ppf "[ %d x %a ]" size pp_t elem_type
      | Vector { size; elem_type } ->
        fprintf ppf "< %d x %a >" size pp_t elem_type
      | Label -> fprintf ppf "label"
      | Token -> fprintf ppf "token"
      | Metadata -> fprintf ppf "metadata"

    let to_string t = Format.asprintf "%a" pp_t t

    let rec equal t1 t2 =
      match t1, t2 with
      | Int { width_in_bits = x }, Int { width_in_bits = y } -> x = y
      | Ptr { addrspace = x }, Ptr { addrspace = y } ->
        Option.equal String.equal x y
      | Float, Float | Double, Double -> true
      | Struct xs, Struct ys -> List.equal equal xs ys
      | ( Array { size = size1; elem_type = typ1 },
          Array { size = size2; elem_type = typ2 } ) ->
        size1 = size2 && equal typ1 typ2
      | ( Vector { size = size1; elem_type = typ1 },
          Vector { size = size2; elem_type = typ2 } ) ->
        size1 = size2 && equal typ1 typ2
      | Label, Label | Token, Token | Metadata, Metadata -> true
      | ( ( Int _ | Float | Double | Ptr _ | Struct _ | Array _ | Vector _
          | Label | Token | Metadata ),
          _ ) ->
        false

    let get_struct_elements = function
      | Struct elems -> Some elems
      | Int _ | Ptr _ | Float | Double | Array _ | Vector _ | Label | Token
      | Metadata ->
        None

    let rec extract_struct t indices =
      match indices with
      | [] -> Some t
      | i :: rest -> (
        match get_struct_elements t with
        | Some elems ->
          Option.bind (List.nth_opt elems i) (fun elem ->
              extract_struct elem rest)
        | None -> None)

    let elem_type = function
      | Array { elem_type; _ } | Vector { elem_type; _ } -> Some elem_type
      | Int _ | Ptr _ | Float | Double | Struct _ | Label | Token | Metadata ->
        None

    let is_ptr = function
      | Ptr _ -> true
      | Int _ | Float | Double | Struct _ | Array _ | Vector _ | Label | Token
      | Metadata ->
        false

    let is_int = function
      | Int _ -> true
      | Ptr _ | Float | Double | Struct _ | Array _ | Vector _ | Label | Token
      | Metadata ->
        false

    let is_floating_point = function
      | Float | Double -> true
      | Ptr _ | Int _ | Struct _ | Array _ | Vector _ | Label | Token | Metadata
        ->
        false

    module Or_void = struct
      type nonrec t = t option

      let void = None

      let pp_t ppf t =
        match t with Some t -> pp_t ppf t | None -> Format.fprintf ppf "void"

      let equal = Option.equal equal
    end
  end

  module Ident = struct
    type t =
      | Local of string
      | Global of string

    let local s = Local s

    let global s = Global s

    let of_label label = local ("L" ^ Label.to_string label)

    let pp_t ppf t =
      let open F in
      match t with
      | Local s -> fprintf ppf "%%%s" s
      | Global s ->
        let encoded = Asm_targets.(Asm_symbol.create s |> Asm_symbol.encode) in
        fprintf ppf "@%s" encoded

    let to_label_string_exn = function
      | Local s -> s ^ ":"
      | Global _ -> fail "Type.to_label_string_exn"

    let to_string_hum t =
      match t with Local s -> "%" ^ s | Global s -> "@" ^ s

    module Gen = struct
      type t = { mutable next : int }

      (* Local identifiers are only valid within function scope, so we can reset
         it to 0 every time *)
      let create () = { next = 0 }

      let get_fresh t =
        let res = t.next in
        t.next <- succ res;
        local (Int.to_string res)
    end
  end

  module Value = struct
    (* CR yusumez: Split [Immediate] to multiple variants like [Poison],
       [Zeroinitializer], [Int], etc. *)

    type value =
      | Ident of Ident.t
      | Immediate of string
      | Struct_constant of t list
      | Blockaddress of
          { func : Ident.t;
            block : Ident.t
          }

    and t = Type.t * value

    let get_type (typ, _) = typ

    let get_ident_exn (_, value) =
      match value with
      | Ident ident -> ident
      | Immediate _ | Blockaddress _ | Struct_constant _ ->
        fail "Value.get_ident_exn"

    let get_value (_, value) = value

    let of_ident ~typ ident = typ, Ident ident

    let of_symbol ?(typ = Type.ptr) symbol = typ, Ident (Ident.global symbol)

    let of_int ?(typ = Type.i64) i = typ, Immediate (Int.to_string i)

    let of_nativeint ?(typ = Type.i64) i =
      typ, Immediate (Nativeint.to_string i)

    let of_float32_bits bits =
      (* Note that "%#x" formats 0 as "0", not "0x0"... *)
      Type.float, Immediate (Format.sprintf "0x%lx" bits)

    let of_float64_bits bits =
      Type.double, Immediate (Format.sprintf "0x%Lx" bits)

    (* CR yusumez: 64-bit floats with at least 17 digits are guaranteed to round
       trip exactly through string conversions by the IEEE 754 standard (9
       digits for 32-bit floats). However, it would still be nice to prefer the
       functions above whenever possible. *)
    let of_float ~typ f = typ, Immediate (Format.sprintf "%.20f" f)

    let of_label label = of_ident ~typ:Type.label (Ident.of_label label)

    let of_string_constant s =
      let open F in
      ( Type.Array { size = String.length s; elem_type = Type.i8 },
        Immediate
          (asprintf "%a"
             (fun ppf () ->
               fprintf ppf "c\"";
               String.iter (fun c -> fprintf ppf "\\%02x" (Char.code c)) s;
               fprintf ppf "\"")
             ()) )

    let poison typ = typ, Immediate "poison"

    let zeroinitializer typ = typ, Immediate "zeroinitializer"

    let imm typ s = typ, Immediate s

    let struct_constant ts =
      Type.Struct (List.map get_type ts), Struct_constant ts

    let blockaddress ~func ~block = Type.ptr, Blockaddress { func; block }

    let rec pp_value ppf value =
      let open F in
      match value with
      | Ident ident -> Ident.pp_t ppf ident
      | Immediate s -> fprintf ppf "%s" s
      | Struct_constant ts ->
        fprintf ppf "{ ";
        let pp_elem ppf t =
          match get_value t with
          | Ident _ | Immediate _ | Blockaddress _ -> pp_t ppf t
          | Struct_constant _ ->
            fail_msg ~name:"Value.pp_value" "struct constants cannot nest"
        in
        (pp_print_list ~pp_sep:pp_comma pp_elem) ppf ts;
        fprintf ppf " }"
      | Blockaddress { func; block } ->
        fprintf ppf "blockaddress(%a, %a)" Ident.pp_t func Ident.pp_t block

    and pp_t ppf (typ, value) =
      Format.fprintf ppf "%a %a" Type.pp_t typ pp_value value
  end

  module Fn_attr = struct
    type t =
      | Cold
      | Gc of string
      | Gc_leaf_function
      | Noinline
      | Returns_twice
      | Statepoint_id of int

    let to_string = function
      | Cold -> "cold"
      | Gc s -> Format.sprintf {|gc "%s"|} s
      | Gc_leaf_function -> {|"gc-leaf-function"="true"|}
      | Noinline -> "noinline"
      | Returns_twice -> "returns_twice"
      | Statepoint_id i -> Format.sprintf {|"statepoint-id"="%d"|} i

    let pp_t ppf t = Format.pp_print_string ppf (to_string t)

    let pp_t_list = Format.pp_print_list ~pp_sep:F.pp_space pp_t

    let to_string t = Format.asprintf "%a" pp_t t

    let compare t1 t2 = String.compare (to_string t1) (to_string t2)
  end

  module Calling_conventions = struct
    type t =
      | Default (* Default C calling convention *)
      | Ocaml (* See backend/<arch>/proc.ml for details *)
      | Ocaml_c_call
    (* Same as [Default] but passes the function address and stack offset (if
       there are stack arguments) through rax and r13. *)

    let to_string = function
      | Default -> ""
      | Ocaml -> "cc 104"
      | Ocaml_c_call -> "cc 105"

    let pp_t ppf t = Format.pp_print_string ppf (to_string t)
  end

  module Instruction = struct
    type unary_op = Fneg

    type binary_op =
      (* Integer *)
      | Add
      | Sub
      | Mul
      | Udiv
      | Sdiv
      | Urem
      | Srem
      (* Float *)
      | Fadd
      | Fsub
      | Fmul
      | Fdiv
      | Frem
      (* Bitwise *)
      | Shl
      | Lshr
      | Ashr
      | And
      | Or
      | Xor

    type atomicrmw_op =
      | Atomicrmw_add
      | Atomicrmw_sub
      | Atomicrmw_and
      | Atomicrmw_or
      | Atomicrmw_xor
      | Atomicrmw_xchg

    type convert_op =
      (* Int *)
      | Sext
      | Zext
      | Trunc
      (* Float *)
      | Fpext
      | Fptrunc
      (* Int <-> Float *)
      | Fptoui
      | Fptosi
      | Uitofp
      | Sitofp
      (* Ptr *)
      | Inttoptr
      | Ptrtoint
      | Ptrtoaddr
      | Addrspacecast
      (* Magic *)
      | Bitcast

    type icmp_cond =
      | Ieq
      | Ine
      | Iugt
      | Iuge
      | Iult
      | Iule
      | Isgt
      | Isge
      | Islt
      | Isle

    type fcmp_cond =
      | Ffalse
      | Foeq
      | Fogt
      | Foge
      | Folt
      | Fole
      | Fone
      | Ford
      | Fueq
      | Fugt
      | Fuge
      | Fult
      | Fule
      | Fune
      | Funo
      | Ftrue

    let binary_op_to_string = function
      | Add -> "add"
      | Sub -> "sub"
      | Mul -> "mul"
      | Udiv -> "udiv"
      | Sdiv -> "sdiv"
      | Urem -> "urem"
      | Srem -> "srem"
      | Fadd -> "fadd"
      | Fsub -> "fsub"
      | Fmul -> "fmul"
      | Fdiv -> "fdiv"
      | Frem -> "frem"
      | Shl -> "shl"
      | Lshr -> "lshr"
      | Ashr -> "ashr"
      | And -> "and"
      | Or -> "or"
      | Xor -> "xor"

    let unary_op_to_string = function Fneg -> "fneg"

    let convert_op_to_string = function
      | Sext -> "sext"
      | Zext -> "zext"
      | Trunc -> "trunc"
      | Fpext -> "fpext"
      | Fptrunc -> "fptrunc"
      | Fptoui -> "fptoui"
      | Fptosi -> "fptosi"
      | Uitofp -> "uitofp"
      | Sitofp -> "sitofp"
      | Inttoptr -> "inttoptr"
      | Ptrtoint -> "ptrtoint"
      | Ptrtoaddr -> "ptrtoaddr"
      | Addrspacecast -> "addrspacecast"
      | Bitcast -> "bitcast"

    let icmp_cond_to_string = function
      | Ieq -> "eq"
      | Ine -> "ne"
      | Iugt -> "ugt"
      | Iuge -> "uge"
      | Iult -> "ult"
      | Iule -> "ule"
      | Isgt -> "sgt"
      | Isge -> "sge"
      | Islt -> "slt"
      | Isle -> "sle"

    let icmp_cond_of_ocaml (cond : Operation.integer_comparison) =
      match cond with
      | Ceq -> Ieq
      | Cne -> Ine
      | Clt -> Islt
      | Cgt -> Isgt
      | Cle -> Isle
      | Cge -> Isge
      | Cult -> Iult
      | Cugt -> Iugt
      | Cule -> Iule
      | Cuge -> Iuge

    let fcmp_cond_to_string = function
      | Ffalse -> "false"
      | Foeq -> "oeq"
      | Fogt -> "ogt"
      | Foge -> "oge"
      | Folt -> "olt"
      | Fole -> "ole"
      | Fone -> "one"
      | Ford -> "ord"
      | Fueq -> "ueq"
      | Fugt -> "ugt"
      | Fuge -> "uge"
      | Fult -> "ult"
      | Fule -> "ule"
      | Fune -> "une"
      | Funo -> "uno"
      | Ftrue -> "true"

    let fcmp_cond_of_ocaml (cond : Operation.float_comparison) =
      match cond with
      | CFeq -> Foeq
      | CFneq -> Fune
      | CFlt -> Folt
      | CFnlt -> Fuge
      | CFgt -> Fogt
      | CFngt -> Fule
      | CFle -> Fole
      | CFnle -> Fugt
      | CFge -> Foge
      | CFnge -> Fult

    let atomicrmw_op_to_string = function
      | Atomicrmw_add -> "add"
      | Atomicrmw_sub -> "sub"
      | Atomicrmw_and -> "and"
      | Atomicrmw_or -> "or"
      | Atomicrmw_xor -> "xor"
      | Atomicrmw_xchg -> "xchg"

    type op =
      (* Terminator *)
      | Ret of Value.t
      | Br of Value.t
      | Br_cond of
          { cond : Value.t;
            ifso : Value.t;
            ifnot : Value.t
          }
      | Switch of
          { discr : Value.t;
            default : Value.t;
            branches : (Value.t * Value.t) list (* (index, label) *)
          }
      | Unreachable
      (* Basic *)
      | Unary of
          { op : unary_op;
            arg : Value.t
          }
      | Binary of
          { op : binary_op;
            arg1 : Value.t;
            arg2 : Value.t
          }
      | Convert of
          { op : convert_op;
            arg : Value.t;
            to_ : Type.t
          }
      | Icmp of
          { cond : icmp_cond;
            arg1 : Value.t;
            arg2 : Value.t
          }
      | Fcmp of
          { cond : fcmp_cond;
            arg1 : Value.t;
            arg2 : Value.t
          }
      (* Vector ops *)
      | Extractelement of
          { vector : Value.t;
            index : Value.t
          }
      | Insertelement of
          { vector : Value.t;
            index : Value.t;
            to_insert : Value.t
          }
      (* Struct/array ops *)
      | Extractvalue of
          { aggregate : Value.t;
            indices : int list
          }
      | Insertvalue of
          { aggregate : Value.t;
            indices : int list;
            to_insert : Value.t
          }
      (* Memory *)
      | Alloca of Type.t
      | Load of
          { ptr : Value.t;
            typ : Type.t
          }
      | Store of
          { ptr : Value.t;
            to_store : Value.t
          }
      | Getelementptr of
          { base_type : Type.t;
            base_ptr : Value.t;
            indices : Value.t list
          }
      (* Atomics *)
      (* CR yusumez: Implement ordering constraints instead of hard-coding them *)
      | Cmpxchg of
          { ptr : Value.t;
            compare_with : Value.t;
            set_if_equal : Value.t
          }
      | Atomicrmw of
          { op : atomicrmw_op;
            ptr : Value.t;
            arg : Value.t
          }
      (* Control flow *)
      | Select of
          { cond : Value.t;
            ifso : Value.t;
            ifnot : Value.t
          }
      | Call of
          { func : Ident.t;
            args : Value.t list;
            res_type : Type.Or_void.t;
            attrs : Fn_attr.t list;
            cc : Calling_conventions.t;
            musttail : bool
          }
      | Inline_asm of
          { asm : string;
            constraints : string;
            args : Value.t list;
            res_type : Type.Or_void.t;
            sideeffect : bool
          }

    type t =
      { op : op;
        res : Ident.t option
      }

    let assert' name cond = fail_if_not ("Instruction." ^ name) cond

    let op_res_type = function
      (* Terminators return no value *)
      | Ret _ | Br _ | Br_cond _ | Switch _ | Unreachable -> None
      (* Basic operations *)
      | Unary { arg; _ } -> Some (Value.get_type arg)
      | Binary { arg1; _ } -> Some (Value.get_type arg1)
      | Convert { to_; _ } -> Some to_
      | Icmp _ | Fcmp _ -> Some Type.i1
      (* Vector operations *)
      | Extractelement { vector; _ } -> Type.elem_type (Value.get_type vector)
      | Insertelement { vector; _ } -> Some (Value.get_type vector)
      (* Aggregate operations *)
      | Extractvalue { aggregate; indices } ->
        let typ = Type.extract_struct (Value.get_type aggregate) indices in
        assert' "op_res_type" (Option.is_some typ);
        typ
      | Insertvalue { aggregate; _ } -> Some (Value.get_type aggregate)
      (* Memory operations *)
      | Alloca _typ -> Some Type.ptr
      | Load { typ; _ } -> Some typ
      | Store _ -> None
      | Getelementptr { base_ptr; _ } -> Some (Value.get_type base_ptr)
      (* Atomics *)
      | Cmpxchg { compare_with; _ } ->
        let elem_type = Value.get_type compare_with in
        Some (Type.Struct [elem_type; Type.i1])
      | Atomicrmw { arg; _ } -> Some (Value.get_type arg)
      (* Control flow *)
      | Select { ifso; _ } -> Some (Value.get_type ifso)
      | Call { res_type; _ } -> res_type
      | Inline_asm { res_type; _ } -> res_type

    let with_res op ident =
      assert' "with_res" (Option.is_some (op_res_type op));
      { op; res = Some ident }

    let without_res op =
      assert' "without_res" (Option.is_none (op_res_type op));
      { op; res = None }

    let get_res_value { op; res } =
      match res with
      | Some res ->
        Option.map (fun typ -> Value.of_ident ~typ res) (op_res_type op)
      | None -> None

    (* CR yusumez: add checks as necessary *)

    let ret v = Ret v

    let br v =
      assert' "br" (Value.get_type v |> Type.(equal label));
      Br v

    let br_cond ~cond ~ifso ~ifnot =
      assert' "br_cond" (Value.get_type cond |> Type.(equal i1));
      assert' "br_cond" (Value.get_type ifso |> Type.(equal label));
      assert' "br_cond" (Value.get_type ifnot |> Type.(equal label));
      Br_cond { cond; ifso; ifnot }

    let switch ~discr ~default ~branches =
      assert' "switch" (Value.get_type discr |> Type.is_int);
      assert' "switch" (Value.get_type default |> Type.(equal label));
      assert' "switch"
        (List.for_all
           (fun (index, label) ->
             Value.get_type index |> Type.is_int
             && Value.get_type label |> Type.equal Type.label)
           branches);
      Switch { discr; default; branches }

    let unreachable = Unreachable

    let unary op ~arg = Unary { op; arg }

    let binary op ~arg1 ~arg2 = Binary { op; arg1; arg2 }

    let convert op ~arg ~to_ = Convert { op; arg; to_ }

    let icmp cond ~arg1 ~arg2 =
      let arg1_type = Value.get_type arg1 in
      let arg2_type = Value.get_type arg2 in
      assert' "icmp" (Type.equal arg1_type arg2_type);
      assert' "icmp" (Type.is_int arg1_type || Type.is_ptr arg1_type);
      Icmp { cond; arg1; arg2 }

    let fcmp cond ~arg1 ~arg2 =
      let arg1_type = Value.get_type arg1 in
      let arg2_type = Value.get_type arg2 in
      assert' "fcmp" (Type.equal arg1_type arg2_type);
      assert' "fcmp" (Type.is_floating_point arg1_type);
      Fcmp { cond; arg1; arg2 }

    let extractelement ~vector ~index =
      assert' "extractelement" (Value.get_type index |> Type.is_int);
      Extractelement { vector; index }

    let insertelement ~vector ~index ~to_insert =
      assert' "insertelement" (Value.get_type index |> Type.is_int);
      Insertelement { vector; index; to_insert }

    let extractvalue ~aggregate ~indices = Extractvalue { aggregate; indices }

    let insertvalue ~aggregate ~indices ~to_insert =
      Insertvalue { aggregate; indices; to_insert }

    let alloca typ = Alloca typ

    let load ~ptr ~typ =
      assert' "load" (Value.get_type ptr |> Type.is_ptr);
      Load { ptr; typ }

    let store ~ptr ~to_store =
      assert' "store" (Value.get_type ptr |> Type.is_ptr);
      Store { ptr; to_store }

    let getelementptr ~base_type ~base_ptr ~indices =
      assert' "getelementptr" (Value.get_type base_ptr |> Type.is_ptr);
      assert' "getelementptr"
        (List.for_all
           (fun index -> Value.get_type index |> Type.is_int)
           indices);
      Getelementptr { base_type; base_ptr; indices }

    let cmpxchg ~ptr ~compare_with ~set_if_equal =
      assert' "cmpxchg" (Value.get_type ptr |> Type.is_ptr);
      let compare_type = Value.get_type compare_with in
      let set_type = Value.get_type set_if_equal in
      assert' "cmpxchg" (Type.equal compare_type set_type);
      Cmpxchg { ptr; compare_with; set_if_equal }

    let atomicrmw op ~ptr ~arg =
      assert' "atomicrmw" (Value.get_type ptr |> Type.is_ptr);
      Atomicrmw { op; ptr; arg }

    let select ~cond ~ifso ~ifnot =
      assert' "select" (Value.get_type cond |> Type.(equal i1));
      let ifso_type = Value.get_type ifso in
      let ifnot_type = Value.get_type ifnot in
      assert' "select" (Type.equal ifso_type ifnot_type);
      Select { cond; ifso; ifnot }

    let call ~func ~args ~res_type ~attrs ~cc ~musttail =
      Call { func; args; res_type; attrs; cc; musttail }

    let inline_asm ~args ~res_type ~asm ~constraints ~sideeffect =
      Inline_asm { args; res_type; asm; constraints; sideeffect }

    (* Note: this function handles indentation and newlines itself *)
    let pp_t ?comment ppf { op; res } =
      let open F in
      let ins ?(num_indents = 1) fmt = pp_line ~num_indents ?comment ppf fmt in
      let ins_res ?(num_indents = 1) fmt =
        pp_line ~num_indents ?comment ppf ("%a = " ^^ fmt) Ident.pp_t
          (Option.get res)
      in
      match op with
      | Ret v -> ins "ret %a" Value.pp_t v
      | Br v -> ins "br %a" Value.pp_t v
      | Br_cond { cond; ifso; ifnot } ->
        ins "br %a, %a, %a" Value.pp_t cond Value.pp_t ifso Value.pp_t ifnot
      | Switch { discr; default; branches } ->
        ins "switch %a, %a [" Value.pp_t discr Value.pp_t default;
        List.iter
          (fun (index, label) ->
            ins ~num_indents:2 "%a, %a" Value.pp_t index Value.pp_t label)
          branches;
        ins "]"
      | Unreachable -> ins "unreachable"
      | Unary { op; arg } ->
        ins_res "%s %a" (unary_op_to_string op) Value.pp_t arg
      | Binary { op; arg1; arg2 } ->
        ins_res "%s %a, %a" (binary_op_to_string op) Value.pp_t arg1
          Value.pp_value (Value.get_value arg2)
      | Convert { op; arg; to_ } ->
        ins_res "%s %a to %a" (convert_op_to_string op) Value.pp_t arg Type.pp_t
          to_
      | Icmp { cond; arg1; arg2 } ->
        ins_res "icmp %s %a, %a" (icmp_cond_to_string cond) Value.pp_t arg1
          Value.pp_value (Value.get_value arg2)
      | Fcmp { cond; arg1; arg2 } ->
        ins_res "icmp %s %a, %a" (fcmp_cond_to_string cond) Value.pp_t arg1
          Value.pp_value (Value.get_value arg2)
      | Extractelement { vector; index } ->
        ins_res "extractelement %a, %a" Value.pp_t vector Value.pp_t index
      | Insertelement { vector; index; to_insert } ->
        ins_res "insertelement %a, %a, %a" Value.pp_t vector Value.pp_t
          to_insert Value.pp_t index
      | Extractvalue { aggregate; indices } ->
        ins_res "extractvalue %a, %a" Value.pp_t aggregate
          (pp_print_list ~pp_sep:pp_comma pp_print_int)
          indices
      | Insertvalue { aggregate; indices; to_insert } ->
        ins_res "insertvalue %a, %a, %a" Value.pp_t aggregate Value.pp_t
          to_insert
          (pp_print_list ~pp_sep:pp_comma pp_print_int)
          indices
      | Alloca typ -> ins_res "alloca %a" Type.pp_t typ
      | Load { ptr; typ } -> ins_res "load %a, %a" Type.pp_t typ Value.pp_t ptr
      | Store { ptr; to_store } ->
        ins "store %a, %a" Value.pp_t to_store Value.pp_t ptr
      | Getelementptr { base_type; base_ptr; indices } ->
        ins_res "getelementptr %a, %a, %a" Type.pp_t base_type Value.pp_t
          base_ptr
          (pp_print_list ~pp_sep:pp_comma Value.pp_t)
          indices
      | Cmpxchg { ptr; compare_with; set_if_equal } ->
        ins_res "cmpxchg %a, %a, %a acq_rel monotonic" Value.pp_t ptr Value.pp_t
          compare_with Value.pp_t set_if_equal
      | Atomicrmw { op; ptr; arg } ->
        ins_res "atomicrmw %s %a, %a acq_rel"
          (atomicrmw_op_to_string op)
          Value.pp_t ptr Value.pp_t arg
      | Select { cond; ifso; ifnot } ->
        ins_res "select %a, %a, %a" Value.pp_t cond Value.pp_t ifso Value.pp_t
          ifnot
      | Call { func; args; res_type; attrs; cc; musttail } -> (
        let pp_call ppf () =
          fprintf ppf "%acall %a %a %a(%a) %a" (pp_str_if "musttail") musttail
            Calling_conventions.pp_t cc Type.Or_void.pp_t res_type Ident.pp_t
            func
            (pp_print_list ~pp_sep:pp_comma Value.pp_t)
            args Fn_attr.pp_t_list attrs
        in
        match res with
        | Some _ -> ins_res "%a" pp_call ()
        | None -> ins "%a" pp_call ())
      | Inline_asm { args; res_type; asm; constraints; sideeffect } -> (
        let pp_call ppf () =
          fprintf ppf {|call %a asm %a "%s", "%s"(%a)|} Type.Or_void.pp_t
            res_type (pp_str_if "sideeffect") sideeffect asm constraints
            (pp_print_list ~pp_sep:pp_comma Value.pp_t)
            args
        in
        match res with
        | Some _ -> ins_res "%a" pp_call ()
        | None -> ins "%a" pp_call ())
  end

  module Function = struct
    (* CR yusumez: Consider a more structured representation *)
    type slot =
      | Comment of string
      | Label_def of Ident.t
      | Instruction of
          { instr : Instruction.t;
            comment : string option
          }

    type t =
      { name : Ident.t;
        args : (Type.t * Ident.t) list;
            (* Not using [Value.t] to have explicit identifiers *)
        res : Type.Or_void.t;
        cc : Calling_conventions.t;
        attrs : Fn_attr.t list;
        private_ : bool;
        gc : string option;
        dbg : Debuginfo.t;
        mutable body_rev : slot list
      }

    let add_instruction ?comment t instr =
      t.body_rev <- Instruction { instr; comment } :: t.body_rev

    let add_label_def t label = t.body_rev <- Label_def label :: t.body_rev

    let add_comment t comment = t.body_rev <- Comment comment :: t.body_rev

    let pp_t ppf { name; args; res; cc; attrs; private_; gc; dbg; body_rev } =
      let open F in
      (* Definition line *)
      pp_dbg_comment ~newline:true ppf (Ident.to_string_hum name) dbg;
      let pp_private ppf () = if private_ then fprintf ppf "private" in
      let pp_args =
        pp_print_list ~pp_sep:pp_comma (fun ppf (typ, ident) ->
            fprintf ppf "%a %a" Type.pp_t typ Ident.pp_t ident)
      in
      let pp_gc = pp_print_option (fun ppf s -> fprintf ppf "gc %S" s) in
      pp_line ppf "define %a %a %a %a(%a) %a %a {" pp_private ()
        Calling_conventions.pp_t cc Type.Or_void.pp_t res Ident.pp_t name
        pp_args args Fn_attr.pp_t_list attrs pp_gc gc;
      (* Body *)
      let body = List.rev body_rev in
      List.iter
        (function
          | Instruction { instr; comment } ->
            Instruction.pp_t ?comment ppf instr
          | Comment s -> pp_ins ppf "%s" s
          | Label_def ident ->
            pp_line ppf "%s" (Ident.to_label_string_exn ident))
        body;
      pp_line ppf "}";
      pp_line ppf ""

    (* This module handles the creation of [Function.t]s *)
    module Emitter = struct
      type funcdef = t

      type t =
        { ident_gen : Ident.Gen.t;
          funcdef : funcdef
        }

      let create ~name ~args ~res ~cc ~attrs ~gc ~dbg ~private_ =
        let ident_gen = Ident.Gen.create () in
        let name = Ident.global name in
        let args =
          List.map (fun typ -> typ, Ident.Gen.get_fresh ident_gen) args
        in
        (* First unnamed local identifier after parameters is reserved for the
           entry label. *)
        Ident.Gen.get_fresh ident_gen |> ignore;
        let funcdef =
          { name; args; res; cc; attrs; private_; gc; dbg; body_rev = [] }
        in
        { ident_gen; funcdef }

      let get_fun t = t.funcdef

      let get_args_as_values t =
        List.map (fun (typ, ident) -> Value.of_ident ~typ ident) t.funcdef.args

      let get_res_type t = t.funcdef.res

      let get_fun_ident t = t.funcdef.name

      let ins ?res_ident t op =
        let res_ident =
          match res_ident with
          | Some ident -> ident
          | None -> Ident.Gen.get_fresh t.ident_gen
        in
        let instr = Instruction.with_res op res_ident in
        add_instruction t.funcdef instr;
        Instruction.get_res_value instr |> Option.get

      let ins_no_res t op =
        let instr = Instruction.without_res op in
        add_instruction t.funcdef instr

      let comment t s = add_comment t.funcdef s

      let label_def t ident = add_label_def t.funcdef ident
    end
  end

  module Fundecl = struct
    (* CR yusumez: add cc, attrs, etc. as needed *)
    type t =
      { name : string;
        args : Type.t list;
            (* Not using [Value.t] to have explicit identifiers *)
        res : Type.Or_void.t
      }

    let create name args res = { name; args; res }

    let pp_t ppf { name; args; res } =
      let open F in
      let ident = Ident.global name in
      let pp_args = pp_print_list ~pp_sep:pp_comma Type.pp_t in
      pp_line ppf "declare %a %a(%a)" Type.Or_void.pp_t res Ident.pp_t ident
        pp_args args

    let equal { name; args; res } { name = name'; args = args'; res = res' } =
      String.equal name name'
      && List.equal Type.equal args args'
      && Type.Or_void.equal res res'
  end

  module Data = struct
    type t =
      | Constant of
          { name : string;
            value : Value.t;
                (* CR yusumez: check this is actually a constant? *)
            section : string option;
            align : int option;
            private_ : bool
          }
      | External of string

    let constant ?(section = Some ".data") ?(align = Some 8) ?(private_ = false)
        name value =
      Constant { name; value; section; align; private_ }

    let external_ name = External name

    let pp_t ppf = function
      | Constant { name; value; section; align; private_ = _ } ->
        (* CR yusumez: If private global variables are not referenced anywhere
           directly, LLVM will delete them in the globaldce (dead global
           elimination) pass, so we don't mark anything as private for now. *)
        let open F in
        let pp_section =
          pp_print_option (fun ppf section ->
              fprintf ppf ", section \"%s\"" section)
        in
        let pp_align =
          pp_print_option (fun ppf align -> fprintf ppf ", align %d" align)
        in
        let ident = Ident.global name in
        pp_line ppf "%a = global %a%a%a" Ident.pp_t ident Value.pp_t value
          pp_section section pp_align align
      | External name ->
        (* CR yusumez: We don't need that ptr there... *)
        F.pp_line ppf "%a = external global ptr" Ident.pp_t (Ident.global name)
  end
end

module LL = Llvm_ir
module V = LL.Value
module T = LL.Type
module E = LL.Function.Emitter
module I = LL.Instruction

type c_call_wrapper =
  { args : LL.Type.t list;
    res : LL.Type.t list;
    c_fun_name : string
  }

type trap_block_info =
  { trap_block : LL.Value.t;
    stacksave_ptr : LL.Value.t;
    payload : LL.Value.t
  }

type fun_info =
  { emitter : LL.Function.Emitter.t;
        (* Emitter responsible for producing LLVM IR of a function *)
    reg2alloca : LL.Value.t Reg.Tbl.t;
        (* Map [Reg.t]'s from OCaml to alloca'd identifiers in LLVM IR *)
    trap_blocks : trap_block_info Label.Tbl.t
        (* Identifiers created during a [Pushtrap] instruction needed for
           [Poptrap] and trap handler entry *)
  }

type t =
  { llvmir_filename : string;
    oc : Out_channel.t;
    ppf : Format.formatter;
    ppf_dump : Format.formatter;
    mutable sourcefile : string option; (* gets set in [begin_assembly] *)
    mutable asm_filename : string option; (* gets set in [open_out] *)
    mutable current_fun_info : fun_info option;
        (* Maintains the state of the current function (reset for every
           function) *)
    mutable function_defs : LL.Function.t list;
    mutable data_defs : LL.Data.t list;
    mutable defined_symbols : String.Set.t; (* Global symbols defined so far *)
    mutable referenced_symbols : String.Set.t;
        (* Global symbols referenced so far *)
    mutable called_intrinsics : LL.Fundecl.t String.Map.t;
        (* Names + signatures (args, ret) of LLVM intrinsics called so far. Note
           that external functions are treated as symbols and are not declared
           to avoid type mismatches, but intrinsics must be declared with their
           signatures. *)
    mutable c_call_wrappers : c_call_wrapper String.Map.t;
        (* Wrappers for noalloc C calls. This is currently needed since
           manipulating the stack inline is broken. *)
    mutable stack_offset : int
        (* Accumulate [Stackoffset]s to be able to calculate the # of trap
           blocks at a given instruction via [i.stack_offset] *)
  }

(* current_fun_info interface *)

let create_fun_info emitter =
  { emitter; reg2alloca = Reg.Tbl.create 0; trap_blocks = Label.Tbl.create 0 }

let get_fun_info t =
  match t.current_fun_info with
  | Some fun_info -> fun_info
  | None -> fail_msg ~name:"get_fun_info" "not_available"

let reset_fun_info t emitter =
  t.current_fun_info <- Some (create_fun_info emitter)

let get_alloca_for_reg t reg =
  let fun_info = get_fun_info t in
  match Reg.Tbl.find_opt fun_info.reg2alloca reg with
  | Some v -> v
  | None -> fail_msg ~name:"get_alloca_for_reg" "reg not found"

let set_alloca_for_reg t reg alloca =
  let fun_info = get_fun_info t in
  Reg.Tbl.add fun_info.reg2alloca reg alloca
(* t interface *)

let current_compilation_unit = ref None

let get_current_compilation_unit msg =
  match !current_compilation_unit with
  | Some t -> t
  | None -> fail_msg "current compilation unit not set (%s)" msg

let create ~llvmir_filename ~ppf_dump =
  let oc = Out_channel.open_text llvmir_filename in
  let ppf = Format.formatter_of_out_channel oc in
  { llvmir_filename;
    asm_filename = None;
    sourcefile = None;
    oc;
    ppf;
    ppf_dump;
    current_fun_info = None;
    function_defs = [];
    data_defs = [];
    defined_symbols = String.Set.empty;
    referenced_symbols = String.Set.empty;
    called_intrinsics = String.Map.empty;
    c_call_wrappers = String.Map.empty;
    stack_offset = 0
  }

let add_called_intrinsic t name ~args ~res =
  fail_if_not ~msg:"expected intrinsic" "add_called_intrinsic"
    (String.begins_with name ~prefix:"llvm");
  let fundecl = LL.Fundecl.create name args res in
  (match String.Map.find_opt name t.called_intrinsics with
  | None -> ()
  | Some fundecl' ->
    fail_if_not ~msg:"incompatible signatures" "add_called_intrinsic"
      (LL.Fundecl.equal fundecl fundecl'));
  t.called_intrinsics <- String.Map.add name fundecl t.called_intrinsics

let add_c_call_wrapper t c_fun_name ~args ~res =
  let wrapper_name = "c_call_wrapper." ^ c_fun_name in
  (match String.Map.find_opt wrapper_name t.c_call_wrappers with
  | None -> ()
  | Some { c_fun_name = c_fun_name'; args = args'; res = res' } ->
    let all_equal =
      String.equal c_fun_name c_fun_name'
      && List.equal LL.Type.equal args args'
      && List.equal LL.Type.equal res res'
    in
    fail_if_not ~msg:"incompatible signatures" "add_c_call_wrapper" all_equal);
  t.c_call_wrappers
    <- String.Map.add wrapper_name { c_fun_name; args; res } t.c_call_wrappers;
  wrapper_name

let add_defined_symbol t sym_name =
  t.defined_symbols <- String.Set.add sym_name t.defined_symbols

let add_referenced_symbol t sym_name =
  t.referenced_symbols <- String.Set.add sym_name t.referenced_symbols

(* Note that these need to be reversed while emitting *)

let add_function_def t fundef = t.function_defs <- fundef :: t.function_defs

let add_data_def t data_def = t.data_defs <- data_def :: t.data_defs

let complete_func_def t =
  add_function_def t (LL.Function.Emitter.get_fun (get_fun_info t).emitter);
  t.current_fun_info <- None

(* CR yusumez: Consider making a new GCStrategy for OxCaml in LLVM *)
let gc_name = "statepoint-example"

(* Runtime registers. These are registers that get threaded through function
   arguments and returns and are pinned to particular physical registers via the
   calling convention. Note that we treat them similarly to [Reg.t]s, where we
   alloca them in the entry block and access them through load/stores. to be
   simplified by mem2reg in LLVM. We assume they are [i64]s while threading
   through functions. *)

let domainstate_ptr = LL.Value.of_ident ~typ:LL.Type.ptr (LL.Ident.local "ds")

let allocation_ptr = LL.Value.of_ident ~typ:LL.Type.ptr (LL.Ident.local "alloc")

let runtime_regs = [domainstate_ptr; allocation_ptr]

let domainstate_idx = 0

let _allocation_idx = 1

let runtime_reg_idents = List.map LL.Value.get_ident_exn runtime_regs

(* Registers living in the domain state are already stored in the appropriate
   address as arguments or when returned, so we don't touch them while doing a
   call / return *)
(* CR yusumez: We don't expect to get arguments from the stack, but this might
   happen if we have too many arguments. There might be a way to limit this in
   the frontend. *)
let reg_list_for_call regs =
  Array.to_list regs |> List.filter (fun reg -> not (Reg.is_domainstate reg))

let make_ret_type ret_types =
  let runtime_reg_types = List.map (fun _ -> LL.Type.i64) runtime_regs in
  LL.Type.(Struct [Struct runtime_reg_types; Struct ret_types])

let make_ret_type_of_machtype ret_machtype =
  let actual_ret_types =
    Array.to_list ret_machtype |> List.map LL.Type.of_machtype_component
  in
  make_ret_type actual_ret_types

let make_arg_types arg_types =
  List.map (fun _ -> LL.Type.i64) runtime_regs @ arg_types

let emit_ins ?res_ident t op =
  let fun_info = get_fun_info t in
  E.ins ?res_ident fun_info.emitter op

let emit_ins_no_res t op =
  let fun_info = get_fun_info t in
  E.ins_no_res fun_info.emitter op

let emit_label t label_value =
  let fun_info = get_fun_info t in
  E.label_def fun_info.emitter (V.get_ident_exn label_value)

let emit_comment t fmt =
  let fun_info = get_fun_info t in
  F.kasprintf (fun s -> E.comment fun_info.emitter s) fmt

(* Common helpers *)

let cast t arg to_ =
  let from = V.get_type arg in
  if T.equal from to_
  then arg
  else if T.is_int from && T.is_ptr to_
  then emit_ins t (I.convert Inttoptr ~arg ~to_)
  else if T.is_ptr from && T.is_int to_
  then emit_ins t (I.convert Ptrtoint ~arg ~to_)
  else if T.is_ptr from && T.is_ptr to_
  then emit_ins t (I.convert Addrspacecast ~arg ~to_)
  else fail_msg ~name:"cast" "unexpected types: %a, %a" T.pp_t from T.pp_t to_

let cast_to_ptr t arg =
  if T.is_ptr (V.get_type arg) then arg else cast t arg T.ptr

let do_offset ?(int_type = T.i64) t arg res_type offset =
  if offset = 0
  then cast t arg res_type
  else
    let base = cast t arg int_type in
    let offset_val =
      emit_ins t (I.binary Add ~arg1:base ~arg2:(V.of_int offset))
    in
    cast t offset_val res_type

(* CR yusumez: Do we need explicit casts, or can we get away with directly
   loading/storing incompatible types (given that the ptr type doesn't have the
   underlying type attached to it) *)

(* Loading directly with the given type will put an explicit cast after the
   mem2reg pass, so we don't need a cast here. *)
let load_reg_to_temp ?typ t reg =
  let ptr = get_alloca_for_reg t reg in
  let typ = Option.value typ ~default:(T.of_reg reg) in
  emit_ins t (I.load ~ptr ~typ)

(* Although the same applies here as above, the cast happening any time later
   for a store back would be problematic (e.g. consider setting a [ptr
   addrspace(1)] as an [i64]. if it gets cast after a function call, it won't be
   marked as live, which is bad). So, we put the cast explicitly. *)
let store_into_reg t reg to_store =
  let ptr = get_alloca_for_reg t reg in
  let to_store = cast t to_store (T.of_reg reg) in
  emit_ins_no_res t (I.store ~ptr ~to_store)

let load_domainstate_addr ?ds_loc ?(typ = T.ptr) ?(offset = 0) t ds_field =
  let ds =
    match ds_loc with
    | None -> emit_ins t (I.load ~ptr:domainstate_ptr ~typ:T.i64)
    | Some ds_loc -> ds_loc
  in
  let offset = offset + (Domainstate.idx_of_field ds_field * 8) in
  do_offset t ds typ offset

let load_address t addr_mode base typ =
  let offset = Arch.addressing_displacement_for_llvmize addr_mode in
  do_offset t base typ offset

let load_address_from_reg t addr_mode reg =
  let base = load_reg_to_temp t reg in
  load_address t addr_mode base (T.of_reg reg)

let assemble_struct t root_type vals_to_insert =
  let insert cur_struct (indices, to_insert) =
    emit_ins t (I.insertvalue ~aggregate:cur_struct ~indices ~to_insert)
  in
  let init = V.poison root_type in
  List.fold_left insert init vals_to_insert

let extract_struct t root_struct indices_to_extract =
  List.map
    (fun indices -> emit_ins t (I.extractvalue ~aggregate:root_struct ~indices))
    indices_to_extract

(* Helpers for calls *)

let prepare_call_args t args =
  List.map (fun ptr -> emit_ins t (I.load ~ptr ~typ:T.i64)) runtime_regs @ args

let prepare_call_args_from_regs t regs =
  prepare_call_args t (List.map (load_reg_to_temp t) regs)

let extract_call_res t call_res_struct num_res_values =
  (* Runtime regs *)
  let runtime_reg_indices = List.mapi (fun i _ -> [0; i]) runtime_regs in
  let extracted_values = extract_struct t call_res_struct runtime_reg_indices in
  List.iter2
    (fun ptr to_store -> emit_ins_no_res t (I.store ~ptr ~to_store))
    runtime_regs extracted_values;
  (* Actual return values *)
  let res_value_indices = List.init num_res_values (fun i -> [1; i]) in
  extract_struct t call_res_struct res_value_indices

let extract_call_res_into_regs t call_res_struct res_regs =
  let vals = extract_call_res t call_res_struct (List.length res_regs) in
  List.iter2 (store_into_reg t) res_regs vals

let assemble_return t res_type values =
  let runtime_values =
    List.mapi
      (fun i ptr -> [0; i], emit_ins t (I.load ~ptr ~typ:T.i64))
      runtime_regs
  in
  let actual_values = List.mapi (fun i v -> [1; i], v) values in
  assemble_struct t res_type (runtime_values @ actual_values)

(* Prepare and extract arguments following the OCaml calling convention in LLVM,
   handling the threading of runtime registers. *)
let call_simple ?(attrs = []) ~cc t name args res_types =
  let args = prepare_call_args t args in
  let res_type = Some (make_ret_type res_types) in
  let func = LL.Ident.global name in
  let res =
    emit_ins t (I.call ~func ~args ~res_type ~attrs ~cc ~musttail:false)
  in
  extract_call_res t res (List.length res_types)

(* This tells the frametable printer in LLVM to adjust its frame size
   appropriately. This is because it doesn't properly track trap blocks (which
   count as "dynamic objects" on the stack). Note that we multiply by 2 since
   our trap blocks are 4 words wide as opposed to 2 for the normal compiler
   (without frame pointers). OCaml functions aren't supposed to pass arguments
   via the stack, but C calls might, so we need to account for [Stackoffset]
   instructions *)
let statepoint_id_attr_for_stack_adjustment ?(offset = 0) t
    (i : 'a Cfg.instruction) =
  LL.Fn_attr.Statepoint_id (((i.stack_offset - t.stack_offset) * 2) + offset)

let gc_attr ~can_call_gc t i =
  if can_call_gc
  then [statepoint_id_attr_for_stack_adjustment t i]
  else [LL.Fn_attr.Gc_leaf_function]

(* Helpers for LLVM intrinsics *)

let call_llvm_intrinsic_aux ~emit_ins t name args res_type =
  let arg_types = List.map fst args in
  let intrinsic_name = "llvm." ^ name in
  let func = LL.Ident.global intrinsic_name in
  add_called_intrinsic t intrinsic_name ~args:arg_types ~res:res_type;
  emit_ins t
    (I.call ~func ~args ~res_type ~attrs:[] ~cc:Default ~musttail:false)

let call_llvm_intrinsic t name args res_type =
  call_llvm_intrinsic_aux
    ~emit_ins:(fun t -> emit_ins t)
    t name args (Some res_type)

let call_llvm_intrinsic_no_res t name args =
  call_llvm_intrinsic_aux ~emit_ins:emit_ins_no_res t name args None

let read_rsp t =
  call_llvm_intrinsic t "read_register.i64"
    [V.imm T.metadata {|!{!"rsp\00"}|}]
    T.i64

let write_rsp t v =
  let v = cast t v T.i64 in
  call_llvm_intrinsic_no_res t "write_register.i64"
    [V.imm T.metadata {|!{!"rsp\00"}|}; v]

(* Other miscellaneous stuff... *)

let reject_addr_regs (regs : Reg.t array) msg =
  if Array.to_list regs
     |> List.map (fun (reg : Reg.t) -> Cmm.is_addr reg.typ)
     |> List.fold_left ( || ) false
  then fail_msg ~name:"reject_addr_regs" "%s" msg

let br_label t label = emit_ins_no_res t (I.br (V.of_label label))

(* Terminator instructions *)

let int_comp t cond (i : _ Cfg.instruction) ~imm =
  let typ = T.i64 in
  let cond = I.icmp_cond_of_ocaml cond in
  match imm with
  | None ->
    let arg1 = load_reg_to_temp ~typ t i.arg.(0) in
    let arg2 = load_reg_to_temp ~typ t i.arg.(1) in
    emit_ins t (I.icmp cond ~arg1 ~arg2)
  | Some n ->
    let arg1 = load_reg_to_temp ~typ t i.arg.(0) in
    let arg2 = V.of_int ~typ n in
    emit_ins t (I.icmp cond ~arg1 ~arg2)

let float_comp t cond (i : _ Cfg.instruction) typ =
  let cond = I.fcmp_cond_of_ocaml cond in
  let arg1 = load_reg_to_temp ~typ t i.arg.(0) in
  let arg2 = load_reg_to_temp ~typ t i.arg.(1) in
  emit_ins t (I.fcmp cond ~arg1 ~arg2)

let odd_test t (i : _ Cfg.instruction) =
  let arg = load_reg_to_temp ~typ:T.i64 t i.arg.(0) in
  emit_ins t (I.convert Trunc ~arg ~to_:T.i1)

let test t (op : Operation.test) (i : _ Cfg.instruction) =
  match op with
  | Itruetest -> int_comp t Cne i ~imm:(Some 0)
  | Ifalsetest -> int_comp t Ceq i ~imm:(Some 0)
  | Iinttest int_comp_op -> int_comp t int_comp_op i ~imm:None
  | Iinttest_imm (int_comp_op, imm) -> int_comp t int_comp_op i ~imm:(Some imm)
  | Ifloattest (width, float_comp_op) ->
    let typ = T.of_float_width width in
    float_comp t float_comp_op i typ
  | Ioddtest -> odd_test t i
  | Ieventest ->
    let is_odd = odd_test t i in
    emit_ins t (I.binary Xor ~arg1:is_odd ~arg2:(V.of_int ~typ:T.i1 1))

let call ?(tail = false) t (i : Cfg.terminator Cfg.instruction)
    (op : Cfg.func_call_operation) =
  let args_begin, args_end =
    (* [Indirect] has the function in i.arg.(0) *)
    match op.callee with
    | Direct _ -> 0, Array.length i.arg
    | Indirect -> 1, Array.length i.arg - 1
  in
  let arg_regs = Array.sub i.arg args_begin args_end |> reg_list_for_call in
  let args = prepare_call_args_from_regs t arg_regs in
  let res_regs = reg_list_for_call i.res in
  let res_type =
    (* This will always be [Some] *)
    if tail
    then E.get_res_type (get_fun_info t).emitter
    else Some (make_ret_type (List.map T.of_reg res_regs))
  in
  let func =
    match op.callee with
    | Direct { sym_name; sym_global = _ } ->
      add_referenced_symbol t sym_name;
      LL.Ident.global sym_name
    | Indirect -> load_reg_to_temp ~typ:T.ptr t i.arg.(0) |> V.get_ident_exn
  in
  let attrs = gc_attr ~can_call_gc:true t i in
  let res =
    emit_ins t (I.call ~func ~args ~res_type ~attrs ~cc:Ocaml ~musttail:tail)
  in
  if tail
  then emit_ins_no_res t (I.ret res)
  else extract_call_res_into_regs t res res_regs

let return t (i : Cfg.terminator Cfg.instruction) =
  let res_regs = reg_list_for_call i.arg in
  let res_type = E.get_res_type (get_fun_info t).emitter |> Option.get in
  (* Note: type information is not propagated to the backend for functions that
     never return, like [raise], so there might be type mismatches. If that is
     the case, this point is unreachable. *)
  let res_type_elems =
    Option.bind (T.extract_struct res_type [1]) T.get_struct_elements
    |> Option.get
  in
  if List.length res_type_elems <> List.length res_regs
  then emit_ins_no_res t I.unreachable
  else
    let res_values =
      List.map2
        (fun reg typ -> load_reg_to_temp ~typ t reg)
        res_regs res_type_elems
    in
    let res = assemble_return t res_type res_values in
    emit_ins_no_res t (I.ret res)

let extcall t (i : Cfg.terminator Cfg.instruction) ~func_symbol ~alloc
    ~stack_ofs ~stack_align =
  let func_ptr = V.of_symbol func_symbol in
  let make_ocaml_c_call caml_c_call_symbol args res_types =
    add_referenced_symbol t caml_c_call_symbol;
    add_referenced_symbol t func_symbol;
    call_simple
      ~attrs:(gc_attr ~can_call_gc:true t i)
      ~cc:Ocaml_c_call t caml_c_call_symbol args res_types
    (* CR yusumez: Record frame table here *)
  in
  let call_func args res_types =
    if stack_ofs > 0
    then
      (* [caml_c_call_stack_args_llvm_backend] is a wrapper around
         [caml_c_call_stack_args] which computes the address range of the
         arguments on the stack given the offset. *)
      let args = [func_ptr; V.of_int stack_ofs] @ args in
      let caml_c_call_stack_args =
        "caml_c_call_stack_args_llvm_backend"
        ^
        match (stack_align : Cmm.stack_align) with
        | Align_16 -> ""
        | Align_32 -> "_avx"
        | Align_64 -> "_avx512"
      in
      make_ocaml_c_call caml_c_call_stack_args args res_types
    else if alloc
    then
      (* [caml_c_call] doesn't use the second argument since nothing is passed
         on the stack *)
      let args = [func_ptr; V.poison T.i64] @ args in
      make_ocaml_c_call "caml_c_call" args res_types
    else
      (* Wrap C calls to avoid reloading from the stack after overwriting the
         stack pointer *)
      let wrapper_symbol =
        add_c_call_wrapper t func_symbol ~args:(List.map V.get_type args)
          ~res:res_types
      in
      add_referenced_symbol t func_symbol;
      call_simple
        ~attrs:(gc_attr ~can_call_gc:false t i)
        ~cc:Ocaml t wrapper_symbol args res_types
  in
  let arg_regs = reg_list_for_call i.arg in
  let args = List.map (load_reg_to_temp t) arg_regs in
  let res_regs = reg_list_for_call i.res in
  let res_types = List.map T.of_reg res_regs in
  let res_values = call_func args res_types in
  List.iter2 (store_into_reg t) res_regs res_values

let raise_ t (i : Cfg.terminator Cfg.instruction)
    (raise_kind : Lambda.raise_kind) =
  let call_raise raise_fn_name =
    let payload = load_reg_to_temp t i.arg.(0) in
    add_referenced_symbol t raise_fn_name;
    call_simple
      ~attrs:(gc_attr ~can_call_gc:true t i)
      ~cc:Ocaml t raise_fn_name [payload] []
    |> ignore;
    emit_ins_no_res t I.unreachable
  in
  match raise_kind with
  | Raise_notrace ->
    (* Get sp for trap block *)
    let exn_sp_ptr = load_domainstate_addr t Domain_exn_handler in
    let trap_block = emit_ins t (I.load ~ptr:exn_sp_ptr ~typ:T.ptr) in
    (* Get contents of the trap block *)
    let prev_exn_sp = emit_ins t (I.load ~ptr:trap_block ~typ:T.i64) in
    let handler_addr =
      let ptr = do_offset t trap_block T.ptr 8 in
      emit_ins t (I.load ~ptr ~typ:T.i64)
    in
    (* Pop trap block from linked list in the domain *)
    emit_ins_no_res t (I.store ~ptr:exn_sp_ptr ~to_store:prev_exn_sp);
    (* Get payload *)
    let payload = load_reg_to_temp t i.arg.(0) in
    (* Pop trap block from stack + set my sp *)
    let new_sp = do_offset t trap_block T.i64 16 in
    write_rsp t new_sp;
    (* Put payload in RAX and jump to handler *)
    emit_ins_no_res t
      (I.inline_asm ~asm:"movq $0, %rax; jmpq *$1" ~constraints:"r,r,~{rax}"
         ~args:[payload; handler_addr] ~res_type:T.Or_void.void ~sideeffect:true);
    emit_ins_no_res t I.unreachable
  | Raise_regular ->
    let backtrace_pos = load_domainstate_addr t Domain_backtrace_pos in
    emit_ins_no_res t (I.store ~ptr:backtrace_pos ~to_store:(V.of_int 0));
    call_raise "caml_raise_exn"
  | Raise_reraise -> call_raise "caml_reraise_exn"

let emit_terminator t (i : Cfg.terminator Cfg.instruction) =
  emit_comment t "%a" F.pp_dbg_instr_terminator i;
  match i.desc with
  | Never -> fail "terminator.Never"
  | Always lbl -> br_label t lbl
  | Parity_test { ifso; ifnot } ->
    emit_ins_no_res t
      (I.br_cond ~cond:(odd_test t i) ~ifso:(V.of_label ifso)
         ~ifnot:(V.of_label ifnot))
  | Truth_test { ifso; ifnot } ->
    emit_ins_no_res t
      (I.br_cond ~cond:(test t Itruetest i) ~ifso:(V.of_label ifso)
         ~ifnot:(V.of_label ifnot))
  | Return -> return t i
  | Int_test { lt; eq; gt; is_signed; imm } ->
    let open struct
      type comp =
        | Lt
        | Gt
    end in
    let make_comp (comp : comp) : Cmm.integer_comparison =
      match is_signed, comp with
      | Signed, Lt -> Clt
      | Signed, Gt -> Cgt
      | Unsigned, Lt -> Cult
      | Unsigned, Gt -> Cugt
    in
    let lt = V.of_label lt in
    let eq = V.of_label eq in
    let gt = V.of_label gt in
    let ge = Cmm.new_label () |> V.of_label in
    let is_lt = int_comp t (make_comp Lt) i ~imm in
    emit_ins_no_res t (I.br_cond ~cond:is_lt ~ifso:lt ~ifnot:ge);
    emit_label t ge;
    let is_gt = int_comp t (make_comp Gt) i ~imm in
    emit_ins_no_res t (I.br_cond ~cond:is_gt ~ifso:gt ~ifnot:eq)
  | Float_test { width; lt; eq; gt; uo } ->
    let typ = T.of_float_width width in
    let lt = V.of_label lt in
    let eq = V.of_label eq in
    let gt = V.of_label gt in
    let uo = V.of_label uo in
    let ge = V.of_label (Cmm.new_label ()) in
    let eq_or_uo = V.of_label (Cmm.new_label ()) in
    let is_lt = float_comp t Cmm.CFlt i typ in
    emit_ins_no_res t (I.br_cond ~cond:is_lt ~ifso:lt ~ifnot:ge);
    emit_label t ge;
    let is_gt = float_comp t Cmm.CFgt i typ in
    emit_ins_no_res t (I.br_cond ~cond:is_gt ~ifso:gt ~ifnot:eq_or_uo);
    emit_label t eq_or_uo;
    let is_eq = float_comp t Cmm.CFeq i typ in
    emit_ins_no_res t (I.br_cond ~cond:is_eq ~ifso:eq ~ifnot:uo)
  | Switch labels ->
    let discr = load_reg_to_temp ~typ:T.i64 t i.arg.(0) in
    let default = V.of_label (Cmm.new_label ()) in
    let branches =
      List.mapi
        (fun i label -> V.of_int i, V.of_label label)
        (Array.to_list labels)
    in
    emit_ins_no_res t (I.switch ~discr ~default ~branches);
    (* note: [Switch] does not take a default label, as switches are assumed to
       be exhaustive. Since it is mandatory in LLVM, we will use a label with an
       [unreachable] instruction for every instance of this instruction. *)
    emit_label t default;
    emit_ins_no_res t I.unreachable
  | Raise raise_kind -> raise_ t i raise_kind
  | Call { op; label_after } ->
    reject_addr_regs i.arg "call";
    call t i op;
    br_label t label_after
  | Tailcall_self { destination } -> br_label t destination
  | Tailcall_func op ->
    reject_addr_regs i.arg "tailcall func";
    call ~tail:true t i op
  | Call_no_return { func_symbol; alloc; stack_ofs; stack_align; _ } ->
    extcall t i ~func_symbol ~alloc ~stack_ofs ~stack_align;
    emit_ins_no_res t I.unreachable
  | Prim { op; label_after } -> (
    reject_addr_regs i.arg "prim";
    match op with
    | Probe _ -> not_implemented_terminator ~msg:"probe" i
    | External { func_symbol; alloc; stack_ofs; stack_align; _ } ->
      extcall t i ~func_symbol ~alloc ~stack_ofs ~stack_align;
      br_label t label_after)

(* Basic instructions *)

let int_op t (i : Cfg.basic Cfg.instruction) (op : Operation.integer_operation)
    ~imm =
  let do_binary op =
    let typ = T.i64 in
    reject_addr_regs i.res "int_op";
    match imm with
    | None ->
      let arg1 = load_reg_to_temp ~typ t i.arg.(0) in
      let arg2 = load_reg_to_temp ~typ t i.arg.(1) in
      emit_ins t (I.binary op ~arg1 ~arg2)
    | Some n ->
      let arg1 = load_reg_to_temp ~typ t i.arg.(0) in
      let arg2 = V.of_int ~typ n in
      emit_ins t (I.binary op ~arg1 ~arg2)
  in
  let do_unary_intrinsic op_name =
    let typ = T.i64 in
    let arg = load_reg_to_temp ~typ t i.arg.(0) in
    call_llvm_intrinsic t (op_name ^ "." ^ T.to_string typ) [arg] typ
  in
  let do_gep ~negate_arg =
    let base_ptr = load_reg_to_temp t i.arg.(0) in
    fail_if_not "int_op.do_gep" (T.is_ptr (V.get_type base_ptr));
    let offset =
      match imm with
      | None ->
        let temp = load_reg_to_temp ~typ:T.i64 t i.arg.(1) in
        if negate_arg
        then emit_ins t (I.binary Sub ~arg1:(V.of_int 0) ~arg2:temp)
        else temp
      | Some n -> V.of_int ~typ:T.i64 (if negate_arg then -n else n)
    in
    emit_ins t (I.getelementptr ~base_type:T.i8 ~base_ptr ~indices:[offset])
  in
  let do_imulh ~signed =
    (* Assuming operands are i64 *)
    let arg1 = load_reg_to_temp ~typ:T.i64 t i.arg.(0) in
    let arg2 =
      match imm with
      | None -> load_reg_to_temp ~typ:T.i64 t i.arg.(1)
      | Some n -> V.of_int n
    in
    (* Extend args to i128 *)
    let extend_value arg =
      let ext_op = if signed then I.Sext else I.Zext in
      emit_ins t (I.convert ext_op ~arg ~to_:T.i128)
    in
    let arg1 = extend_value arg1 in
    let arg2 = extend_value arg2 in
    (* Multiply as i128 *)
    let res_ext = emit_ins t (I.binary Mul ~arg1 ~arg2) in
    (* Shift the bits we care about *)
    let shifted =
      emit_ins t (I.binary Lshr ~arg1:res_ext ~arg2:(V.of_int ~typ:T.i128 64))
    in
    emit_ins t (I.convert Trunc ~arg:shifted ~to_:T.i64)
  in
  let res =
    match op with
    | Iadd ->
      if Cmm.is_addr i.res.(0).typ
      then do_gep ~negate_arg:false
      else do_binary Add
    | Isub ->
      if Cmm.is_addr i.res.(0).typ
      then do_gep ~negate_arg:true
      else do_binary Sub
    | Imul -> do_binary Mul
    | Imulh { signed } -> do_imulh ~signed
    | Idiv -> do_binary Sdiv
    | Imod -> do_binary Srem
    | Iand -> do_binary And
    | Ior -> do_binary Or
    | Ixor -> do_binary Xor
    | Ilsl -> do_binary Shl
    | Ilsr -> do_binary Lshr
    | Iasr -> do_binary Ashr
    | Icomp comp ->
      let bool_res = int_comp t comp i ~imm in
      (* convert i1 -> i64 *)
      emit_ins t (I.convert Zext ~arg:bool_res ~to_:T.i64)
    | Iclz _ -> do_unary_intrinsic "ctlz"
    | Ictz _ -> do_unary_intrinsic "cttz"
    | Ipopcnt -> do_unary_intrinsic "ctpop"
  in
  store_into_reg t i.res.(0) res

let float_op t (i : Cfg.basic Cfg.instruction) (width : Cmm.float_width)
    (op : Operation.float_operation) =
  let typ = T.of_float_width width in
  let do_binary op =
    let arg1 = load_reg_to_temp ~typ t i.arg.(0) in
    let arg2 = load_reg_to_temp ~typ t i.arg.(1) in
    emit_ins t (I.binary op ~arg1 ~arg2)
  in
  let do_unary_intrinsic op_name =
    let arg = load_reg_to_temp ~typ t i.arg.(0) in
    call_llvm_intrinsic t (op_name ^ "." ^ T.to_string typ) [arg] typ
  in
  let res =
    match op with
    | Iaddf -> do_binary Fadd
    | Isubf -> do_binary Fsub
    | Imulf -> do_binary Fmul
    | Idivf -> do_binary Fdiv
    | Inegf ->
      let arg = load_reg_to_temp ~typ t i.arg.(0) in
      emit_ins t (I.unary Fneg ~arg)
    | Iabsf -> do_unary_intrinsic "fabs"
    | Icompf comp ->
      let bool_res = float_comp t comp i typ in
      (* convert i1 -> i64 *)
      emit_ins t (I.convert Zext ~arg:bool_res ~to_:T.i64)
  in
  store_into_reg t i.res.(0) res

(* CR yusumez: Put these into the appropriate arch-specific files *)

(* CR yusumez: add a generic Cfg instruction for bswap *)
let bswap t (i : Cfg.basic Cfg.instruction) (bitwidth : Arch.bswap_bitwidth) =
  let typ =
    match bitwidth with
    | Sixteen -> T.i16
    | Thirtytwo -> T.i32
    | Sixtyfour -> T.i64
  in
  let do_trunc arg =
    if T.equal typ T.i64
    then arg
    else emit_ins t (I.convert Trunc ~arg ~to_:T.i64)
  in
  let do_zext arg =
    if T.equal typ T.i64
    then arg
    else emit_ins t (I.convert Zext ~arg ~to_:T.i64)
  in
  let arg = load_reg_to_temp t i.arg.(0) in
  let trunced = do_trunc arg in
  let bswapped =
    call_llvm_intrinsic t ("bswap." ^ T.to_string typ) [trunced] typ
  in
  let zexted = do_zext bswapped in
  store_into_reg t i.res.(0) zexted

let intrinsic t (i : Cfg.basic Cfg.instruction) intrinsic_name =
  (* CR yusumez: I really don't like the -fragile-match... *)
  let do_conv arg (to_ : T.t) =
    let from : T.t = V.get_type arg in
    match[@warning "-fragile-match"] from, to_ with
    | _ when T.equal from to_ -> arg
    | Double, Vector { size = _; elem_type = Double } ->
      emit_ins t
        (I.insertelement ~vector:(V.poison to_) ~index:(V.of_int 0)
           ~to_insert:arg)
    | Vector { size = _; elem_type = Double }, Double ->
      emit_ins t (I.extractelement ~vector:arg ~index:(V.of_int 0))
    | Int { width_in_bits = 64 }, Int { width_in_bits = 32 } ->
      emit_ins t (I.convert Trunc ~arg ~to_)
    | _ -> Misc.fatal_error "Llvmize: unexpected reg types in intrinsic"
  in
  let do_intrinsic_call name arg_types res_type =
    (* Sometimes, we get unit arguments for intrinsics with no arguments. We use
       [map2_prefix] to ignore them. *)
    let args, _ =
      List.map2_prefix
        (fun arg_type reg ->
          let temp = load_reg_to_temp t reg in
          do_conv temp arg_type)
        arg_types (Array.to_list i.arg)
    in
    let res = call_llvm_intrinsic t name args res_type in
    let conved_res = do_conv res (T.of_reg i.res.(0)) in
    store_into_reg t i.res.(0) conved_res
  in
  match intrinsic_name with
  | "caml_sse2_float64_min" ->
    do_intrinsic_call "x86.sse2.min.sd" [T.doublex2; T.doublex2] T.doublex2
  | "caml_sse2_float64_max" ->
    do_intrinsic_call "x86.sse2.max.sd" [T.doublex2; T.doublex2] T.doublex2
  | "caml_rdtsc_unboxed" -> do_intrinsic_call "readcyclecounter" [] T.i64
  | "caml_rdpmc_unboxed" -> do_intrinsic_call "x86.rdpmc" [T.i32] T.i64
  | _ -> not_implemented_basic ~msg:"specific intrinsic" i

let specific t (i : Cfg.basic Cfg.instruction) (op : Arch.specific_operation) =
  match op with
  | Ibswap { bitwidth } -> bswap t i bitwidth
  | Illvm_intrinsic intrinsic_name -> intrinsic t i intrinsic_name
  | Ilea _ | Istore_int _ | Ioffset_loc _ | Ifloatarithmem _ | Isextend32
  | Izextend32 | Irdtsc | Irdpmc | Ilfence | Isfence | Imfence | Ipackf32
  | Isimd _ | Isimd_mem _ | Icldemote _ | Iprefetch _ ->
    not_implemented_basic ~msg:"specific" i

(* CR yusumez: Implement atomic operations properly, since the current
   implementation is most likely incorrect. Check how the C++ memory model (or
   whatever LLVM supports the closest to ours) matches with OCaml's (memory.c is
   a nice place to start) *)
let atomic t (i : Cfg.basic Cfg.instruction) (op : Cmm.atomic_op) ~size ~addr =
  (* CR yusumez: potential val mismatch? (test this) *)
  let ptr_arg_idx =
    match op with
    | Compare_set -> 2
    | Fetch_and_add -> 1
    | Add | Sub | Land | Lor | Lxor -> 1
    | Exchange -> 1
    | Compare_exchange -> 2
  in
  let typ =
    match (size : Cmm.atomic_bitwidth) with
    | Thirtytwo -> T.i32
    | Sixtyfour | Word -> T.i64
  in
  let arg = load_reg_to_temp ~typ t i.arg.(ptr_arg_idx - 1) in
  let ptr = load_address_from_reg t addr i.arg.(ptr_arg_idx) |> cast_to_ptr t in
  let do_atomicrmw ?(set_res = false) op =
    let res = emit_ins t (I.atomicrmw op ~ptr ~arg) in
    if set_res then store_into_reg t i.res.(0) res
  in
  let do_cmpxchg () =
    let compare_with = load_reg_to_temp ~typ t i.arg.(0) in
    let res = emit_ins t (I.cmpxchg ~ptr ~compare_with ~set_if_equal:arg) in
    match extract_struct t res [[0] (* loaded *); [1] (* success *)] with
    | [loaded; success] -> loaded, success
    | [] | _ :: _ -> fail "atomic.do_cmpxchg"
  in
  match op with
  | Fetch_and_add -> do_atomicrmw ~set_res:true Atomicrmw_add
  | Add -> do_atomicrmw Atomicrmw_add
  | Sub -> do_atomicrmw Atomicrmw_sub
  | Land -> do_atomicrmw Atomicrmw_and
  | Lor -> do_atomicrmw Atomicrmw_or
  | Lxor -> do_atomicrmw Atomicrmw_xor
  | Exchange -> do_atomicrmw ~set_res:true Atomicrmw_xchg
  | Compare_set ->
    let _, success = do_cmpxchg () in
    (* convert i1 -> i64 *)
    let res = emit_ins t (I.convert Zext ~arg:success ~to_:typ) in
    store_into_reg t i.res.(0) res
  | Compare_exchange ->
    let loaded, success = do_cmpxchg () in
    let orig = load_reg_to_temp ~typ t i.res.(0) in
    let selected =
      emit_ins t (I.select ~cond:success ~ifso:orig ~ifnot:loaded)
    in
    store_into_reg t i.res.(0) selected

let load t (i : Cfg.basic Cfg.instruction) (memory_chunk : Cmm.memory_chunk)
    (addr_mode : Arch.addressing_mode) =
  let ptr = load_address_from_reg t addr_mode i.arg.(0) |> cast_to_ptr t in
  let basic typ =
    let loaded = emit_ins t (I.load ~ptr ~typ) in
    store_into_reg t i.res.(0) loaded
  in
  let extend op ~from ~to_ =
    let loaded = emit_ins t (I.load ~ptr ~typ:from) in
    let extended = emit_ins t (I.convert op ~arg:loaded ~to_) in
    store_into_reg t i.res.(0) extended
  in
  match memory_chunk with
  | Word_int -> basic T.i64
  | Word_val -> basic T.val_ptr
  | Byte_unsigned -> extend Zext ~from:T.i8 ~to_:T.i64
  | Byte_signed -> extend Sext ~from:T.i8 ~to_:T.i64
  | Sixteen_unsigned -> extend Zext ~from:T.i16 ~to_:T.i64
  | Sixteen_signed -> extend Sext ~from:T.i16 ~to_:T.i64
  | Thirtytwo_unsigned -> extend Zext ~from:T.i32 ~to_:T.i64
  | Thirtytwo_signed -> extend Sext ~from:T.i32 ~to_:T.i64
  | Single { reg = Float32 } -> basic T.float
  | Double -> basic T.double
  | Single { reg = Float64 } -> extend Fpext ~from:T.float ~to_:T.double
  | Onetwentyeight_unaligned | Onetwentyeight_aligned | Twofiftysix_unaligned
  | Twofiftysix_aligned | Fivetwelve_unaligned | Fivetwelve_aligned ->
    not_implemented_basic ~msg:"load vector" i

let store t (i : Cfg.basic Cfg.instruction) (memory_chunk : Cmm.memory_chunk)
    (addr_mode : Arch.addressing_mode) =
  let ptr = load_address_from_reg t addr_mode i.arg.(1) |> cast_to_ptr t in
  let basic typ =
    let to_store = load_reg_to_temp ~typ t i.arg.(0) in
    emit_ins_no_res t (I.store ~ptr ~to_store)
  in
  let trunc op to_ =
    let arg = load_reg_to_temp t i.arg.(0) in
    let to_store = emit_ins t (I.convert op ~arg ~to_) in
    emit_ins_no_res t (I.store ~ptr ~to_store)
  in
  match memory_chunk with
  | Word_int -> basic T.i64
  | Word_val -> basic T.val_ptr
  | Byte_unsigned | Byte_signed -> trunc Trunc T.i8
  | Sixteen_unsigned | Sixteen_signed -> trunc Trunc T.i16
  | Thirtytwo_signed | Thirtytwo_unsigned -> trunc Trunc T.i32
  | Single { reg = Float32 } -> basic T.float
  | Double -> basic T.double
  | Single { reg = Float64 } -> trunc Fptrunc T.float
  | Onetwentyeight_unaligned | Onetwentyeight_aligned | Twofiftysix_unaligned
  | Twofiftysix_aligned | Fivetwelve_unaligned | Fivetwelve_aligned ->
    not_implemented_basic ~msg:"store vector" i

let local_alloc t (i : Cfg.basic Cfg.instruction) num_bytes =
  (* Make space on the local stack *)
  let local_sp_ptr = load_domainstate_addr t Domain_local_sp in
  let local_sp = emit_ins t (I.load ~ptr:local_sp_ptr ~typ:T.i64) in
  let new_local_sp =
    emit_ins t (I.binary Sub ~arg1:local_sp ~arg2:(V.of_int num_bytes))
  in
  emit_ins_no_res t (I.store ~ptr:local_sp_ptr ~to_store:new_local_sp);
  (* Check if new_local_sp exceeds local_limit *)
  let local_limit_ptr = load_domainstate_addr t Domain_local_limit in
  let local_limit = emit_ins t (I.load ~ptr:local_limit_ptr ~typ:T.i64) in
  let skip_realloc =
    emit_ins t (I.icmp Islt ~arg1:local_limit ~arg2:new_local_sp)
  in
  (* Let LLVM know calling realloc isn't likely *)
  let skip_realloc_expect =
    call_llvm_intrinsic t "expect.i1" [skip_realloc; V.of_int ~typ:T.i1 1] T.i1
  in
  (* Branch appropriately *)
  let call_realloc = V.of_label (Cmm.new_label ()) in
  let after_realloc = V.of_label (Cmm.new_label ()) in
  emit_ins_no_res t
    (I.br_cond ~cond:skip_realloc_expect ~ifso:after_realloc ~ifnot:call_realloc);
  (* Call realloc *)
  emit_label t call_realloc;
  (* CR yusumez: Handle SIMD regs appropriately once we have them *)
  (* CR yusumez: Fix calling conventions in LLVM for this (and the heap one) to
     mark registers saved/clobbered as in Proc.destroyed_at_alloc_or_poll. We
     might also want to thread runtime registers through them (especially for
     heap allocations since they adjust R15) *)
  add_referenced_symbol t "caml_call_local_realloc";
  call_simple
    ~attrs:(gc_attr ~can_call_gc:false t i @ [LL.Fn_attr.Cold])
    ~cc:Ocaml t "caml_call_local_realloc" [] []
  |> ignore;
  emit_ins_no_res t (I.br after_realloc);
  (* After alloc *)
  emit_label t after_realloc;
  let local_top_ptr = load_domainstate_addr t Domain_local_top in
  let local_top = emit_ins t (I.load ~ptr:local_top_ptr ~typ:T.i64) in
  (* Calculate the address of the object on the local stack *)
  let new_local_sp_addr =
    emit_ins t (I.binary Add ~arg1:new_local_sp ~arg2:local_top)
  in
  (* Skip the header word *)
  let res = do_offset t new_local_sp_addr T.val_ptr 8 in
  store_into_reg t i.res.(0) res

let heap_alloc t (i : Cfg.basic Cfg.instruction) num_bytes =
  (* Make space on the minor heap *)
  let alloc_ptr = emit_ins t (I.load ~ptr:allocation_ptr ~typ:T.i64) in
  let new_alloc_ptr =
    emit_ins t (I.binary Sub ~arg1:alloc_ptr ~arg2:(V.of_int num_bytes))
  in
  emit_ins_no_res t (I.store ~ptr:allocation_ptr ~to_store:new_alloc_ptr);
  (* Check if we exceeded the limit *)
  let domain_young_limit =
    let ptr = load_domainstate_addr t Domain_young_limit in
    emit_ins t (I.load ~ptr ~typ:T.i64)
  in
  let skip_gc =
    emit_ins t (I.icmp Iult ~arg1:domain_young_limit ~arg2:new_alloc_ptr)
  in
  (* Let LLVM know calling GC isn't likely *)
  let skip_gc_expect =
    call_llvm_intrinsic t "expect.i1" [skip_gc; V.of_int ~typ:T.i1 1] T.i1
  in
  (* Branch appropriately *)
  let call_gc = V.of_label (Cmm.new_label ()) in
  let after_gc = V.of_label (Cmm.new_label ()) in
  emit_ins_no_res t
    (I.br_cond ~cond:skip_gc_expect ~ifso:after_gc ~ifnot:call_gc);
  (* Call GC *)
  emit_label t call_gc;
  add_referenced_symbol t "caml_call_gc";
  call_simple
    ~attrs:(gc_attr ~can_call_gc:true t i @ [LL.Fn_attr.Cold])
    ~cc:Ocaml t "caml_call_gc" [] []
  |> ignore;
  emit_ins_no_res t (I.br after_gc);
  (* After GC *)
  emit_label t after_gc;
  (* Load alloc ptr again since GC call might have changed it *)
  let alloc_ptr = emit_ins t (I.load ~ptr:allocation_ptr ~typ:T.i64) in
  (* Skip the header word *)
  let res = do_offset t alloc_ptr T.val_ptr 8 in
  store_into_reg t i.res.(0) res

let basic_op t (i : Cfg.basic Cfg.instruction) (op : Operation.t) =
  match op with
  | Move -> load_reg_to_temp t i.arg.(0) |> store_into_reg t i.res.(0)
  | Opaque ->
    let temp = load_reg_to_temp t i.arg.(0) in
    let opaque_temp =
      emit_ins t
        (I.inline_asm ~asm:"" ~constraints:"=r,0" ~args:[temp]
           ~res_type:(Some (V.get_type temp))
           ~sideeffect:false)
    in
    store_into_reg t i.res.(0) opaque_temp
  | Const_int n -> store_into_reg t i.res.(0) (V.of_nativeint n)
  | Const_symbol { sym_name; sym_global = _ } ->
    add_referenced_symbol t sym_name;
    store_into_reg t i.res.(0) (V.of_symbol sym_name)
  | Const_float32 bits -> store_into_reg t i.res.(0) (V.of_float32_bits bits)
  | Const_float bits -> store_into_reg t i.res.(0) (V.of_float64_bits bits)
  | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ ->
    not_implemented_basic ~msg:"const_vec" i
  (* CR yusumez: What do we do with mutability / is_atomic / is_modify? *)
  | Load { memory_chunk; addressing_mode; mutability = _; is_atomic = _ } ->
    load t i memory_chunk addressing_mode
  | Store (memory_chunk, addressing_mode, _is_modify) ->
    store t i memory_chunk addressing_mode
  | Intop op -> int_op t i op ~imm:None
  | Intop_imm (op, n) -> int_op t i op ~imm:(Some n)
  | Floatop (width, op) -> float_op t i width op
  | Stackoffset n -> t.stack_offset <- t.stack_offset + n
  | Begin_region ->
    let local_sp_ptr = load_domainstate_addr t Domain_local_sp in
    let local_sp = emit_ins t (I.load ~ptr:local_sp_ptr ~typ:T.i64) in
    store_into_reg t i.res.(0) local_sp
  | End_region ->
    let local_sp_ptr = load_domainstate_addr t Domain_local_sp in
    let saved_local_sp = load_reg_to_temp t i.arg.(0) in
    emit_ins_no_res t (I.store ~ptr:local_sp_ptr ~to_store:saved_local_sp)
  | Alloc { bytes; dbginfo = _; mode = Local } -> local_alloc t i bytes
  | Alloc { bytes; dbginfo = _; mode = Heap } -> heap_alloc t i bytes
  | Csel test_op ->
    let typ = T.of_reg i.res.(0) in
    let len = Array.length i.arg in
    let ifso = load_reg_to_temp ~typ t i.arg.(len - 2) in
    let ifnot = load_reg_to_temp ~typ t i.arg.(len - 1) in
    let cond = test t test_op i in
    let res = emit_ins t (I.select ~cond ~ifso ~ifnot) in
    store_into_reg t i.res.(0) res
  | Static_cast cast_op -> (
    let do_conv op ~from ~to_ =
      let arg = load_reg_to_temp ~typ:from t i.arg.(0) in
      let converted = emit_ins t (I.convert op ~arg ~to_) in
      store_into_reg t i.res.(0) converted
    in
    match cast_op with
    | Float_of_int width ->
      do_conv Sitofp ~from:T.i64 ~to_:(T.of_float_width width)
    | Int_of_float width ->
      do_conv Fptosi ~from:(T.of_float_width width) ~to_:T.i64
    | Float_of_float32 -> do_conv Fpext ~from:T.float ~to_:T.double
    | Float32_of_float -> do_conv Fptrunc ~from:T.double ~to_:T.float
    | V128_of_scalar _ | Scalar_of_v128 _ | V256_of_scalar _ | Scalar_of_v256 _
    | V512_of_scalar _ | Scalar_of_v512 _ ->
      not_implemented_basic ~msg:"static cast" i)
  | Reinterpret_cast cast_op -> (
    match cast_op with
    | Int_of_value | Value_of_int | Float_of_int64 | Int64_of_float
    | Float32_of_int32 | Int32_of_float32 | Float_of_float32 | Float32_of_float
      ->
      let arg = load_reg_to_temp t i.arg.(0) in
      let converted =
        emit_ins t (I.convert Bitcast ~arg ~to_:(T.of_reg i.res.(0)))
      in
      store_into_reg t i.res.(0) converted
    | V128_of_vec _ | V256_of_vec _ | V512_of_vec _ ->
      not_implemented_basic ~msg:"vector reinterpret cast" i)
  | Specific op -> specific t i op
  | Intop_atomic { op; size; addr } -> atomic t i op ~size ~addr
  | Pause -> call_llvm_intrinsic_no_res t "x86.sse2.pause" []
  | Poll -> () (* CR yusumez: insert poll call *)
  | Spill | Reload -> not_implemented_basic ~msg:"spill / reload" i
  | Probe_is_enabled _ | Name_for_debugger _ | Dls_get ->
    not_implemented_basic i

let emit_basic t (i : Cfg.basic Cfg.instruction) =
  emit_comment t "%a" F.pp_dbg_instr_basic i;
  match i.desc with
  | Op op -> basic_op t i op
  | Prologue | Epilogue | Reloadretaddr -> () (* LLVM handles these for us *)
  | Stack_check _ ->
    if Config.no_stack_checks || not !Oxcaml_flags.cfg_stack_checks
    then () (* Don't emit stack checks *)
    else not_implemented_basic ~msg:"stack check" i
  | Poptrap { lbl_handler } -> (
    match Label.Tbl.find_opt (get_fun_info t).trap_blocks lbl_handler with
    | None -> fail_msg "unbalanced trap pop"
    | Some { trap_block; stacksave_ptr; payload = _ } ->
      (* Restore previous exn handler sp (top word on trap block) *)
      let exn_sp_ptr = load_domainstate_addr t Domain_exn_handler in
      let prev_exn_sp = emit_ins t (I.load ~ptr:trap_block ~typ:T.i64) in
      emit_ins_no_res t (I.store ~ptr:exn_sp_ptr ~to_store:prev_exn_sp);
      (* Pop! *)
      call_llvm_intrinsic_no_res t "stackrestore" [stacksave_ptr])
  | Pushtrap { lbl_handler } -> (
    (* Exception control flow is implemented in a way that emulates setjmp in C.
       Namely. we call a function that "returns twice", first falling through to
       the try block, second when an exception happens. This ensures that LLVM
       is aware of potential control flow to the handler so that it doesn't
       eliminate it as dead code or does other funny things moving blocks
       around.

       The wrapper function follows the OCaml calling conventions to handle
       spilling/reloading registers. Note that here, we assume spill locations
       are consistent across calls within the try block. We could solidify this
       assumption via using native instructions like [invoke] or [callbr].

       Due to an unfortunate series of interactions with LLVM (allocating after
       the entry block causes LLVM to force the use of frame pointers, and it
       fails to save RBP across OCaml calls when this happens), this only can
       work with frame pointers enabled.

       Because of this, trap blocks allocated by this backend are 4 words wide,
       as opposed to 2, to account for RBP (and padding for alignment). These
       two words are placed below the normal trap block so that the top two
       words are handled as expected by the runtime and functions compiled
       without the LLVM backend. Pushtrap sets this up, while they get torn down
       at trap handler entry.

       Currently, this implementation does not work for some cases since the two
       points during execution at which control flow can arrive after the dummy
       call don't agree in whether RBP needs restoring or not. So, we get cases
       where LLVM reloads stuff from the stack using RBP before it gets restored
       in the handler (since we need to do the check + jump to the handler
       first). *)
    call_simple
      ~attrs:[Returns_twice; Gc_leaf_function]
      ~cc:Ocaml t "wrap_try" [] [T.i32]
    |> ignore (* Note that we don't need the returned identifier here. *);
    (* Record label here - we will jump here for the handler *)
    let try_and_exn_entry = V.of_label (Cmm.new_label ()) in
    emit_ins_no_res t (I.br try_and_exn_entry);
    emit_label t try_and_exn_entry;
    (* Extract the result of the call, or the exception payload. *)
    let payload =
      emit_ins t
        (I.inline_asm ~asm:"mov %rax, $0" ~constraints:"=r" ~args:[]
           ~res_type:(Some T.i64) ~sideeffect:true)
    in
    (* If it's nonzero, we have an exception. Otherwise, go to the try block. *)
    let payload_is_zero =
      emit_ins t (I.icmp Ieq ~arg1:payload ~arg2:(V.of_int 0))
    in
    let try_label = V.of_label (Cmm.new_label ()) in
    let exn_label = V.of_label lbl_handler in
    emit_ins_no_res t
      (I.br_cond ~cond:payload_is_zero ~ifso:try_label ~ifnot:exn_label);
    (* Enter try block from this point onwards. *)
    emit_label t try_label;
    (* Save state of stack *)
    let stacksave_ptr = call_llvm_intrinsic t "stacksave" [] T.ptr in
    (* Allocate trap block on stack. It will get allocated at the top of the
       stack. The layout is [prev_sp; handler_addr; saved_rbp; padding] *)
    let trap_block = emit_ins t (I.alloca T.(Struct [i64; i64; i64; i64])) in
    (* Slots on the trap block *)
    let rbp_slot = do_offset t trap_block T.ptr 16 in
    let handler_slot = do_offset t trap_block T.ptr 8 in
    let prev_sp_slot = do_offset t trap_block T.ptr 0 in
    (* Push my trap block to the exn handler list *)
    let exn_sp_ptr = load_domainstate_addr t Domain_exn_handler in
    let prev_exn_sp = emit_ins t (I.load ~ptr:exn_sp_ptr ~typ:T.i64) in
    emit_ins_no_res t (I.store ~ptr:exn_sp_ptr ~to_store:trap_block);
    (* Fill up the slots *)
    emit_ins_no_res t
      (I.store ~ptr:handler_slot
         ~to_store:
           (V.blockaddress
              ~func:(E.get_fun_ident (get_fun_info t).emitter)
              ~block:(V.get_ident_exn try_and_exn_entry)));
    emit_ins_no_res t
      (I.inline_asm ~asm:"mov %rbp, ($0)" ~constraints:"r" ~args:[rbp_slot]
         ~res_type:T.Or_void.void ~sideeffect:true);
    emit_ins_no_res t (I.store ~ptr:prev_sp_slot ~to_store:prev_exn_sp);
    (* Save the trap block in [t] *)
    match Label.Tbl.find_opt (get_fun_info t).trap_blocks lbl_handler with
    | Some _ -> fail_msg "multiple pushtraps for the same handler"
    | None ->
      Label.Tbl.add (get_fun_info t).trap_blocks lbl_handler
        { trap_block; stacksave_ptr; payload })

(* Cfg translation entry *)

let collect_body_regs cfg =
  Cfg.fold_blocks cfg
    ~f:(fun _ block regs ->
      let body_regs =
        DLL.fold_left block.body
          ~f:(fun regs (instr : Cfg.basic Cfg.instruction) ->
            Reg.add_set_array regs (Array.append instr.res instr.arg))
          ~init:Reg.Set.empty
      in
      let terminator_regs =
        Array.append block.terminator.res block.terminator.arg
      in
      Reg.add_set_array body_regs terminator_regs |> Reg.Set.union regs)
    ~init:Reg.Set.empty

let reg_listed_in_signature (reg : Reg.t) =
  match reg.loc with
  | Reg _ -> true
  | Stack _ -> false
  | Unknown -> fail "reg_listed_in_signature"

let fun_attrs ~fun_has_try codegen_options =
  let exn_attrs : LL.Fn_attr.t list = if fun_has_try then [Noinline] else [] in
  let codegen_attrs =
    List.concat_map
      (fun opt : LL.Fn_attr.t list ->
        match (opt : Cfg.codegen_option) with
        | Cfg.Cold -> [Cold; Noinline]
        | Reduce_code_size | No_CSE | Use_linscan_regalloc | Use_regalloc _
        | Use_regalloc_param _ | Assume_zero_alloc _ | Check_zero_alloc _ ->
          [] (* CR gyorsh: translate and communicate to llvm backend *))
      codegen_options
  in
  exn_attrs @ codegen_attrs |> List.sort_uniq LL.Fn_attr.compare

(* Returns argument registers listed in the signature *)
let prepare_fun_info t (cfg : Cfg.t) =
  let { Cfg.blocks = _;
        entry_label = _;
        fun_name;
        fun_args;
        fun_codegen_options;
        fun_dbg;
        fun_contains_calls = _ (* not used at this point *);
        fun_num_stack_slots = _ (* only available after regalloc *);
        fun_poll = _ (* not needed after poll insertion *);
        next_instruction_id = _;
        fun_ret_type
      } =
    cfg
  in
  let fun_has_try =
    Cfg.fold_blocks cfg
      ~f:(fun _ block acc -> acc || block.is_trap_handler)
      ~init:false
  in
  let arg_regs =
    Array.to_list fun_args |> List.filter reg_listed_in_signature
  in
  let arg_types = List.map LL.Type.of_reg arg_regs |> make_arg_types in
  let res_type = make_ret_type_of_machtype fun_ret_type in
  let attrs = fun_attrs ~fun_has_try fun_codegen_options in
  let emitter =
    LL.Function.Emitter.create ~name:fun_name ~args:arg_types
      ~res:(Some res_type) ~cc:Ocaml ~attrs ~gc:(Some gc_name) ~dbg:fun_dbg
      ~private_:false
  in
  reset_fun_info t emitter;
  arg_regs

(* Emits [alloca]'s in the entry block for all [Reg.t]s in [cfg] and runtime
   registers and prepares the [t.reg2alloca] table. [arg_values] are all values
   listed in the signature in LLVM IR (including runtime registers). [arg_regs]
   are the [Reg.t]s corresponding to those present in the argument list.

   Our [Cfg.t] is not always in SSA form, and pinned registers also break SSA.
   Because of this, all [Reg.t]s and runtime registere are accessed through
   loads and stores to pointers returned by [alloca] instructions. These will
   then get optimised by LLVM as part of the mem2reg pass.

   Note that arguments not passed in registers (e.g. in Domainstate) will point
   to that block of memory instead of being allocated on the stack for
   uniformity. *)
let alloca_regs t (cfg : Cfg.t) arg_values arg_regs =
  let alloca_runtime_reg ~runtime_reg_ident ~arg_value =
    let alloca'ed =
      emit_ins ~res_ident:runtime_reg_ident t (I.alloca (V.get_type arg_value))
    in
    emit_ins_no_res t (I.store ~ptr:alloca'ed ~to_store:arg_value)
  in
  let alloca_reg ?init (reg : Reg.t) =
    match reg.loc with
    (* [Outgoing] is for extcalls only - these will get put on the stack by
       LLVM, so we treat them as normal temporaries. We don't expect OCaml calls
       to use the stack for us... *)
    | Unknown | Reg _ | Stack (Outgoing _) -> (
      let alloca'd = emit_ins t (I.alloca (T.of_reg reg)) in
      set_alloca_for_reg t reg alloca'd;
      match init with
      | None -> () (* Don't initialise *)
      | Some value -> store_into_reg t reg value)
    | Stack (Domainstate idx) ->
      (* Compute pointer to where [reg] is located - we can assume [ds] will not
         change through the run of this function *)
      let ds_loc = load_domainstate_addr ~offset:idx t Domain_extra_params in
      set_alloca_for_reg t reg ds_loc
    | Stack (Local _) | Stack (Incoming _) ->
      fail_msg ~name:"alloca_regs" "unexpected register location"
  in
  (* First handle values passed from the arguments *)
  let open struct
    type arg =
      | Register of Reg.t
      | Runtime_ident of LL.Ident.t
  end in
  (* Runtime registers come first (since the rest might reference them) *)
  let args =
    List.map (fun ident -> Runtime_ident ident) runtime_reg_idents
    @ List.map (fun reg -> Register reg) arg_regs
  in
  (try
     List.iter2
       (fun arg_reg arg_value ->
         match arg_reg with
         | Register reg -> alloca_reg ~init:arg_value reg
         | Runtime_ident ident ->
           alloca_runtime_reg ~runtime_reg_ident:ident ~arg_value)
       args arg_values
   with _ ->
     fail_msg ~name:"alloca_regs" "argument count mismatch %d %d"
       (List.length args) (List.length arg_values));
  (* Handle remaining registers in the body *)
  let body_regs =
    let all_body_regs = collect_body_regs cfg in
    Reg.Set.diff all_body_regs (Reg.Set.of_list arg_regs)
  in
  Reg.Set.iter (fun reg -> alloca_reg reg) body_regs;
  (* Jump to entry block *)
  emit_ins_no_res t (I.br (V.of_label cfg.entry_label))

let trap_handler_entry t (block : Cfg.basic_block) label =
  match[@ocaml.warning "-fragile-match"]
    DLL.hd block.body |> Option.map (fun (i : _ Cfg.instruction) -> i, i.desc)
  with
  | Some (i, Op Move) -> (
    match Label.Tbl.find_opt (get_fun_info t).trap_blocks label with
    | Some { payload; _ } ->
      (* Restore RBP (+ remove padding) *)
      emit_ins_no_res t
        (I.inline_asm ~asm:"pop %rbp; addq $$8, %rsp" ~constraints:"" ~args:[]
           ~res_type:T.Or_void.void ~sideeffect:true);
      (* Restore allocation pointer *)
      let new_alloc_ptr =
        emit_ins t
          (I.inline_asm ~asm:"movq %r15, $0" ~constraints:"=r" ~args:[]
             ~res_type:(Some T.i64) ~sideeffect:true)
      in
      emit_ins_no_res t (I.store ~ptr:allocation_ptr ~to_store:new_alloc_ptr);
      (* Move payload to appropriate temp *)
      store_into_reg t i.arg.(0) payload
    | None -> ())
  | _ ->
    fail_msg ~name:"trap_handler_entry"
      "first instruction of trap handler not a move"

let emit_block t (cfg : Cfg.t) label =
  let block = Label.Tbl.find cfg.blocks label in
  emit_label t (V.of_label label);
  if block.is_trap_handler then trap_handler_entry t block label;
  DLL.iter ~f:(emit_basic t) block.body;
  emit_terminator t block.terminator

let cfg (cl : CL.t) =
  let t = get_current_compilation_unit "cfg" in
  let layout = CL.layout cl in
  let cfg = CL.cfg cl in
  reject_addr_regs cfg.fun_args "fun args";
  let arg_regs = prepare_fun_info t cfg in
  let arg_values =
    LL.Function.Emitter.get_args_as_values (get_fun_info t).emitter
  in
  alloca_regs t cfg arg_values arg_regs;
  DLL.iter ~f:(emit_block t cfg) layout;
  add_defined_symbol t cfg.fun_name;
  complete_func_def t

(* Data declarations *)

(* CR yusumez: We make some assumptions about the structure of the data items we
   receive and decode it manually here. Ideally, [data_item]s would be
   represented in a more structured manner, but this should do for now. *)
let make_temp_data_symbol =
  let idx = ref 0 in
  fun () ->
    let module_name =
      Compilation_unit.(get_current_or_dummy () |> name |> Name.to_string)
    in
    let res = Format.asprintf "temp.%s.%d" module_name !idx in
    incr idx;
    res

let llvm_value_of_data_item (d : Cmm.data_item) =
  match d with
  | Cdefine_symbol _ -> fail_msg ~name:"llvm_value_of_data_item" "define_symbol"
  | Calign _ | Csymbol_offset _ ->
    (* [Calign] and [Csymbol_offset] are never produced *)
    fail_msg ~name:"llvm_value_of_data_item" "unexpected data item"
  | Cint n -> V.of_nativeint ~typ:T.i64 n
  | Cint8 n -> V.of_int ~typ:T.i8 n
  | Cint16 n -> V.of_int ~typ:T.i16 n
  | Cint32 n -> V.of_nativeint ~typ:T.i32 n
  | Csymbol_address { sym_name; sym_global = _ } -> V.of_symbol sym_name
  | Cstring s -> V.of_string_constant s
  | Cskip size -> V.zeroinitializer (T.Array { size; elem_type = T.i8 })
  | Csingle f -> V.of_float ~typ:T.float f
  | Cdouble f -> V.of_float ~typ:T.double f
  | Cvec128 _ | Cvec256 _ | Cvec512 _ ->
    fail_msg ~name:"llvm_value_of_data_item" "vector"

let define_symbol t ~private_ ~header ~symbol (contents : Cmm.data_item list) =
  let symbol =
    match symbol with None -> make_temp_data_symbol () | Some symbol -> symbol
  in
  (match header with
  | None -> ()
  | Some header ->
    let header_sym = "header." ^ symbol in
    add_data_def t
      (LL.Data.constant ~private_:true header_sym
         (V.of_nativeint ~typ:T.i64 header)));
  let value = V.struct_constant (List.map llvm_value_of_data_item contents) in
  add_data_def t (LL.Data.constant ~private_ symbol value);
  add_defined_symbol t symbol;
  List.iter
    (fun (d : Cmm.data_item) ->
      match[@warning "-fragile-match"] d with
      | Csymbol_address { sym_name; sym_global = _ } ->
        add_referenced_symbol t sym_name
      | _ -> ())
    contents

let data (ds : Cmm.data_item list) =
  let t = get_current_compilation_unit "data" in
  let define_symbol ~private_ ~header ~symbol contents =
    if private_ && List.is_empty contents
    then () (* No need to declare a private symbol with no contents *)
    else define_symbol t ~private_ ~header ~symbol contents
  in
  let peek_int = function[@warning "-4"] Cmm.Cint n -> Some n | _ -> None in
  let peek_define_symbol = function[@warning "-4"]
    | Cmm.Cdefine_symbol { sym_name; sym_global = _ } -> Some sym_name
    | _ -> None
  in
  let eat_if peek = function
    | [] -> None
    | d :: ds -> peek d |> Option.map (fun i -> i, ds)
  in
  let closure_block ds =
    let function_slot ds =
      let header, ds = eat_if peek_int ds |> Option.get in
      let symbol, ds = eat_if peek_define_symbol ds |> Option.get in
      (*= A function slot is either:
          | code pointer | closinfo | (if the function has arity 0 or 1)
          | code pointer | closinfo | second code pointer | (arity >= 2)
          See mlvalues.h for details. *)
      let closinfo = List.nth ds 1 |> peek_int |> Option.get in
      let arity = Nativeint.shift_right_logical closinfo 56 in
      let slot_size = if arity <= 1n then 2 else 3 in
      let slot, tail = List.split_at slot_size ds in
      define_symbol ~private_:false ~header:(Some header) ~symbol:(Some symbol)
        slot;
      let is_last =
        Nativeint.(shift_right_logical (shift_left closinfo 8) (size - 1) = 1n)
      in
      tail, is_last
    in
    let rec iter_slots ds =
      let tail, is_last = function_slot ds in
      if is_last
      then define_symbol ~private_:true ~header:None ~symbol:None tail
      else iter_slots tail
    in
    iter_slots ds
  in
  let block ds =
    match eat_if peek_int ds with
    | Some (i, after_i) -> (
      match eat_if peek_define_symbol after_i with
      | Some (symbol, after_symbol) ->
        (* [i] is a header *)
        if Nativeint.(logand i 0xffn = of_int Obj.closure_tag)
        then closure_block ds
        else
          define_symbol ~private_:false ~header:(Some i) ~symbol:(Some symbol)
            after_symbol
      | None ->
        (* [i] is not a header *)
        define_symbol ~private_:true ~header:None ~symbol:None ds)
    | None -> (
      (* No header *)
      match eat_if peek_define_symbol ds with
      | Some (symbol, after_symbol) ->
        define_symbol ~private_:false ~header:None ~symbol:(Some symbol)
          after_symbol
      | None -> define_symbol ~private_:true ~header:None ~symbol:None ds)
  in
  try block ds
  with _ ->
    fail_msg ~name:"data" "error while decoding data items: %a" Printcmm.data ds

(* Wrapping up loose ends *)

let define_c_call_wrappers t =
  String.Map.iter
    (fun wrapper_name { c_fun_name; args = c_arg_types; res = c_res_types } ->
      let wrapper_res_type = make_ret_type c_res_types in
      let wrapper_arg_types = make_arg_types c_arg_types in
      let emitter =
        LL.Function.Emitter.create ~name:wrapper_name ~args:wrapper_arg_types
          ~res:(Some wrapper_res_type) ~cc:Ocaml ~attrs:[Noinline] ~gc:None
          ~dbg:Debuginfo.none ~private_:true
      in
      reset_fun_info t emitter;
      let runtime_args, c_fun_args =
        List.split_at (List.length runtime_regs)
          (LL.Function.Emitter.get_args_as_values emitter)
      in
      let ds = List.nth runtime_args domainstate_idx in
      let c_sp =
        let c_sp_ptr = load_domainstate_addr ~ds_loc:ds t Domain_c_stack in
        emit_ins t (I.load ~ptr:c_sp_ptr ~typ:T.i64)
      in
      let ocaml_sp = read_rsp t in
      write_rsp t c_sp;
      let c_res =
        emit_ins t
          (I.call
             ~func:(V.of_symbol c_fun_name |> V.get_ident_exn)
             ~args:c_fun_args ~res_type:(Some (T.Struct c_res_types)) ~attrs:[]
             ~cc:Default ~musttail:false)
      in
      write_rsp t ocaml_sp;
      let wrapper_res =
        assemble_struct t wrapper_res_type
          (([1], c_res) :: List.mapi (fun i v -> [0; i], v) runtime_args)
      in
      emit_ins_no_res t (I.ret wrapper_res);
      add_defined_symbol t wrapper_name;
      complete_func_def t)
    t.c_call_wrappers

let define_wrap_try t =
  let arg_types = make_arg_types [] in
  let res_type = make_ret_type [T.i32] in
  let emitter =
    LL.Function.Emitter.create ~name:"wrap_try" ~args:arg_types
      ~res:(Some res_type) ~cc:Ocaml ~attrs:[Returns_twice; Noinline] ~gc:None
      ~dbg:Debuginfo.none ~private_:true
  in
  reset_fun_info t emitter;
  let runtime_args =
    LL.Function.Emitter.get_args_as_values emitter
    (* All are runtime regs *)
  in
  let try_res = call_llvm_intrinsic t "eh.ocaml.try" [] T.i32 in
  let res =
    assemble_struct t res_type
      (([1; 0], try_res) :: List.mapi (fun i v -> [0; i], v) runtime_args)
  in
  emit_ins_no_res t (I.ret res);
  complete_func_def t

let define_empty_function t name =
  let emitter =
    LL.Function.Emitter.create ~name ~args:[] ~res:T.Or_void.void ~cc:Default
      ~attrs:[] ~gc:None ~dbg:Debuginfo.none ~private_:false
  in
  reset_fun_info t emitter;
  emit_ins_no_res t I.unreachable;
  complete_func_def t

let define_empty_symbol t symbol =
  define_symbol t ~private_:false ~header:None ~symbol:(Some symbol) []

(* Declare menitoned but not declared data items as extern *)
let declare_data t =
  String.Set.diff t.referenced_symbols t.defined_symbols
  |> String.Set.iter (fun sym -> add_data_def t (LL.Data.external_ sym))

let define_auxiliary_functions t =
  define_c_call_wrappers t;
  define_wrap_try t

(* Interface with the rest of the compiler *)

(* Create LLVM IR file for the current compilation unit. *)
let init ~output_prefix ~ppf_dump =
  let llvmir_filename = output_prefix ^ ".ll" in
  current_compilation_unit := Some (create ~llvmir_filename ~ppf_dump)

let close_out () =
  match !current_compilation_unit with
  | None -> ()
  | Some t ->
    (* Exception raised during llvmize, keep .ll file. *)
    Out_channel.close t.oc;
    current_compilation_unit := None

let open_out ~asm_filename =
  let t = get_current_compilation_unit "open_out" in
  t.asm_filename <- Some asm_filename

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename
  with Sys_error _msg -> ()

let invoke_clang_with_llvmir ~output_filename ~input_filename ~extra_flags =
  (* CR-someday gyorsh: add other optimization flags and control which passes to
     perform. *)
  let cmd =
    match !Oxcaml_flags.llvm_path with Some path -> path | None -> Config.asm
  in
  let fp_flags =
    if Config.with_frame_pointers
    then ["-fno-omit-frame-pointer"]
    else ["-fomit-frame-pointer"; "-momit-leaf-frame-pointer"]
  in
  let llvm_flags = [!Oxcaml_flags.llvm_flags] in
  Ccomp.command
    (String.concat " "
       ([cmd]
       @ ["-o"; Filename.quote output_filename]
       @ ["-x ir"; Filename.quote input_filename]
       @ ["-O3"; "-S"; "-Wno-override-module"]
       @ fp_flags @ llvm_flags @ extra_flags))

let llvmir_to_assembly t =
  match t.asm_filename with
  | None -> 0
  | Some asm_filename ->
    invoke_clang_with_llvmir ~input_filename:t.llvmir_filename
      ~output_filename:asm_filename ~extra_flags:[]

let dump_llvmir ~llvmir_filename ~message t =
  let ic = In_channel.open_text llvmir_filename in
  let contents = In_channel.input_all ic in
  Format.fprintf t.ppf_dump "\n*** %s\n\n%s" message contents;
  In_channel.close ic

let dump_llvmir_after_llvmize t =
  dump_llvmir ~llvmir_filename:t.llvmir_filename ~message:"After llvmize" t

let dump_llvmir_after_opt t =
  let opt_llvmir_filename = t.llvmir_filename ^ ".opt.ll" in
  let _cmd_ret =
    invoke_clang_with_llvmir ~input_filename:t.llvmir_filename
      ~output_filename:opt_llvmir_filename ~extra_flags:["-emit-llvm"]
  in
  dump_llvmir ~llvmir_filename:opt_llvmir_filename ~message:"After llopt" t;
  remove_file opt_llvmir_filename

let assemble_file ~asm_filename ~obj_filename =
  let cmd =
    match !Oxcaml_flags.llvm_path with Some path -> path | None -> Config.asm
  in
  Ccomp.command
    (String.concat " "
       [ cmd;
         "-c";
         "-g";
         "-o";
         Filename.quote obj_filename;
         Filename.quote asm_filename ])

(* CR yusumez: [begin_assembly] and [end_assembly] emit extra things to the .ll
   file, so they always need to be called. However, this will still generate an
   assembly file if -stop-after simplify_cfg or -stop_after linearization are
   passed, which it shouldn't do. *)

let begin_assembly ~sourcefile =
  let t = get_current_compilation_unit "begin_asm" in
  t.sourcefile <- sourcefile;
  define_empty_function t (Cmm_helpers.make_symbol "code_begin");
  define_empty_symbol t (Cmm_helpers.make_symbol "data_begin")

let write_llvmir_to_file t =
  (match t.sourcefile with
  | Some sourcefile -> F.pp_line t.ppf "source_filename = \"%s\"\n" sourcefile
  | None -> ());
  List.iter (LL.Function.pp_t t.ppf) (List.rev t.function_defs);
  List.iter (LL.Data.pp_t t.ppf) (List.rev t.data_defs);
  F.pp_line t.ppf "";
  String.Map.iter
    (fun _ fundecl -> LL.Fundecl.pp_t t.ppf fundecl)
    t.called_intrinsics

let end_assembly () =
  let t = get_current_compilation_unit "end_asm" in
  define_auxiliary_functions t;
  declare_data t;
  define_empty_function t (Cmm_helpers.make_symbol "code_end");
  define_empty_symbol t (Cmm_helpers.make_symbol "data_end");
  write_llvmir_to_file t;
  Out_channel.close t.oc;
  (* Dump if -dllvmir passed *)
  if !Oxcaml_flags.dump_llvmir
  then (
    dump_llvmir_after_llvmize t;
    dump_llvmir_after_opt t);
  (* Call clang to compile .ll to .s *)
  let ret_code = llvmir_to_assembly t in
  if ret_code <> 0
  then
    raise
      (Error
         (Asm_generation
            ( Option.value ~default:"(no source file specified)" t.sourcefile,
              ret_code )));
  if not !Oxcaml_flags.keep_llvmir then remove_file t.llvmir_filename;
  current_compilation_unit := None

(* CR-someday gyorsh: currently, llvm backend can be selected at the compilation
   unit granularity only. It could be controlled at the function granularity. *)
(* CR-someday gyorsh: Compiling directly to .o would involve more changes to
   [Asmgen], [Asmlink], and drivers. It would improve compilation speed but not
   as much as avoiding the textual representation entirely by linking in the
   llvm library statically. *)
(* CR-someday gyorsh: we could set [binary_backend_available] but it is
   currently too tightly coupled with the [internal_assembler], especially in
   [asmlink] for shared libraries. *)
(* CR gyorsh: how to emit data_begin/end and code_begin/end symbol? Do we still
   need both of them with ocaml ?

   yusumez: For now, we are just emitting them as global constants.
   code_begin/end are empty function decl's so that they end up in the proper
   section. However, we currently don't have control over where they end up in
   the asm file. *)
(* CR gyorsh: assume 64-bit architecture *)
(* CR yusumez: We ignore whether symbols are local/global. *)
(* CR yusumez: Move all arch-specific things to an arch-specific file. *)
(* CR yusumez: The structure of this file needs to be completely refactored:

   - Separate IR generation from printing/emitting

   - Make working with identifiers easy (make instructions with results return
   the fresh identifier themselves, carry their types around, etc.)

   - Factor out small common operations in a better way (eg. for function
   calls) *)
(* CR yusumez: Exceptions might not work with statepoints since LLVM doesn't
   seem to be able to statically determine the size of the stack in that
   case. *)

(* Error report *)

let report_error ppf = function
  | Asm_generation (fn, ret_code) ->
    Format.fprintf ppf "Error producing assembly code for %s: %d" fn ret_code

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
