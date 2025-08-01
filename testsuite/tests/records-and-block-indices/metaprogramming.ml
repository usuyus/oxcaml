open Stdlib_upstream_compatible
open Stdlib_stable
module String = StringLabels
module Int_map = Map.Make (Int)

module List : sig
  include module type of ListLabels

  val fold_left_mapi :
    f:('a -> int -> 'b -> 'a * 'c) -> init:'a -> 'b list -> 'a * 'c list

  val fold_lefti : f:('a -> int -> 'b -> 'a) -> init:'a -> 'b list -> 'a
end = struct
  include ListLabels

  let enumerate l = mapi ~f:(fun i x -> i, x) l

  let fold_left_mapi ~f ~init l =
    enumerate l |> fold_left_map ~f:(fun acc (i, x) -> f acc i x) ~init

  let fold_lefti ~f ~init l =
    enumerate l |> fold_left ~f:(fun acc (i, x) -> f acc i x) ~init
end

let sprintf = Printf.sprintf

module Boxing = struct
  type t =
    | Boxed
    | Unboxed

  let to_string = function Boxed -> "" | Unboxed -> "#"
end

module Layout = struct
  type t =
    | Product of t list
    | Value of
        { ignorable : bool;
          non_float : bool
        }
    | Float64
    | Float32
    | Bits64
    | Bits32
    | Vec128
    | Word
    | Void

  let rec all_scannable t =
    match t with
    | Value _ -> true
    | Void -> true
    | Bits32 | Bits64 | Float64 | Float32 | Vec128 | Word -> false
    | Product ts -> List.for_all ts ~f:all_scannable

  let rec all_ignorable t =
    match t with
    | Value { ignorable; _ } -> ignorable
    | Float64 | Float32 | Bits64 | Bits32 | Vec128 | Word -> true
    | Void -> true
    | Product ts -> List.for_all ts ~f:all_ignorable

  let rec contains_vec128 t =
    match t with
    | Value _ | Float64 | Float32 | Bits64 | Bits32 | Word | Void -> false
    | Vec128 -> true
    | Product ts -> List.exists ts ~f:contains_vec128

  let rec contains_void t =
    match t with
    | Value _ | Float64 | Float32 | Bits64 | Bits32 | Word | Vec128 -> false
    | Void -> true
    | Product ts -> List.exists ts ~f:contains_void

  let is_product t = match t with Product _ -> true | _ -> false

  let rec is_non_float t =
    match t with
    | Value { non_float; _ } -> non_float
    | Bits32 | Bits64 | Float64 | Float32 | Vec128 | Word | Void -> true
    | Product ts -> List.for_all ts ~f:is_non_float

  let rec all_void t =
    match t with
    | Void -> true
    | Product ts -> List.for_all ts ~f:all_void
    | Value _ | Bits32 | Bits64 | Float64 | Float32 | Vec128 | Word -> false

  type acc =
    { seen_flat : bool;
      last_value_after_flat : bool
    }

  let reordered_in_block t =
    let rec aux t acc =
      match t with
      | Value _ -> { acc with last_value_after_flat = acc.seen_flat }
      | Float64 | Float32 | Bits64 | Bits32 | Vec128 | Word ->
        { acc with seen_flat = true }
      | Void -> acc
      | Product ts ->
        List.fold_left ts ~init:acc ~f:(fun acc layout -> aux layout acc)
    in
    (aux t { seen_flat = false; last_value_after_flat = false })
      .last_value_after_flat
end

module Tree = struct
  type 'a t =
    | Branch of 'a t list
    | Leaf of 'a

  module By_leaves_and_singleton_branches = struct
    let rec enumerate_forest_shapes ~cost ~min_num_trees : unit t list list =
      assert (cost >= min_num_trees);
      if Int.equal cost 0
      then [[]]
      else
        let possible_first_tree_cost =
          (* Two upper bounds on number of nodes for the first tree: *)
          (* [cost - min_num_trees + 1], because after generating the first
             tree, we need (min_num_trees - 1) left over to generate the rest *)
          (* [cost] *)
          List.init
            ~len:(Int.min (cost - min_num_trees + 1) cost)
            ~f:(fun x -> x + 1)
        in
        List.concat_map possible_first_tree_cost ~f:(fun first_tree_cost ->
            List.concat_map (enumerate_shapes ~cost:first_tree_cost)
              ~f:(fun first_tree ->
                assert (cost - first_tree_cost >= min_num_trees - 1);
                let cost = cost - first_tree_cost in
                let min_num_trees = Int.max 0 (min_num_trees - 1) in
                assert (cost >= min_num_trees);
                List.map (enumerate_forest_shapes ~cost ~min_num_trees)
                  ~f:(fun rest_trees -> first_tree :: rest_trees
                )
            )
        )

    and enumerate_shapes ~cost : unit t list =
      assert (cost >= 1);
      if Int.equal cost 1
      then [Leaf ()]
      else
        (* a branch costs 1 node only if it has a single child *)
        List.map
          (enumerate_shapes ~cost:(cost - 1))
          ~f:(fun shape -> Branch [shape])
        @ List.map (enumerate_forest_shapes ~cost ~min_num_trees:2)
            ~f:(fun forest -> Branch forest
          )
  end

  module By_num_nodes = struct
    let rec enumerate_forest_shapes num_nodes : unit t list list =
      assert (num_nodes > 0);
      let possible_first_tree_num_nodes =
        List.init ~len:num_nodes ~f:(fun x -> x + 1)
      in
      List.concat_map possible_first_tree_num_nodes
        ~f:(fun first_tree_num_nodes ->
          List.concat_map (enumerate_shapes ~num_nodes:first_tree_num_nodes)
            ~f:(fun first_tree ->
              let rest_num_nodes = num_nodes - first_tree_num_nodes in
              assert (rest_num_nodes >= 0);
              if Int.equal rest_num_nodes 0
              then [[first_tree]]
              else
                List.map (enumerate_forest_shapes rest_num_nodes)
                  ~f:(fun rest_trees -> first_tree :: rest_trees
                )
          )
      )

    and enumerate_shapes ~num_nodes : unit t list =
      assert (num_nodes >= 1);
      if Int.equal num_nodes 1
      then [Leaf ()]
      else
        List.map
          (enumerate_forest_shapes (num_nodes - 1))
          ~f:(fun forest -> Branch forest)
  end

  let enumerate_shapes ~max_num_nodes =
    let num_nodes_list = List.init ~f:(fun i -> i + 1) ~len:max_num_nodes in
    List.concat_map num_nodes_list ~f:(fun num_nodes ->
        By_num_nodes.enumerate_shapes ~num_nodes
    )

  let enumerate_shapes' ~max_leaves_and_singleton_branches =
    let costs =
      List.init ~f:(fun i -> i + 1) ~len:max_leaves_and_singleton_branches
    in
    List.concat_map costs ~f:(fun cost ->
        By_leaves_and_singleton_branches.enumerate_shapes ~cost
    )

  let rec compare f t1 t2 =
    match t1, t2 with
    | Leaf _, Branch _ -> -1
    | Branch _, Leaf _ -> 1
    | Leaf x1, Leaf x2 -> f x1 x2
    | Branch l1, Branch l2 -> List.compare ~cmp:(compare f) l1 l2

  let rec exists ~f = function
    | Leaf x -> f x
    | Branch forest -> List.exists ~f:(exists ~f) forest

  let rec enumerate ~(shape : unit t) ~(leaves : 'a list) : 'a t list =
    match shape with
    | Leaf () -> List.map leaves ~f:(fun x -> Leaf x)
    | Branch forest ->
      List.map (enumerate_forests forest leaves) ~f:(fun forest -> Branch forest)

  and enumerate_forests (forest : unit t list) (leaves : 'a list) :
      'a t list list =
    match forest with
    | [] -> [[]]
    | tree :: forest ->
      let trees = enumerate ~shape:tree ~leaves in
      let forests = enumerate_forests forest leaves in
      List.concat_map trees ~f:(fun tree ->
          List.map forests ~f:(fun forest -> tree :: forest)
      )

  let rec to_string f t =
    match t with
    | Leaf x -> f x
    | Branch forest ->
      sprintf "(%s)" (String.concat ~sep:"" (List.map forest ~f:(to_string f)))
end

module Type_structure = struct
  type t =
    | Record of t list * Boxing.t
    | Variant of t list list
    | Tuple of t list * Boxing.t
    | Option of t
    | Int
    | Int64
    | Int64_u
    | Int32
    | Int32_u
    | Nativeint
    | Nativeint_u
    | Unit_u
    | Float
    | Float_u
    | Float32
    | Float32_u
    | String
    | Int64x2_u

  let rec scrape t = match t with Record ([t], Unboxed) -> scrape t | _ -> t

  let compare : t -> t -> int = Stdlib.compare

  let rec layout t : Layout.t =
    match t with
    | Record ([t], Unboxed) -> layout t
    | Record (ts, Unboxed) | Tuple (ts, Unboxed) ->
      Product (List.map ts ~f:layout)
    | Record (_, Boxed)
    | Tuple (_, Boxed)
    | Option _ | String | Int64 | Nativeint | Float32 | Int32 ->
      Value { ignorable = false; non_float = true }
    | Variant constructors ->
      let constructor_is_immediate args =
        List.for_all args ~f:(fun t -> Layout.all_void (layout t))
      in
      Value
        { ignorable = List.for_all constructors ~f:constructor_is_immediate;
          non_float = true
        }
    | Int -> Value { ignorable = true; non_float = true }
    | Float -> Value { ignorable = false; non_float = false }
    | Int64_u -> Bits64
    | Int32_u -> Bits32
    | Float32_u -> Float32
    | Float_u -> Float64
    | Nativeint_u -> Word
    | Unit_u -> Void
    | Int64x2_u -> Vec128

  let is_flat_float_record t =
    match t with
    | Record (ts, Boxed) ->
      List.for_all ts ~f:(fun t -> layout t = Float64 || scrape t = Float)
    | _ -> false

  let rec contains_vec128 t =
    match t with
    | Record (ts, _) | Tuple (ts, _) -> List.exists ts ~f:contains_vec128
    | Variant constructors ->
      List.exists constructors ~f:(fun ts -> List.exists ts ~f:contains_vec128)
    | Option t -> contains_vec128 t
    | String | Int64 | Nativeint | Float32 | Int32 | Int | Float | Int64_u
    | Float_u | Int32_u | Float32_u | Nativeint_u | Unit_u ->
      false
    | Int64x2_u -> true

  let rec contains_unit_u t =
    match t with
    | Record (ts, _) | Tuple (ts, _) -> List.exists ts ~f:contains_unit_u
    | Variant constructors ->
      List.exists constructors ~f:(fun ts -> List.exists ts ~f:contains_unit_u)
    | Option t -> contains_unit_u t
    | String | Int64 | Nativeint | Float32 | Int32 | Int | Float | Int64_u
    | Float_u | Int32_u | Float32_u | Nativeint_u | Int64x2_u ->
      false
    | Unit_u -> true

  let rec nested_unboxed_record (tree : t Tree.t) : t =
    match tree with
    | Leaf t -> t
    | Branch trees -> Record (List.map trees ~f:nested_unboxed_record, Unboxed)

  let rec boxed_record_containing_unboxed_records (tree : t Tree.t) : t option =
    match tree with
    | Leaf _ -> None
    | Branch trees ->
      let fields = List.map trees ~f:nested_unboxed_record in
      if List.for_all fields ~f:(fun t -> Layout.all_void (layout t))
      then (* all-void records not supported *)
        None
      else Some (Record (fields, Boxed))

  let rec to_string (t : t) : string =
    match t with
    | Record (ts, boxing) ->
      sprintf "%s{ %s }" (Boxing.to_string boxing)
        (List.map ts ~f:to_string |> String.concat ~sep:"; ")
    | Tuple (ts, boxing) ->
      sprintf "%s(%s)" (Boxing.to_string boxing)
        (List.map ts ~f:to_string |> String.concat ~sep:", ")
    | Variant constructors ->
      let constructor_to_string ts =
        List.map ts ~f:to_string |> String.concat ~sep:" * "
      in
      "(| "
      ^ (List.map constructors ~f:constructor_to_string
        |> String.concat ~sep:" | "
        )
      ^ ")"
    | Option t -> sprintf "%s option" (to_string t)
    | String -> "string"
    | Int64 -> "int64"
    | Nativeint -> "nativeint"
    | Float32 -> "float32"
    | Int32 -> "int32"
    | Int -> "int"
    | Float -> "float"
    | Int64_u -> "int64#"
    | Int32_u -> "int32#"
    | Nativeint_u -> "nativeint#"
    | Unit_u -> "unit_u"
    | Float_u -> "float#"
    | Float32_u -> "float32#"
    | Int64x2_u -> "int64x2#"

  let size (t : t) ~bytecode =
    let rec layout_size_in_block (layout : Layout.t) =
      match layout with
      | Value _ | Float64 | Float32 | Bits64 | Bits32 | Word -> 1
      | Vec128 -> 2
      | Void -> 0
      | Product layouts ->
        List.fold_left
          ~f:(fun acc l -> acc + layout_size_in_block l)
          ~init:0 layouts
    in
    match t with
    | Record (ts, Boxed) ->
      if bytecode
      then List.length ts
      else
        List.fold_left ts ~init:0 ~f:(fun acc t ->
            acc + layout_size_in_block (layout t)
        )
    | _ -> failwith "unimplemented"
end

let assemble_record colon_or_eq (boxing : Boxing.t) labels vals =
  let hash = match boxing with Boxed -> "" | Unboxed -> "#" in
  let fields =
    List.map2 labels vals ~f:(fun s x -> s ^ " " ^ colon_or_eq ^ " " ^ x)
  in
  hash ^ "{ " ^ String.concat ~sep:"; " fields ^ " }"

let assemble_record_expr boxing name labels vals =
  (* used to need to type-annotate, but now we generate unique field names *)
  (* "(" ^ *)
  assemble_record "=" boxing labels vals
(* ^ " : " ^ name ^ ")" *)

let assemble_constructor name vals =
  match vals with
  | [] -> name
  | _ -> name ^ "(" ^ String.concat ~sep:", " vals ^ ")"

let assemble_tuple ~sep (boxing : Boxing.t) xs =
  let hash = match boxing with Boxed -> "" | Unboxed -> "#" in
  sprintf "%s(%s)" hash (String.concat ~sep xs)

module Path = struct
  type access =
    | Field of string
    | Unboxed_field of string

  type t = access list

  let to_string t =
    String.concat ~sep:""
      (List.map t ~f:(function
        | Field s -> "." ^ s
        | Unboxed_field s -> ".#" ^ s
        )
        )
end

module Type = struct
  type t =
    | Record of
        { name : string;
          fields : (string * t) list;
          boxing : Boxing.t
        }
    | Variant of
        { name : string;
          constructors : constructor list
        }
    | Tuple of t list * Boxing.t
    | Option of t
    | Int
    | Int64
    | Int64_u
    | Int32
    | Int32_u
    | Nativeint
    | Nativeint_u
    | Unit_u
    | Float
    | Float_u
    | Float32
    | Float32_u
    | String
    | Int64x2_u

  and constructor =
    { name : string;
      args : t list
    }

  let rec follow_path t (path : Path.t) =
    match path with
    | [] -> t
    | access :: rest -> (
      match access, t with
      | Field lbl, Record { name = _; fields; boxing = Boxed }
      | Unboxed_field lbl, Record { name = _; fields; boxing = Unboxed } ->
        follow_path (List.assoc lbl fields) rest
      | _ -> invalid_arg "bad path"
    )

  let compare : t -> t -> int = Stdlib.compare

  let rec structure (t : t) : Type_structure.t =
    match t with
    | Record { fields; boxing; _ } ->
      let ts = List.map fields ~f:(fun (_, t) -> structure t) in
      Record (ts, boxing)
    | Variant { constructors; _ } ->
      let constructors =
        List.map constructors ~f:(fun c -> List.map ~f:structure c.args)
      in
      Variant constructors
    | Tuple (ts, boxing) ->
      let ts = List.map ts ~f:structure in
      Tuple (ts, boxing)
    | Option t -> Option (structure t)
    | Int -> Int
    | Int64 -> Int64
    | Int64_u -> Int64_u
    | Int32 -> Int32
    | Int32_u -> Int32_u
    | Nativeint -> Nativeint
    | Nativeint_u -> Nativeint_u
    | Unit_u -> Unit_u
    | Float -> Float
    | Float_u -> Float_u
    | Float32 -> Float32
    | Float32_u -> Float32_u
    | String -> String
    | Int64x2_u -> Int64x2_u

  let rec code (t : t) =
    match t with
    | Record { name; _ } -> name
    | Variant { name; _ } -> name
    | Tuple (tys, boxing) ->
      assemble_tuple ~sep:" * " boxing (List.map tys ~f:code)
    | Option t -> code t ^ " option"
    | Int -> "int"
    | Int64 -> "int64"
    | Int64_u -> "int64#"
    | Int32 -> "int32"
    | Int32_u -> "int32#"
    | Nativeint -> "nativeint"
    | Nativeint_u -> "nativeint#"
    | Unit_u -> "unit_u"
    | Float -> "float"
    | Float_u -> "float#"
    | Float32 -> "float32"
    | Float32_u -> "float32#"
    | String -> "string"
    | Int64x2_u -> "int64x2#"

  let rec num_subvals t : int =
    let sum_num_subvals ts =
      List.fold_left ts ~f:(fun acc t -> acc + num_subvals t) ~init:0
    in
    match t with
    | Record { fields; _ } -> sum_num_subvals (List.map ~f:snd fields)
    | Variant { constructors; _ } ->
      List.fold_left ~init:0
        ~f:(fun acc c -> max acc (sum_num_subvals c.args))
        constructors
    | Tuple (ts, _) -> sum_num_subvals ts
    | Option t -> num_subvals t
    | Unit_u -> 0
    | Int | Int64 | Int64_u | Int32 | Int32_u | Nativeint | Nativeint_u | Float
    | Float_u | Float32 | Float32_u | String ->
      1
    | Int64x2_u -> 2

  let rec num_subvals_left_of_path t (path : Path.t) =
    match path with
    | [] -> 0
    | access :: rest -> (
      match access, t with
      | Field lbl, Record { name = _; fields; boxing = Boxed }
      | Unboxed_field lbl, Record { name = _; fields; boxing = Unboxed } ->
        let seen = ref false in
        let acc =
          List.fold_left fields ~init:0 ~f:(fun acc (s, t) ->
              if !seen
              then acc
              else if String.equal s lbl
              then begin
                seen := true;
                acc + num_subvals_left_of_path t rest
              end
              else acc + num_subvals t
          )
        in
        assert !seen;
        acc
      | _ -> invalid_arg "bad path"
    )

  let rec value_code (t : t) (i : int) : string =
    match t with
    | Record { name; fields; boxing } ->
      let _, xs =
        List.fold_left_map fields ~init:i ~f:(fun acc (_, t) ->
            let x = value_code t acc in
            acc + num_subvals t, x
        )
      in
      let labels = List.map ~f:fst fields in
      assemble_record_expr boxing name labels xs
    | Variant { name; constructors } ->
      let constructor =
        List.nth constructors (i mod List.length constructors)
      in
      let _, xs =
        List.fold_left_map constructor.args ~init:i ~f:(fun acc t ->
            let x = value_code t acc in
            acc + num_subvals t, x
        )
      in
      assemble_constructor constructor.name xs
    | Tuple (tys, boxing) ->
      let _, xs =
        List.fold_left_map tys ~init:i ~f:(fun acc t ->
            let x = value_code t acc in
            acc + num_subvals t, x
        )
      in
      assemble_tuple ~sep:", " boxing xs
    | Option t -> if Int.equal i 0 then "None" else "Some " ^ value_code t i
    | Int -> Int.to_string i
    | Int64 -> Int.to_string i ^ "L"
    | Int64_u -> "#" ^ Int.to_string i ^ "L"
    | Int32 -> Int.to_string i ^ "l"
    | Int32_u -> "#" ^ Int.to_string i ^ "l"
    | Nativeint -> Int.to_string i ^ "n"
    | Nativeint_u -> "#" ^ Int.to_string i ^ "n"
    | Unit_u -> "(unbox_unit ())"
    | Float -> Int.to_string i ^ "."
    | Float_u -> "#" ^ Int.to_string i ^ "."
    | Float32 -> Int.to_string i ^ ".s"
    | Float32_u -> "#" ^ Int.to_string i ^ ".s"
    | String -> "\"" ^ Int.to_string i ^ "\""
    | Int64x2_u ->
      sprintf
        "(interleave_low_64 (int64x2_of_int64 %dL) (int64x2_of_int64 %dL))" i
        (i + 1)

  let rec mk_value_body_code (t : t) i =
    match t with
    | Record { name; fields; boxing } ->
      let _, xs =
        List.fold_left_map fields ~init:i ~f:(fun acc (_, t) ->
            let x = mk_value_body_code t acc in
            acc + num_subvals t, x
        )
      in
      let labels = List.map ~f:fst fields in
      assemble_record_expr boxing name labels xs
    | Tuple (tys, boxing) ->
      let _, xs =
        List.fold_left_map tys ~init:i ~f:(fun acc t ->
            let x = mk_value_body_code t acc in
            acc + num_subvals t, x
        )
      in
      assemble_tuple ~sep:", " boxing xs
    | Variant { name; constructors } -> (
      let num_constructors = List.length constructors in
      if num_constructors = 0
      then failwith "empty variant"
      else
        let cases =
          List.mapi constructors ~f:(fun idx constructor ->
              let cond = sprintf "i mod %d = %d" num_constructors idx in
              let _, args =
                List.fold_left_map constructor.args ~init:i ~f:(fun acc t ->
                    let x = mk_value_body_code t acc in
                    acc + num_subvals t, x
                )
              in
              let expr = assemble_constructor constructor.name args in
              cond, expr
          )
        in
        match cases with
        | [] -> assert false
        | [(_, expr)] -> expr
        | (cond1, expr1) :: rest ->
          let build_if_else acc (cond, expr) =
            sprintf "(if %s then %s else %s)" cond expr acc
          in
          let last_expr = snd (List.hd (List.rev rest)) in
          List.fold_left
            (List.rev (List.tl (List.rev rest)))
            ~init:(sprintf "(if %s then %s else %s)" cond1 expr1 last_expr)
            ~f:build_if_else
    )
    | Option t ->
      sprintf "(if (i + %d) == 0 then None else Some (%s))" i
        (mk_value_body_code t i)
    | Int -> sprintf "(i + %d)" i
    | Int64 -> sprintf "Int64.of_int (i + %d)" i
    | Int64_u -> sprintf "Int64_u.of_int (i + %d)" i
    | Int32 -> sprintf "Int32.of_int (i + %d)" i
    | Int32_u -> sprintf "Int32_u.of_int (i + %d)" i
    | Nativeint -> sprintf "Nativeint.of_int (i + %d)" i
    | Nativeint_u -> sprintf "Nativeint_u.of_int (i + %d)" i
    | Unit_u -> "unbox_unit ()"
    | Float -> sprintf "Float.of_int (i + %d)" i
    | Float_u -> sprintf "Float_u.of_int (i + %d)" i
    | Float32 -> sprintf "Float32.of_int (i + %d)" i
    | Float32_u -> sprintf "Float32_u.of_int (i + %d)" i
    | String -> sprintf "Int.to_string (i + %d)" i
    | Int64x2_u ->
      sprintf
        "(interleave_low_64 (int64x2_of_int64 (Int64.of_int (i + %d))) \
         (int64x2_of_int64 (Int64.of_int (i + %d))))"
        i (i + 1)

  let mk_value_body_code (t : t) : string = mk_value_body_code t 0

  let rec eq_code (t : t) : string =
    match t with
    | Record { name; fields; boxing } ->
      let body =
        List.map fields ~f:(fun (s, t) -> sprintf "%s %s1 %s2" (eq_code t) s s)
        |> String.concat ~sep:" && "
      in
      let labels = List.map ~f:fst fields in
      let pat i =
        assemble_record_expr boxing name labels
          (List.map fields ~f:(fun (s, _) -> s ^ Int.to_string i))
      in
      sprintf "(fun %s %s -> %s)" (pat 1) (pat 2) body
    | Tuple (tys, boxing) ->
      let pat s =
        assemble_tuple ~sep:", " boxing
          (List.mapi tys ~f:(fun i _ -> s ^ Int.to_string i))
      in
      let body =
        List.mapi tys ~f:(fun i t -> sprintf "%s a%d b%d" (eq_code t) i i)
        |> String.concat ~sep:" && "
      in
      sprintf "(fun %s %s -> %s)" (pat "a") (pat "b") body
    | Variant { name; constructors } ->
      let cases =
        List.map constructors ~f:(fun c ->
            match c.args with
            | [] -> sprintf "%s, %s -> true" c.name c.name
            | args ->
              let a_vars = List.mapi args ~f:(fun i _ -> sprintf "a%d" i) in
              let b_vars = List.mapi args ~f:(fun i _ -> sprintf "b%d" i) in
              let a_pat =
                sprintf "%s(%s)" c.name (String.concat ~sep:", " a_vars)
              in
              let b_pat =
                sprintf "%s(%s)" c.name (String.concat ~sep:", " b_vars)
              in
              let comparisons =
                List.mapi args ~f:(fun i t ->
                    sprintf "%s a%d b%d" (eq_code t) i i
                )
              in
              let body =
                match comparisons with
                | [] -> "true"
                | _ -> String.concat ~sep:" && " comparisons
              in
              sprintf "%s, %s -> %s" a_pat b_pat body
        )
      in
      let extra_cases =
        match List.length cases with
        | 0 -> ["_, _ -> ."]
        | 1 -> []
        | _ -> ["_, _ -> false"]
      in
      let all_cases = String.concat ~sep:" | " (cases @ extra_cases) in
      sprintf "(fun a b -> match a, b with %s)" all_cases
    | Option t ->
      sprintf
        "(fun a b -> match a, b with None,None -> true | Some a,Some b -> %s a \
         b|_->false)"
        (eq_code t)
    | Int -> sprintf "(fun a b -> Int.equal a b)"
    | Int64 -> sprintf "(fun a b -> Int64.equal (globalize a) (globalize b))"
    | Int64_u -> sprintf "(fun a b -> Int64_u.(equal (add #0L a) (add #0L b)))"
    | Int32 -> sprintf "(fun a b -> Int32.equal (globalize a) (globalize b))"
    | Int32_u -> sprintf "(fun a b -> Int32_u.(equal (add #0l a) (add #0l b)))"
    | Nativeint ->
      sprintf "(fun a b -> Nativeint.equal (globalize a) (globalize b))"
    | Nativeint_u ->
      sprintf "(fun a b -> Nativeint_u.(equal (add #0n a) (add #0n b)))"
    | Unit_u -> "(fun _ _ -> true)"
    | Float -> sprintf "(fun a b -> Float.equal (globalize a) (globalize b))"
    | Float_u -> sprintf "(fun a b -> Float_u.(equal (add #0. a) (add #0. b)))"
    | Float32_u ->
      sprintf "(fun a b -> Float32_u.(equal (add #0.s a) (add #0.s b)))"
    | Float32 ->
      sprintf
        "(fun a b -> Float.equal (Float32.to_float a) (Float32.to_float b))"
    | String -> sprintf "(fun a b -> String.equal (globalize a) (globalize b))"
    | Int64x2_u -> sprintf "int64x2_u_equal"

  let rec reverse_unboxed_paths (ty : t) acc cur_path =
    match ty with
    | Record { name = _; fields; boxing = Unboxed } ->
      List.fold_left fields ~init:acc ~f:(fun acc (s, t) ->
          let cur_path = Path.Unboxed_field s :: cur_path in
          let acc = cur_path :: acc in
          reverse_unboxed_paths t acc cur_path
      )
    | _ -> acc

  let unboxed_paths_by_depth ty =
    List.fold_left
      (reverse_unboxed_paths ty [[] (* empty path *)] [])
      ~f:(fun acc rev_path ->
        let depth = List.length rev_path in
        let path = List.rev rev_path in
        let paths =
          match Int_map.find_opt depth acc with
          | Some paths -> path :: paths
          | None -> [path]
        in
        Int_map.add depth paths acc
      )
      ~init:Int_map.empty
    |> Int_map.bindings
end

module Type_structure_map = Map.Make (Type_structure)

module Type_naming = struct
  type t =
    { next_id : int;
      cache : (int * Type.t) Type_structure_map.t
    }

  let empty = { next_id = 0; cache = Type_structure_map.empty }

  let rec add_names t (ty_structure : Type_structure.t) : t * Type.t =
    match Type_structure_map.find_opt ty_structure t.cache with
    | Some (_, ty) -> t, ty
    | None -> (
      match ty_structure with
      | Record (tys, boxing) ->
        let t, tys =
          List.fold_left_mapi ~init:t tys ~f:(fun t i ty ->
              let t, ty = add_names t ty in
              t, ty
          )
        in
        let id = t.next_id in
        let t = { t with next_id = id + 1 } in
        let fields =
          List.mapi tys ~f:(fun i ty ->
              let field_name =
                sprintf "%c%d" (Char.unsafe_chr (Char.code 'a' + i)) id
              in
              field_name, ty
          )
        in
        let name = sprintf "t%d" id in
        let ty : Type.t = Record { name; fields; boxing } in
        ( { t with cache = Type_structure_map.add ty_structure (id, ty) t.cache },
          ty )
      | Tuple (tys, boxing) ->
        let t, tys =
          List.fold_left_mapi ~init:t tys ~f:(fun t i ty ->
              let t, ty = add_names t ty in
              t, ty
          )
        in
        t, Tuple (tys, boxing)
      | Option ty ->
        let t, ty = add_names t ty in
        t, Option ty
      | Int64 -> t, Int64
      | Int64_u -> t, Int64_u
      | Int32 -> t, Int32
      | Int32_u -> t, Int32_u
      | Nativeint -> t, Nativeint
      | Nativeint_u -> t, Nativeint_u
      | Unit_u -> t, Unit_u
      | Float -> t, Float
      | Float_u -> t, Float_u
      | Float32 -> t, Float32
      | Float32_u -> t, Float32_u
      | Int -> t, Int
      | String -> t, String
      | Int64x2_u -> t, Int64x2_u
      | Variant constructors ->
        let t, constructors =
          List.fold_left_map constructors ~init:t ~f:(fun t args ->
              let t, args =
                List.fold_left_map args ~init:t ~f:(fun t ty ->
                    let t, ty = add_names t ty in
                    t, ty
                )
              in
              t, args
          )
        in
        let id = t.next_id in
        let t = { t with next_id = id + 1 } in
        let name = sprintf "v%d" id in
        let constructors : Type.constructor list =
          List.mapi constructors ~f:(fun i args ->
              let cname = sprintf "C%d_%d" id i in
              { Type.name = cname; args }
          )
        in
        let ty : Type.t = Variant { name; constructors } in
        ( { t with cache = Type_structure_map.add ty_structure (id, ty) t.cache },
          ty )
    )

  let decls_code t =
    let decls =
      Type_structure_map.fold
        (fun (ty_structure : Type_structure.t) ((id : int), (ty : Type.t)) acc ->
          let type_definition =
            match ty with
            | Record { name; fields; boxing } ->
              let labels, tys = List.split fields in
              let labels =
                match boxing with
                | Unboxed -> labels
                | Boxed -> List.map labels ~f:(fun s -> "mutable " ^ s)
              in
              assemble_record ":" boxing labels (List.map tys ~f:Type.code)
            | Variant { name; constructors } ->
              let constructor_defs =
                List.map constructors ~f:(fun c ->
                    match (c : Type.constructor).args with
                    | [] -> c.name
                    | args ->
                      sprintf "%s of %s" c.name
                        (String.concat ~sep:" * " (List.map args ~f:Type.code))
                )
              in
              String.concat ~sep:" | " constructor_defs
            | _ -> assert false
          in
          let type_name = Type.code ty in
          ( id,
            sprintf "type %s = %s (* %s *)" type_name type_definition
              (Type_structure.to_string (Type.structure ty))
          )
          :: acc
        )
        t.cache []
    in
    (* Sort by type id *)
    List.sort decls ~cmp:(fun (i, _) (j, _) -> Int.compare i j)
    |> List.map ~f:snd
end

let preamble ~bytecode =
  {|type unit_u : void
external unbox_unit : unit -> unit_u = "%unbox_unit"
external globalize : local_ 'a -> 'a = "%obj_dup";;
|}
  ^
  if bytecode
  then
    {|let int64x2_u_equal (_ : int64x2#) (_ : int64x2#) = failwith "should not be called from bytecode"
|}
  else
    {|external box_int64x2 : int64x2# -> int64x2 = "%box_vec128"
external unbox_int64x2 : int64x2 -> int64x2# = "%unbox_vec128"
external interleave_low_64 : int64x2# -> int64x2# -> int64x2# = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_64" [@@unboxed] [@@builtin]
external interleave_high_64 : int64x2# -> int64x2# -> int64x2# = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_64" [@@unboxed] [@@builtin]
external int64x2_of_int64 : int64 -> int64x2# = "caml_vec128_unreachable" "caml_int64x2_low_of_int64" [@@unboxed] [@@builtin]
external int64_of_int64x2 : int64x2# -> int64 = "caml_vec128_unreachable" "caml_int64x2_low_to_int64" [@@unboxed] [@@builtin]

let int64x2_u_equal i1 i2 =
    let a1 = int64_of_int64x2 i1 in
    let b1 = int64_of_int64x2 (interleave_high_64 i1 i1) in
    let a2 = int64_of_int64x2 i2 in
    let b2 = int64_of_int64x2 (interleave_high_64 i2 i2) in
    Int64.equal a1 a2 && Int64.equal b1 b2
|}
