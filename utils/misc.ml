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

(* Errors *)

exception Fatal_error

let fatal_errorf fmt =
  Format.kfprintf
    (fun _ -> raise Fatal_error)
    Format.err_formatter
    ("@?>> Fatal error: " ^^ fmt ^^ "@.")

let fatal_error msg = fatal_errorf "%s" msg

let unboxed_small_int_arrays_are_not_implemented () =
  fatal_error "unboxed int8/int16 arrays are not implemented"

(* Exceptions *)

let try_finally ?(always=(fun () -> ())) ?(exceptionally=(fun () -> ())) work =
  match work () with
    | result ->
      begin match always () with
        | () -> result
        | exception always_exn ->
          let always_bt = Printexc.get_raw_backtrace () in
          exceptionally ();
          Printexc.raise_with_backtrace always_exn always_bt
      end
    | exception work_exn ->
      let work_bt = Printexc.get_raw_backtrace () in
      begin match always () with
        | () ->
          exceptionally ();
          Printexc.raise_with_backtrace work_exn work_bt
        | exception always_exn ->
          let always_bt = Printexc.get_raw_backtrace () in
          exceptionally ();
          Printexc.raise_with_backtrace always_exn always_bt
      end

let reraise_preserving_backtrace e f =
  let bt = Printexc.get_raw_backtrace () in
  f ();
  Printexc.raise_with_backtrace e bt

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = List.iter (fun (R (r, v)) -> r := v) l in
  fun refs f ->
    let backup = List.map (fun (R (r, _)) -> R (r, !r)) refs in
    set_refs refs;
    Fun.protect ~finally:(fun () -> set_refs backup) f

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rev_map_end f l1 l2 =
  let rec rmap_f accu = function
    | [] -> accu
    | hd::tl -> rmap_f (f hd :: accu) tl
  in
  rmap_f l2 l1

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | (_, _) -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

let rec list_remove x = function
    [] -> []
  | hd :: tl ->
      if hd = x then tl else hd :: list_remove x tl

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl

module Stdlib = struct
  module List = struct
    include List

    type 'a t = 'a list

    let is_empty = function
      | [] -> true
      | _ :: _ -> false

    let rec compare cmp l1 l2 =
      match l1, l2 with
      | [], [] -> 0
      | [], _::_ -> -1
      | _::_, [] -> 1
      | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c <> 0 then c
        else compare cmp t1 t2

    let rec equal eq l1 l2 =
      match l1, l2 with
      | ([], []) -> true
      | (hd1 :: tl1, hd2 :: tl2) -> eq hd1 hd2 && equal eq tl1 tl2
      | (_, _) -> false

    let map2_prefix f l1 l2 =
      let rec aux acc l1 l2 =
        match l1, l2 with
        | [], _ -> (List.rev acc, l2)
        | _ :: _, [] -> raise (Invalid_argument "map2_prefix")
        | h1::t1, h2::t2 ->
          let h = f h1 h2 in
          aux (h :: acc) t1 t2
      in
      aux [] l1 l2

    let map3 f =
      let rec loop acc as_ bs cs = match as_, bs, cs with
        | [], [], [] -> List.rev acc
        | a :: as_, b :: bs, c :: cs -> loop (f a b c :: acc) as_ bs cs
        | _ -> invalid_arg "map3"
      in
      loop []

    let concat_map2 f l1 l2 =
      let rec aux f acc = function
        | [], [] -> rev acc
        | (a1 :: l1, a2 :: l2) ->
          let xs = f a1 a2 in
          aux f (rev_append xs acc) (l1, l2)
        | (_, _) -> invalid_arg "List.concat_map2"
      in aux f [] (l1, l2)

    let rec iteri2 i f l1 l2 =
      match (l1, l2) with
        ([], []) -> ()
      | (a1::l1, a2::l2) -> f i a1 a2; iteri2 (i + 1) f l1 l2
      | (_, _) -> raise (Invalid_argument "iteri2")

    let iteri2 f l1 l2 = iteri2 0 f l1 l2

    let some_if_all_elements_are_some l =
      let rec aux acc l =
        match l with
        | [] -> Some (List.rev acc)
        | None :: _ -> None
        | Some h :: t -> aux (h :: acc) t
      in
      aux [] l

    let map_option f l =
      let rec aux l acc =
        match l with
        | [] -> Some (List.rev acc)
        | x :: xs ->
          match f x with
          | None -> None
          | Some x -> aux xs (x :: acc)
      in
      aux l []

    let map2_option f l1 l2 =
      let rec aux l1 l2 acc =
        match l1, l2 with
        | [], [] -> Some (List.rev acc)
        | x :: xs, y :: ys ->
          begin match f x y with
          | None -> None
          | Some z -> aux xs ys (z :: acc)
          end
        | _, _ -> invalid_arg "map2_option"
      in
      aux l1 l2 []

    let split_at n l =
      let rec aux n acc l =
        if n = 0
        then List.rev acc, l
        else
          match l with
          | [] -> raise (Invalid_argument "split_at")
          | t::q -> aux (n-1) (t::acc) q
      in
      aux n [] l

    let rec map_sharing f l0 =
      match l0 with
      | a :: l ->
        let a' = f a in
        let l' = map_sharing f l in
        if a' == a && l' == l then l0 else a' :: l'
      | [] -> []

    let fold_lefti f accu l =
      let rec aux f i accu l =
        match l with
        | [] -> accu
        | a::l -> aux f (succ i) (f i accu a) l
      in
      aux f 0 accu l

    let fold_left_map2 f accu l1 l2 =
      let rec aux f accu res l1 l2 =
        match l1, l2 with
        | [], [] -> accu, List.rev res
        | a1 :: l1, a2 :: l2 ->
          let accu', r = f accu a1 a2 in
          aux f accu' (r :: res) l1 l2
        | _, _ -> invalid_arg "fold_left_map2"
      in
      aux f accu [] l1 l2

    let chunks_of n l =
      if n <= 0 then raise (Invalid_argument "chunks_of");
      (* Invariant: List.length l = remaining *)
      let rec aux n acc l ~remaining =
        match remaining with
        | 0 -> List.rev acc
        | _ when remaining <= n -> List.rev (l :: acc)
        | _ ->
            let chunk, rest = split_at n l in
            aux n (chunk :: acc) rest ~remaining:(remaining - n)
      in
      aux n [] l ~remaining:(List.length l)

    let rec is_prefix ~equal t ~of_ =
      match t, of_ with
      | [], [] -> true
      | _::_, [] -> false
      | [], _::_ -> true
      | x1::t, x2::of_ -> equal x1 x2 && is_prefix ~equal t ~of_

    type 'a longest_common_prefix_result = {
      longest_common_prefix : 'a list;
      first_without_longest_common_prefix : 'a list;
      second_without_longest_common_prefix : 'a list;
    }

    let find_and_chop_longest_common_prefix ~equal ~first ~second =
      let rec find_prefix ~longest_common_prefix_rev l1 l2 =
        match l1, l2 with
        | elt1 :: l1, elt2 :: l2 when equal elt1 elt2 ->
          let longest_common_prefix_rev = elt1 :: longest_common_prefix_rev in
          find_prefix ~longest_common_prefix_rev l1 l2
        | l1, l2 ->
          { longest_common_prefix = List.rev longest_common_prefix_rev;
            first_without_longest_common_prefix = l1;
            second_without_longest_common_prefix = l2;
          }
      in
      find_prefix ~longest_common_prefix_rev:[] first second

    let rec iter_until_error ~f l =
      match l with
      | [] -> Ok ()
      | x :: xs ->
        match f x with
        | Ok () -> iter_until_error ~f xs
        | Error _ as e -> e

    let [@inline] merge_fold ~cmp ~left_only ~right_only ~both ~init t1 t2 =
      let rec loop acc t1 t2 =
        match t1, t2 with
        | [], [] -> acc
        | a :: t1', [] -> loop (left_only acc a) t1' []
        | [], b :: t2' -> loop (right_only acc b) [] t2'
        | a :: t1', b :: t2' ->
            match cmp a b with
            | 0 -> loop (both acc a b) t1' t2'
            | c when c < 0 -> loop (left_only acc a) t1' t2
            | _ -> loop (right_only acc b) t1 t2'
      in
      loop init t1 t2

    let [@inline] merge_iter ~cmp ~left_only ~right_only ~both t1 t2 =
      merge_fold t1 t2 ~cmp
        ~init:()
        ~left_only:(fun () a -> left_only a)
        ~right_only:(fun () b -> right_only b)
        ~both:(fun () a b -> both a b)
  end

  module Option = struct
    type 'a t = 'a option

    let first_some a b = match a with
      | Some _ -> a
      | None -> b ()

    let print print_contents ppf t =
      match t with
      | None -> Format.pp_print_string ppf "None"
      | Some contents ->
        Format.fprintf ppf "@[(Some@ %a)@]" print_contents contents
  end

  module Array = struct
    let exists2 p a1 a2 =
      let n = Array.length a1 in
      if Array.length a2 <> n then invalid_arg "Misc.Stdlib.Array.exists2";
      let rec loop i =
        if i = n then false
        else if p (Array.unsafe_get a1 i) (Array.unsafe_get a2 i) then true
        else loop (succ i) in
      loop 0

    let fold_left2 f x a1 a2 =
      if Array.length a1 <> Array.length a2
      then invalid_arg "Misc.Stdlib.Array.fold_left2";
      let r = ref x in
      for i = 0 to Array.length a1 - 1 do
        r := f !r (Array.unsafe_get a1 i) (Array.unsafe_get a2 i)
      done;
      !r

    let for_alli p a =
      let n = Array.length a in
      let rec loop i =
        if i = n then true
        else if p i (Array.unsafe_get a i) then loop (succ i)
        else false in
      loop 0

    let all_somes a =
      try
        Some (Array.map (function None -> raise_notrace Exit | Some x -> x) a)
      with
      | Exit -> None

    let equal eq_elt l1 l2 =
      (* Basically inlines [Array.for_all2] to avoid the [raise] *)
      let n = Array.length l1 in
      Int.equal n (Array.length l2) &&
      let rec loop i =
        if Int.equal i n then
          true
        else if eq_elt (Array.unsafe_get l1 i) (Array.unsafe_get l2 i) then
          loop (succ i)
        else
          false
      in
      loop 0

    let compare compare arr1 arr2 =
      let len1 = Array.length arr1 in
      let len2 = Array.length arr2 in
      if len1 <> len2 then
        Int.compare len1 len2
      else
        let rec loop i =
          if i >= len1 then 0
          else
            let cmp = compare arr1.(i) arr2.(i) in
            if cmp <> 0 then cmp else loop (i + 1)
        in
        loop 0

    let map_sharing f a =
      let same = ref true in
      let f' x =
        let x' = f x in
        if x != x' then
          same := false;
        x'
      in
      let a' = (Array.map [@inlined hint]) f' a in
      if !same then a else a'

    let of_list_map f = function
      | [] -> [| |]
      | hd :: tl ->
        let a = Array.make (1 + List.length tl) (f hd) in
        List.iteri (fun i x -> Array.unsafe_set a (i+1) (f x)) tl;
        a

    let concat_arrays : 'a array array -> 'a array = fun arrays ->
      (* CR-soon xclerc for xclerc: should we simply use the following?
        `arrays |> Array.to_list |> Array.concat` *)
      let total_len = ref 0 in
      let init = ref None in
      for i = 0 to pred (Array.length arrays) do
        let array = Array.unsafe_get arrays i in
        let len = Array.length array in
        total_len := !total_len + len;
        if len > 0 then init := Some (Array.unsafe_get array 0)
      done;
      match !total_len, !init with
      | 0, None -> [||]
      | 0, Some _ -> fatal_error "broken invariant"
      | _, None -> fatal_error "broken invariant"
      | _, Some init ->
        let dst = Array.make !total_len init in
        let dst_pos = ref 0 in
        for i = 0 to pred (Array.length arrays) do
          let array = Array.unsafe_get arrays i in
          let len = Array.length array in
          (* CR-soon xclerc for xclerc: use unsafe_blit? *)
          ArrayLabels.blit ~src:array ~src_pos:0 ~dst ~dst_pos:!dst_pos ~len;
          dst_pos := !dst_pos + len
        done;
        dst
  end

  module String = struct
    include String
    module Set = Set.Make(String)
    module Map = struct
      include Map.Make(String)

      let of_seq_multi seq =
        Seq.fold_left
          (fun tbl (key, elt) ->
            update key
              (function None -> Some [elt] | Some s -> Some (elt :: s))
              tbl)
          empty seq
    end

    module Tbl = Hashtbl.Make(struct
      include String
      let hash = Hashtbl.hash
    end)

    let for_all f t =
      let len = String.length t in
      let rec loop i =
        i = len || (f t.[i] && loop (i + 1))
      in
      loop 0

    let print ppf t =
      Format.pp_print_string ppf t

    let begins_with ?(from = 0) str ~prefix =
      let rec helper idx =
        if idx < 0 then true
        else
          String.get str (from + idx) = String.get prefix idx && helper (idx-1)
      in
      let n = String.length str in
      let m = String.length prefix in
      if n >= from + m then helper (m-1) else false

    let split_on_string str ~split_on =
      let n = String.length str in
      let m = String.length split_on in
      let rec helper acc last_idx idx =
        if idx = n then
          let cur = String.sub str last_idx (idx - last_idx) in
          List.rev (cur :: acc)
        else if begins_with ~from:idx str ~prefix:split_on then
          let cur = String.sub str last_idx (idx - last_idx) in
          helper (cur :: acc) (idx + m) (idx + m)
        else
          helper acc last_idx (idx + 1)
      in
      helper [] 0 0

    let split_on_chars str ~split_on:chars =
      let rec helper chars_left s acc =
        match chars_left with
        | [] -> s :: acc
        | c :: cs ->
          List.fold_right (helper cs) (String.split_on_char c s) acc
      in
      helper chars str []

    let split_once str ~idx =
      let n = String.length str in
      String.sub str 0 idx, String.sub str (idx + 1) (n - idx - 1)

    let split_last_exn str ~split_on =
      let ridx = String.rindex str split_on in
      split_once str ~idx:ridx

    let split_first_exn str ~split_on =
      let idx = String.index str split_on in
      split_once str ~idx

    let starts_with ~prefix s =
      let len_s = length s
      and len_pre = length prefix in
      let rec aux i =
        if i = len_pre then true
        else if unsafe_get s i <> unsafe_get prefix i then false
        else aux (i + 1)
      in len_s >= len_pre && aux 0

    let ends_with ~suffix s =
      let len_s = length s
      and len_suf = length suffix in
      let diff = len_s - len_suf in
      let rec aux i =
        if i = len_suf then true
        else if unsafe_get s (diff + i) <> unsafe_get suffix i then false
        else aux (i + 1)
      in diff >= 0 && aux 0

    let is_substring string ~substring =
      let len = String.length substring in
      String.to_seq string
      |> Seq.mapi (fun i _ -> i)
      |> Seq.filter_map (fun i ->
             if i + len < String.length string then
               Some (String.sub string i len)
             else None)
      |> Seq.exists (fun sub -> String.equal substring sub)
  end

  module Int = struct
    include Int
    let min (a : int) (b : int) = min a b
    let max (a : int) (b : int) = max a b
  end

  external compare : 'a -> 'a -> int = "%compare"

  module Monad = struct
    module type Basic2 = sig
      type ('a, 'e) t

      val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

      val return : 'a -> ('a, _) t
    end

    module type Basic = sig
      type 'a t
      include Basic2 with type ('a, _) t := 'a t
    end

    module type S2 = sig
      type ('a, 'e) t

      val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
      val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
      val return : 'a -> ('a, _) t
      val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
      val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
      val ignore_m : (_, 'e) t -> (unit, 'e) t
      val all : ('a, 'e) t list -> ('a list, 'e) t
      val all_unit : (unit, 'e) t list -> (unit, 'e) t

      module Syntax : sig
        val (let+) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
        val (and+) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
        val (let*) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
        val (and*) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
      end
    end

    module type S = sig
      type 'a t
      include S2 with type ('a, _) t := 'a t
    end

    module[@inline] Make2 (X : Basic2) = struct
      include X

      let[@inline] ( >>= ) t f = bind t f

      let map f m =
        bind m (fun a -> return (f a))

      let join m = bind m Fun.id

      let both t1 t2 = t1 >>= fun t1 -> t2 >>= fun t2 -> return (t1, t2)

      let ignore_m m = bind m (fun _ -> return ())

      let all ms =
        let rec loop acc = function
          | [] -> return (List.rev acc)
          | m :: ms -> bind m (fun a -> loop (a :: acc) ms)
        in
        loop [] ms

      let rec all_unit = function
        | [] -> return ()
        | m :: ms -> bind m (fun _ -> all_unit ms)

      module Syntax = struct
        let[@inline] (let+) t f = map f t
        let[@inline] (and+) a b = both a b
        let[@inline] (let*) t f = bind t f
        let[@inline] (and*) a b = (and+) a b
      end
    end

    module[@inline] Make (X : Basic) = struct
      include Make2(struct
          include X
          type ('a, _) t = 'a X.t
        end)

      type nonrec 'a t = 'a X.t
    end

    module Identity = Make(struct
        type 'a t = 'a
        let[@inline] bind x f = f x
        let[@inline] return x = x
      end)

    module Option = Make(struct
        include Stdlib.Option
        let return = some
      end)

    module Result = Make2(struct
        include Stdlib.Result
        let return = ok
      end)
  end
end

module Int = Stdlib.Int

(* File functions *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = simplify (Filename.concat dir name) in
      if Sys.file_exists fullname then fullname else try_dir rem
  in try_dir path

let normalized_unit_filename = String.uncapitalize_ascii

let find_in_path_normalized path name =
  let uname = normalized_unit_filename name in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in try_dir path

let remove_file filename =
  try
    (* merge5: Partial revert of upstream PR 11412, to allow building
       with OCaml 4.14 (which does not have is_regular_file *)
    if Sys.file_exists filename
    then Sys.remove filename
  with Sys_error _msg ->
    ()

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s

let path_separator =
  match Sys.os_type with
  | "Win32" -> ';'
  | _ -> ':'

let split_path_contents ?(sep = path_separator) = function
  | "" -> []
  | s -> String.split_on_char sep s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = Bytes.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (Int.min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

let output_to_file_via_temporary ?(mode = [Open_text]) filename fn =
  let (temp_filename, oc) =
    Filename.open_temp_file
       ~mode ~perms:0o666 ~temp_dir:(Filename.dirname filename)
       (Filename.basename filename) ".tmp" in
    (* The 0o666 permissions will be modified by the umask.  It's just
       like what [open_out] and [open_out_bin] do.
       With temp_dir = dirname filename, we ensure that the returned
       temp file is in the same directory as filename itself, making
       it safe to rename temp_filename to filename later.
       With prefix = basename filename, we are almost certain that
       the first generated name will be unique.  A fixed prefix
       would work too but might generate more collisions if many
       files are being produced simultaneously in the same directory. *)
  match fn temp_filename oc with
  | res ->
      close_out oc;
      begin try
        Sys.rename temp_filename filename; res
      with exn ->
        remove_file temp_filename; raise exn
      end
  | exception exn ->
      close_out oc; remove_file temp_filename; raise exn

let protect_writing_to_file ~filename ~f =
  let outchan = open_out_bin filename in
  try_finally ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file filename)
    (fun () -> f outchan)

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let rec log2_nativeint n =
  if n <= 1n then 0 else 1 + log2_nativeint (Nativeint.shift_right n 1)

let power ~base n =
  let res = ref 1 in
  for _ = 1 to n do
    res := !res * base
  done;
  !res

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

(* Taken from Hacker's Delight, chapter "Overflow Detection" *)
let no_overflow_mul a b =
  not ((a = min_int && b < 0) || (b <> 0 && (a * b) / b <> a))

let no_overflow_lsl a k =
  0 <= k && k < Sys.word_size - 1 && min_int asr k <= a && a <= max_int asr k

let letter_of_int n =
  let letter = String.make 1 (Char.chr (Char.code 'a' + n mod 26)) in
  let num = n / 26 in
  if num = 0 then letter
  else letter ^ Int.to_string num

module Int_literal_converter = struct
  (* To convert integer literals, allowing max_int + 1 (PR#4210) *)
  let cvt_int_aux str neg of_string =
    if String.length str = 0 || str.[0]= '-'
    then of_string str
    else neg (of_string ("-" ^ str))
  let int s = cvt_int_aux s (~-) int_of_string
  let int32 s = cvt_int_aux s Int32.neg Int32.of_string
  let int64 s = cvt_int_aux s Int64.neg Int64.of_string
  let nativeint s = cvt_int_aux s Nativeint.neg Nativeint.of_string
end

(* [find_first_mono p] assumes that there exists a natural number
   N such that [p] is false on [0; N[ and true on [N; max_int], and
   returns this N. (See misc.mli for the detailed specification.) *)
let find_first_mono =
  let rec find p ~low ~jump ~high =
    (* Invariants:
       [low, jump, high] are non-negative with [low < high],
       [p low = false],
       [p high = true]. *)
    if low + 1 = high then high
    (* ensure that [low + jump] is in ]low; high[ *)
    else if jump < 1 then find p ~low ~jump:1 ~high
    else if jump >= high - low then find p ~low ~jump:((high - low) / 2) ~high
    else if p (low + jump) then
      (* We jumped too high: continue with a smaller jump and lower limit *)
      find p ~low:low ~jump:(jump / 2) ~high:(low + jump)
    else
      (* we jumped too low:
         continue from [low + jump] with a larger jump *)
      let next_jump = max jump (2 * jump) (* avoid overflows *) in
      find p ~low:(low + jump) ~jump:next_jump ~high
  in
  fun p ->
    if p 0 then 0
    else find p ~low:0 ~jump:1 ~high:max_int

(* String operations *)

let split_null_terminated s =
  let[@tail_mod_cons] rec discard_last_sep = function
    | [] | [""] -> []
    | x :: xs -> x :: discard_last_sep xs
  in
  discard_last_sep (String.split_on_char '\000' s)

let concat_null_terminated = function
  | [] -> ""
  | l -> String.concat "\000" (l @ [""])

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename 0 pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str curr (next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str curr (String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat after (search [] 0)

let rev_split_words s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      match s.[i] with
        ' ' | '\t' | '\r' | '\n' -> split1 res (i+1)
      | _ -> split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s i (j-i) :: res else begin
      match s.[j] with
        ' ' | '\t' | '\r' | '\n' -> split1 (String.sub s i (j-i) :: res) (j+1)
      | _ -> split2 res i (j+1)
    end
  in split1 [] 0

let get_ref r =
  let v = !r in
  r := []; v

let set_or_ignore f opt x =
  match f x with
  | None -> ()
  | Some y -> opt := Some y

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x


module LongString = struct
  type t = bytes array

  let create str_size =
    let tbl_size = str_size / Sys.max_string_length + 1 in
    let tbl = Array.make tbl_size Bytes.empty in
    for i = 0 to tbl_size - 2 do
      tbl.(i) <- Bytes.create Sys.max_string_length;
    done;
    tbl.(tbl_size - 1) <- Bytes.create (str_size mod Sys.max_string_length);
    tbl

  let length tbl =
    let tbl_size = Array.length tbl in
    Sys.max_string_length * (tbl_size - 1) + Bytes.length tbl.(tbl_size - 1)

  let get tbl ind =
    Bytes.get tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)

  let set tbl ind c =
    Bytes.set tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)
              c

  let blit src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      set dst (dstoff + i) (get src (srcoff + i))
    done

  let output oc tbl pos len =
    for i = pos to pos + len - 1 do
      output_char oc (get tbl i)
    done

  let input_bytes_into tbl ic len =
    let count = ref len in
    Array.iter (fun str ->
      let chunk = Int.min !count (Bytes.length str) in
      really_input ic str 0 chunk;
      count := !count - chunk) tbl

  let input_bytes ic len =
    let tbl = create len in
    input_bytes_into tbl ic len;
    tbl
end


let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)

let ordinal_suffix n =
  let teen = (n mod 100)/10 = 1 in
  match n mod 10 with
  | 1 when not teen -> "st"
  | 2 when not teen -> "nd"
  | 3 when not teen -> "rd"
  | _ -> "th"

let format_as_unboxed_literal s =
  if String.starts_with ~prefix:"-" s
  then "-#" ^ (String.sub s 1 (String.length s - 1))
  else "#" ^ s

(* Color handling *)
module Color = struct
  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> ""
    && isatty stderr

  type setting = Auto | Always | Never

  let default_setting = Auto
  let enabled = ref true

end

(* Terminal styling handling *)
module Style = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"

  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"


  type Format.stag += Style of style list

  type tag_style ={
    ansi: style list;
    text_open:string;
    text_close:string
  }

  type styles = {
    error: tag_style;
    warning: tag_style;
    loc: tag_style;
    hint: tag_style;
    inline_code: tag_style;
  }

  let no_markup stl = { ansi = stl; text_close = ""; text_open = "" }

  let default_styles = {
      warning = no_markup [Bold; FG Magenta];
      error = no_markup [Bold; FG Red];
      loc = no_markup [Bold];
      hint = no_markup [Bold; FG Blue];
      inline_code= { ansi=[Bold]; text_open = {|"|}; text_close = {|"|} }
    }

  let cur_styles = ref default_styles
  let get_styles () = !cur_styles
  let set_styles s = cur_styles := s

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | Format.String_tag "error" ->  (!cur_styles).error
    | Format.String_tag "warning" ->(!cur_styles).warning
    | Format.String_tag "loc" -> (!cur_styles).loc
    | Format.String_tag "hint" -> (!cur_styles).hint
    | Format.String_tag "inline_code" -> (!cur_styles).inline_code
    | Style s -> no_markup s
    | _ -> raise Not_found


  let as_inline_code printer ppf x =
    Format.pp_open_stag ppf (Format.String_tag "inline_code");
    printer ppf x;
    Format.pp_close_stag ppf ()

  let inline_code ppf s = as_inline_code Format.pp_print_string ppf s

  let as_clflag flag printer ppf x =
    Format.fprintf ppf "@{<inline_code>%s %a@}" flag printer x

  (* either prints the tag of [s] or delegates to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l style.ansi else style.text_open
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l [Reset] else style.text_close
    with Not_found -> or_else s

  (* add tag handling to formatter [ppf] *)
  let set_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_stag_functions ppf () in
    let functions' = {functions with
      mark_open_stag=(mark_open_tag ~or_else:functions.mark_open_stag);
      mark_close_stag=(mark_close_tag ~or_else:functions.mark_close_stag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_stag_functions ppf functions';
    ()

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    let enable_color = function
      | Color.Auto -> Color.should_enable_color ()
      | Color.Always -> true
      | Color.Never -> false
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_tag_handling formatter_l;
        Color.enabled := (match o with
          | Some s -> enable_color s
          | None -> enable_color Color.default_setting)
      );
      ()
end

let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    Int.min (Int.max la lb) cutoff in
  if abs (la - lb) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i;
    done;
    for j = 1 to lb do
      m.(0).(j) <- j;
    done;
    for i = 1 to la do
      for j = Int.max 1 (i - cutoff - 1) to Int.min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          Int.min (1 + Int.min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else Int.min best (m.(i-2).(j-2) + cost)
        in
        m.(i).(j) <- best
      done;
    done;
    let result = m.(la).(lb) in
    if result > cutoff
    then None
    else Some result
  end

let spellcheck env name =
  let cutoff =
    match String.length name with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target acc head =
    match edit_distance target head cutoff with
      | None -> acc
      | Some dist ->
         let (best_choice, best_dist) = acc in
         if dist < best_dist then ([head], dist)
         else if dist = best_dist then (head :: best_choice, dist)
         else acc
  in
  let env = List.sort_uniq (fun s1 s2 -> String.compare s2 s1) env in
  fst (List.fold_left (compare name) ([], max_int) env)

let did_you_mean ppf get_choices =
  (* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. *)
  Format.fprintf ppf "@?";
  match get_choices () with
  | [] -> ()
  | choices ->
    let rest, last = split_last choices in
    let comma ppf () = Format.fprintf ppf ", " in
     Format.fprintf ppf "@\n@{<hint>Hint@}: Did you mean %a%s%a?@?"
       (Format.pp_print_list ~pp_sep:comma Style.inline_code) rest
       (if rest = [] then "" else " or ")
       Style.inline_code last

module Error_style = struct
  type setting =
    | Contextual
    | Short

  let default_setting = Contextual
end

let normalise_eol s =
  let b = Buffer.create 80 in
    for i = 0 to String.length s - 1 do
      if s.[i] <> '\r' then Buffer.add_char b s.[i]
    done;
    Buffer.contents b

let delete_eol_spaces src =
  let len_src = String.length src in
  let dst = Bytes.create len_src in
  let rec loop i_src i_dst =
    if i_src = len_src then
      i_dst
    else
      match src.[i_src] with
      | ' ' | '\t' ->
        loop_spaces 1 (i_src + 1) i_dst
      | c ->
        Bytes.set dst i_dst c;
        loop (i_src + 1) (i_dst + 1)
  and loop_spaces spaces i_src i_dst =
    if i_src = len_src then
      i_dst
    else
      match src.[i_src] with
      | ' ' | '\t' ->
        loop_spaces (spaces + 1) (i_src + 1) i_dst
      | '\n' ->
        Bytes.set dst i_dst '\n';
        loop (i_src + 1) (i_dst + 1)
      | _ ->
        for n = 0 to spaces do
          Bytes.set dst (i_dst + n) src.[i_src - spaces + n]
        done;
        loop (i_src + 1) (i_dst + spaces + 1)
  in
  let stop = loop 0 0 in
  Bytes.sub_string dst 0 stop

let pp_two_columns ?(sep = "|") ?max_lines ppf (lines: (string * string) list) =
  let left_column_size =
    List.fold_left (fun acc (s, _) -> Int.max acc (String.length s)) 0 lines in
  let lines_nb = List.length lines in
  let ellipsed_first, ellipsed_last =
    match max_lines with
    | Some max_lines when lines_nb > max_lines ->
        let printed_lines = max_lines - 1 in (* the ellipsis uses one line *)
        let lines_before = printed_lines / 2 + printed_lines mod 2 in
        let lines_after = printed_lines / 2 in
        (lines_before, lines_nb - lines_after - 1)
    | _ -> (-1, -1)
  in
  Format.fprintf ppf "@[<v>";
  List.iteri (fun k (line_l, line_r) ->
    if k = ellipsed_first then Format.fprintf ppf "...@,";
    if ellipsed_first <= k && k <= ellipsed_last then ()
    else Format.fprintf ppf "%*s %s %s@," left_column_size line_l sep line_r
  ) lines;
  Format.fprintf ppf "@]"

let pp_parens_if condition printer ppf arg =
  Format.fprintf ppf "%s%a%s"
    (if condition then "(" else "")
    printer arg
    (if condition then ")" else "")

let pp_nested_list ~nested ~pp_element ~pp_sep ppf arg =
  Format.fprintf ppf "@[<hv>%a@]"
    (pp_parens_if nested
       (Format.pp_print_list ~pp_sep (pp_element ~nested:true)))
    arg

(* showing configuration and configuration variables *)
let show_config_and_exit () =
  Config.print_config stdout;
  exit 0

let show_config_variable_and_exit x =
  match Config.config_var x with
  | Some v ->
      (* we intentionally don't print a newline to avoid Windows \r
         issues: bash only strips the trailing \n when using a command
         substitution $(ocamlc -config-var foo), so a trailing \r would
         remain if printing a newline under Windows and scripts would
         have to use $(ocamlc -config-var foo | tr -d '\r')
         for portability. Ugh. *)
      print_string v;
      exit 0
  | None ->
      exit 2

let get_build_path_prefix_map =
  let init = ref false in
  let map_cache = ref None in
  fun () ->
    if not !init then begin
      init := true;
      match Sys.getenv "BUILD_PATH_PREFIX_MAP" with
      | exception Not_found -> ()
      | encoded_map ->
        match Build_path_prefix_map.decode_map encoded_map with
          | Error err ->
              fatal_errorf
                "Invalid value for the environment variable \
                 BUILD_PATH_PREFIX_MAP: %s" err
          | Ok map -> map_cache := Some map
    end;
    !map_cache

let debug_prefix_map_flags () =
  if not Config.as_has_debug_prefix_map then
    []
  else begin
    match get_build_path_prefix_map () with
    | None -> []
    | Some map ->
      List.fold_right
        (fun map_elem acc ->
           match map_elem with
           | None -> acc
           | Some { Build_path_prefix_map.target; source; } ->
             (Printf.sprintf "--debug-prefix-map %s=%s"
                (Filename.quote source)
                (Filename.quote target)) :: acc)
        map
        []
  end

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let print_see_manual ppf manual_section =
  let open Format in
  fprintf ppf "(see manual section %a)"
    (pp_print_list ~pp_sep:(fun f () -> pp_print_char f '.') pp_print_int)
    manual_section

let output_of_print print =
  let output out_channel t =
    let ppf = Format.formatter_of_out_channel out_channel in
    (* Effectively disable automatic wrapping because [Printf]-based code
       doesn't expect it *)
    Format.pp_set_margin ppf Int.max_int;
    print ppf t;
    (* Must flush the formatter immediately because it has a buffer separate
       from the output channel's buffer *)
    Format.pp_print_flush ppf ()
  in
  output

let is_print_longer_than size p =
  let exception Limit_exceeded in
  let limit = ref size in
  let count_down len =
    limit := !limit - len;
    if !limit < 0 then raise Limit_exceeded
  in
  let out_string _ _ len = count_down len in
  let out_newline () = count_down 1 in
  let out_spaces n = count_down n in
  let out_flush _ = () in
  let out_indent _ = () in
  let out_functions : Format.formatter_out_functions = {
    out_string;
    out_flush;
    out_newline;
    out_spaces;
    out_indent}
  in
  let ppf = Format.formatter_of_out_functions out_functions in
  try p ppf; false
  with Limit_exceeded -> true

let to_string_of_print print =
  let to_string t =
    (* Implemented similarly to [Format.asprintf] *)
    let buf = Buffer.create 32 in
    let ppf = Format.formatter_of_buffer buf in
    Format.pp_set_margin ppf Int.max_int;
    print ppf t;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  to_string


type filepath = string

type alerts = string Stdlib.String.Map.t

module Bitmap = struct
  type t = {
    length: int;
    bits: bytes
  }

  let length_bytes len =
    (len + 7) lsr 3

  let make n =
    { length = n;
      bits = Bytes.make (length_bytes n) '\000' }

  let unsafe_get_byte t n =
    Char.code (Bytes.unsafe_get t.bits n)

  let unsafe_set_byte t n x =
    Bytes.unsafe_set t.bits n (Char.unsafe_chr x)

  let check_bound t n =
    if n < 0 || n >= t.length then invalid_arg "Bitmap.check_bound"

  let set t n =
    check_bound t n;
    let pos = n lsr 3 and bit = 1 lsl (n land 7) in
    unsafe_set_byte t pos (unsafe_get_byte t pos lor bit)

  let clear t n =
    check_bound t n;
    let pos = n lsr 3 and nbit = 0xff lxor (1 lsl (n land 7)) in
    unsafe_set_byte t pos (unsafe_get_byte t pos land nbit)

  let get t n =
    check_bound t n;
    let pos = n lsr 3 and bit = 1 lsl (n land 7) in
    (unsafe_get_byte t pos land bit) <> 0

  let iter f t =
    for i = 0 to length_bytes t.length - 1 do
      let c = unsafe_get_byte t i in
      let pos = i lsl 3 in
      for j = 0 to 7 do
        if c land (1 lsl j) <> 0 then
          f (pos + j)
      done
    done
end

module Magic_number = struct
  type version = int

  type kind =
    | Exec
    | Cmi | Cmo | Cma
    | Cmx | Cmxa
    | Cmxs
    | Cmt
    | Cms
    | Ast_impl | Ast_intf

  (* please keep up-to-date, this is used for sanity checking *)
  let all_kinds = [
    Exec;
    Cmi; Cmo; Cma;
    Cmx; Cmxa;
    Cmt;
    Ast_impl; Ast_intf;
  ]

  type raw = string
  type info = {
    kind: kind;
    version: version;
  }

  type raw_kind = string

  let parse_kind : raw_kind -> kind option = function
    | "Caml1999X" -> Some Exec
    | "Caml1999I" -> Some Cmi
    | "Caml1999O" -> Some Cmo
    | "Caml1999A" -> Some Cma
    | "Caml1999Y" -> Some Cmx
    | "Caml1999Z" -> Some Cmxa

    (* Caml2007D and Caml2012T were used instead of the common Caml1999 prefix
       between the introduction of those magic numbers and October 2017
       (8ba70ff194b66c0a50ffb97d41fe9c4bdf9362d6).

       We accept them here, but will always produce/show kind prefixes
       that follow the current convention, Caml1999{D,T}. *)
    | "Caml2007D" | "Caml1999D" -> Some Cmxs
    | "Caml2012T" | "Caml1999T" -> Some Cmt
    | "Caml1999S" -> Some Cms

    | "Caml1999M" -> Some Ast_impl
    | "Caml1999N" -> Some Ast_intf
    | _ -> None

  (* note: over time the magic kind number has changed for certain kinds;
     this function returns them as they are produced by the current compiler,
     but [parse_kind] accepts older formats as well. *)
  let raw_kind : kind -> raw = function
    | Exec -> "Caml1999X"
    | Cmi -> "Caml1999I"
    | Cmo -> "Caml1999O"
    | Cma -> "Caml1999A"
    | Cmx -> "Caml1999Y"
    | Cmxa -> "Caml1999Z"
    | Cmxs -> "Caml1999D"
    | Cmt -> "Caml1999T"
    | Cms -> "Caml1999S"
    | Ast_impl -> "Caml1999M"
    | Ast_intf -> "Caml1999N"

  let string_of_kind : kind -> string = function
    | Exec -> "exec"
    | Cmi -> "cmi"
    | Cmo -> "cmo"
    | Cma -> "cma"
    | Cmx -> "cmx"
    | Cmxa -> "cmxa"
    | Cmxs -> "cmxs"
    | Cmt -> "cmt"
    | Cms -> "cms"
    | Ast_impl -> "ast_impl"
    | Ast_intf -> "ast_intf"

  let human_name_of_kind : kind -> string = function
    | Exec -> "executable"
    | Cmi -> "compiled interface file"
    | Cmo -> "bytecode object file"
    | Cma -> "bytecode library"
    | Cmx -> "native compilation unit description"
    | Cmxa -> "static native library"
    | Cmxs -> "dynamic native library"
    | Cmt -> "compiled typedtree file"
    | Cms -> "compiled shape file"
    | Ast_impl -> "serialized implementation AST"
    | Ast_intf -> "serialized interface AST"

  let kind_length = 9
  let version_length = 3
  let magic_length =
    kind_length + version_length

  type parse_error =
    | Truncated of string
    | Not_a_magic_number of string

  let explain_parse_error kind_opt error =
       Printf.sprintf
         "We expected a valid %s, but the file %s."
         (Option.fold ~none:"object file" ~some:human_name_of_kind kind_opt)
         (match error with
            | Truncated "" -> "is empty"
            | Truncated _ -> "is truncated"
            | Not_a_magic_number _ -> "has a different format")

  let parse s : (info, parse_error) result =
    if String.length s = magic_length then begin
      let raw_kind = String.sub s 0 kind_length in
      let raw_version = String.sub s kind_length version_length in
      match parse_kind raw_kind with
      | None -> Error (Not_a_magic_number s)
      | Some kind ->
          begin match int_of_string raw_version with
          | exception _ -> Error (Truncated s)
          | version -> Ok { kind; version }
          end
    end
    else begin
      (* a header is "truncated" if it starts like a valid magic number,
         that is if its longest segment of length at most [kind_length]
         is a prefix of [raw_kind kind] for some kind [kind] *)
      let sub_length = Int.min kind_length (String.length s) in
      let starts_as kind =
        String.sub s 0 sub_length = String.sub (raw_kind kind) 0 sub_length
      in
      if List.exists starts_as all_kinds then Error (Truncated s)
      else Error (Not_a_magic_number s)
    end

  let read_info ic =
    let header = Buffer.create magic_length in
    begin
      try Buffer.add_channel header ic magic_length
      with End_of_file -> ()
    end;
    parse (Buffer.contents header)

  let raw { kind; version; } =
    Printf.sprintf "%s%03d" (raw_kind kind) version

  let current_raw kind =
    let open Config in
    match[@warning "+9"] kind with
      | Exec -> exec_magic_number
      | Cmi -> cmi_magic_number
      | Cmo -> cmo_magic_number
      | Cma -> cma_magic_number
      | Cmx -> cmx_magic_number
      | Cmxa -> cmxa_magic_number
      | Cmxs -> cmxs_magic_number
      | Cmt -> cmt_magic_number
      | Cms -> cms_magic_number
      | Ast_intf -> ast_intf_magic_number
      | Ast_impl -> ast_impl_magic_number

  (* it would seem more direct to define current_version with the
     correct numbers and current_raw on top of it, but for now we
     consider the Config.foo values to be ground truth, and don't want
     to trust the present module instead. *)
  let current_version kind =
    let raw = current_raw kind in
    try int_of_string (String.sub raw kind_length version_length)
    with _ -> assert false

  type 'a unexpected = { expected : 'a; actual : 'a }
  type unexpected_error =
    | Kind of kind unexpected
    | Version of kind * version unexpected

  let explain_unexpected_error = function
    | Kind { actual; expected } ->
        Printf.sprintf "We expected a %s (%s) but got a %s (%s) instead."
          (human_name_of_kind expected) (string_of_kind expected)
          (human_name_of_kind actual) (string_of_kind actual)
    | Version (kind, { actual; expected }) ->
        Printf.sprintf "This seems to be a %s (%s) for %s version of OCaml."
          (human_name_of_kind kind) (string_of_kind kind)
          (if actual < expected then "an older" else "a newer")

  let check_current expected_kind { kind; version } : _ result =
    if kind <> expected_kind then begin
      let actual, expected = kind, expected_kind in
      Error (Kind { actual; expected })
    end else begin
      let actual, expected = version, current_version kind in
      if actual <> expected
      then Error (Version (kind, { actual; expected }))
      else Ok ()
    end

  type error =
    | Parse_error of parse_error
    | Unexpected_error of unexpected_error

  let read_current_info ~expected_kind ic =
    match read_info ic with
      | Error err -> Error (Parse_error err)
      | Ok info ->
         let kind = Option.value ~default:info.kind expected_kind in
         match check_current kind info with
           | Error err -> Error (Unexpected_error err)
           | Ok () -> Ok info
end

module Le_result = struct
  type t =
    | Equal
    | Less
    | Not_le

  let combine sr1 sr2 =
    match sr1, sr2 with
    | Equal, Equal -> Equal
    | Equal, Less | Less, Equal | Less, Less -> Less
    | Not_le, _ | _, Not_le -> Not_le

  let combine_list ts = List.fold_left combine Equal ts

  let is_le = function
    | Equal -> true
    | Less -> true
    | Not_le -> false

  let is_equal = function
    | Equal -> true
    | Less | Not_le -> false

  let less_or_equal ~le a b =
    match le a b, le b a with
    | true, true -> Equal
    | true, false -> Less
    | false, _ -> Not_le

  let equal ~le a b = le a b && le b a
end

(*********************************************)
(* Fancy types *)

type (_, _) eq = Refl : ('a, 'a) eq
(*********************************************)
(* Fancy modules *)

module type T = sig
  type t
end

module type T1 = sig
  type 'a t
end

module type T2 = sig
  type ('a, 'b) t
end

module type T3 = sig
  type ('a, 'b, 'c) t
end

module type T4 = sig
  type ('a, 'b, 'c, 'd) t
end

let remove_double_underscores s =
  let len = String.length s in
  let buf = Buffer.create len in
  let skip = ref false in
  let rec loop i =
    if i < len
    then (
      let c = String.get s i in
      if c = '.' then skip := true;
      if (not !skip) && c = '_' && i + 1 < len && String.get s (i + 1) = '_'
      then (
        Buffer.add_char buf '.';
        skip := true;
        loop (i + 2))
      else (
        Buffer.add_char buf c;
        loop (i + 1)))
  in
  loop 0;
  Buffer.contents buf

module Nonempty_list = struct
  type nonrec 'a t = ( :: ) of 'a * 'a list

  let to_list (x :: xs) : _ list = x :: xs

  let of_list_opt : _ list -> _ t option = function
    | [] -> None
    | (x :: xs)-> Some (x :: xs)

  let map f (x :: xs) = f x :: List.map f xs

  let pp_print ?pp_sep f ppf t =
    Format.pp_print_list ?pp_sep f ppf (to_list t)

  let (@) (x :: xs) (y :: ys) =
    x :: List.(xs @ (y :: ys))
end
