(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Stephen Dolan, Jane Street, London                   *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Allowance
open Solver_intf

module Magic_equal (X : Equal) :
  Equal with type ('a, 'b, 'c) t = ('a, 'b, 'c) X.t = struct
  type ('a, 'b, 'd) t = ('a, 'b, 'd) X.t

  let equal :
      type a0 a1 b l0 l1 r0 r1.
      (a0, b, l0 * r0) t -> (a1, b, l1 * r1) t -> (a0, a1) Misc.eq option =
   fun x0 x1 ->
    if Obj.repr x0 = Obj.repr x1 then Some (Obj.magic Misc.Refl) else None
end
[@@inline]

type 'a error =
  { left : 'a;
    right : 'a
  }

module Solver_mono (C : Lattices_mono) = struct
  type nonrec 'a error = 'a error

  type any_morph = Any_morph : ('a, 'b, 'd) C.morph -> any_morph

  module VarMap = Map.Make (struct
    type t = int * any_morph

    let compare (i1, m1) (i2, m2) =
      match Int.compare i1 i2 with 0 -> compare m1 m2 | i -> i
  end)

  (** Map the function to the list, and returns the first [Error] found;
      Returns [Ok ()] if no error. *)
  let find_error (f : 'x -> ('a, 'b) Result.t) (t : 'x VarMap.t) :
      ('a, 'b) Result.t =
    let r = ref (Ok ()) in
    let _ =
      VarMap.for_all
        (fun _ x ->
          match f x with
          | Ok () -> true
          | Error _ as e ->
            r := e;
            false)
        t
    in
    !r

  let var_map_to_list t = VarMap.fold (fun _ a xs -> a :: xs) t []

  type 'a var =
    { mutable vlower : 'a lmorphvar VarMap.t;
          (** A list of variables directly under the current variable.
        Each is a pair [f] [v], and we have [f v <= u] where [u] is the current
        variable.
        TODO: consider using hashset for quicker deduplication *)
      mutable upper : 'a;  (** The precise upper bound of the variable *)
      mutable lower : 'a;
          (** The *conservative* lower bound of the variable.
       Why conservative: if a user calls [submode c u] where [c] is
        some constant and [u] some variable, we can modify [u.lower] of course.
       Idealy we should also modify all [v.lower] where [v] is variable above [u].
       However, we only have [vlower] not [vupper]. Therefore, the [lower] of
       higher variables are not updated immediately, hence conservative. Those
       [lower] of higher variables can be made precise later on demand, see
       [zap_to_floor_var_aux].

       One might argue for an additional [vupper] field, so that [lower] are
       always precise. While this might be doable, we note that the "hotspot" of
       the mode solver is to detect conflict, which is already achieved without
       precise [lower]. Adding [vupper] and keeping [lower] precise will come
       at extra cost. *)
      (* To summarize, INVARIANT:
         - For any variable [v], we have [v.lower <= v.upper].
         - Variables that have been fully constrained will have
         [v.lower = v.upper]. Note that adding a boolean field indicating that
         won't help much.
         - For any [v] and [f u \in v.vlower], we have [f u.upper <= v.upper], but not
         necessarily [f u.lower <= v.lower]. *)
      id : int  (** For identification/printing *)
    }

  and 'b lmorphvar = ('b, left_only) morphvar

  and ('b, 'd) morphvar =
    | Amorphvar : 'a var * ('a, 'b, 'd) C.morph -> ('b, 'd) morphvar
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  let get_key (Amorphvar (v, m)) = v.id, Any_morph m

  module VarSet = Set.Make (Int)

  type change =
    | Cupper : 'a var * 'a -> change
    | Clower : 'a var * 'a -> change
    | Cvlower : 'a var * 'a lmorphvar VarMap.t -> change

  type changes = change list

  let undo_change = function
    | Cupper (v, upper) -> v.upper <- upper
    | Clower (v, lower) -> v.lower <- lower
    | Cvlower (v, vlower) -> v.vlower <- vlower

  let empty_changes = []

  let undo_changes l = List.iter undo_change l

  (** [append_changes l0 l1] returns a log that's equivalent to [l0] followed by
  [l1]. *)
  let append_changes l0 l1 = l1 @ l0

  type ('a, 'd) mode =
    | Amode : 'a -> ('a, 'l * 'r) mode
    | Amodevar : ('a, 'd) morphvar -> ('a, 'd) mode
    | Amodejoin :
        'a * ('a, 'l * disallowed) morphvar VarMap.t
        -> ('a, 'l * disallowed) mode
        (** [Amodejoin a [mv0, mv1, ..]] represents [a join mv0 join mv1 join ..] *)
    | Amodemeet :
        'a * ('a, disallowed * 'r) morphvar VarMap.t
        -> ('a, disallowed * 'r) mode
        (** [Amodemeet a [mv0, mv1, ..]] represents [a meet mv0 meet mv1 meet ..]. *)
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  (** Prints a mode variable, including the set of variables below it
      (recursively). To handle cycles, [traversed] is the set of variables that
      we have already printed and will be skipped. An example of cycle:

      Consider a lattice containing three elements A = {0, 1, 2} with the linear
      lattice structure: 0 < 1 < 2. Furthermore, we define a morphism
      f : A -> A
      f 0 = 0
      f 1 = 2
      f 2 = 2

      Note that f has a left right, which allows us to write f on the LHS of
      submode. Say we create a unconstrained variable [x], and invoke submode:
      f x <= x
      this would result in adding (f, x) into the [vlower] of [x]. That is,
      there will be a self-loop on [x].
      *)
  let rec print_var : type a. ?traversed:VarSet.t -> a C.obj -> _ -> a var -> _
      =
   fun ?traversed obj ppf v ->
    Format.fprintf ppf "modevar#%x[%a .. %a]" v.id (C.print obj) v.lower
      (C.print obj) v.upper;
    match traversed with
    | None -> ()
    | Some traversed ->
      if VarSet.mem v.id traversed
      then ()
      else
        let traversed = VarSet.add v.id traversed in
        let p = print_morphvar ~traversed obj in
        Format.fprintf ppf "{%a}" (Format.pp_print_list p)
          (var_map_to_list v.vlower)

  and print_morphvar :
      type a l r.
      ?traversed:VarSet.t -> a C.obj -> _ -> (a, l * r) morphvar -> _ =
   fun ?traversed dst ppf (Amorphvar (v, f)) ->
    let src = C.src dst f in
    Format.fprintf ppf "%a(%a)" (C.print_morph dst) f (print_var ?traversed src)
      v

  let print_raw :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?(verbose = false) (obj : a C.obj) ppf m ->
    let traversed = if verbose then Some VarSet.empty else None in
    match m with
    | Amode a -> C.print obj ppf a
    | Amodevar mv -> print_morphvar ?traversed obj ppf mv
    | Amodejoin (a, mvs) ->
      Format.fprintf ppf "join(%a,%a)" (C.print obj) a
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        (var_map_to_list mvs)
    | Amodemeet (a, mvs) ->
      Format.fprintf ppf "meet(%a,%a)" (C.print obj) a
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        (var_map_to_list mvs)

  module Morphvar = Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) morphvar constraint 'd = 'l * 'r

    let allow_left :
        type a l r. (a, allowed * r) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m) -> Amorphvar (v, C.allow_left m)

    let allow_right :
        type a l r. (a, l * allowed) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m) -> Amorphvar (v, C.allow_right m)

    let disallow_left :
        type a l r. (a, l * r) morphvar -> (a, disallowed * r) morphvar =
      function
      | Amorphvar (v, m) -> Amorphvar (v, C.disallow_left m)

    let disallow_right :
        type a l r. (a, l * r) morphvar -> (a, l * disallowed) morphvar =
      function
      | Amorphvar (v, m) -> Amorphvar (v, C.disallow_right m)
  end)

  include Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) mode constraint 'd = 'l * 'r

    let allow_left : type a l r. (a, allowed * r) mode -> (a, l * r) mode =
      function
      | Amode c -> Amode c
      | Amodevar mv -> Amodevar (Morphvar.allow_left mv)
      | Amodejoin (c, mvs) -> Amodejoin (c, VarMap.map Morphvar.allow_left mvs)

    let allow_right : type a l r. (a, l * allowed) mode -> (a, l * r) mode =
      function
      | Amode c -> Amode c
      | Amodevar mv -> Amodevar (Morphvar.allow_right mv)
      | Amodemeet (c, mvs) -> Amodemeet (c, VarMap.map Morphvar.allow_right mvs)

    let disallow_left : type a l r. (a, l * r) mode -> (a, disallowed * r) mode
        = function
      | Amode c -> Amode c
      | Amodevar mv -> Amodevar (Morphvar.disallow_left mv)
      | Amodejoin (c, mvs) ->
        Amodejoin (c, VarMap.map Morphvar.disallow_left mvs)
      | Amodemeet (c, mvs) ->
        Amodemeet (c, VarMap.map Morphvar.disallow_left mvs)

    let disallow_right : type a l r. (a, l * r) mode -> (a, l * disallowed) mode
        = function
      | Amode c -> Amode c
      | Amodevar mv -> Amodevar (Morphvar.disallow_right mv)
      | Amodejoin (c, mvs) ->
        Amodejoin (c, VarMap.map Morphvar.disallow_right mvs)
      | Amodemeet (c, mvs) ->
        Amodemeet (c, VarMap.map Morphvar.disallow_right mvs)
  end)

  let mlower dst (Amorphvar (var, morph)) = C.apply dst morph var.lower

  let mupper dst (Amorphvar (var, morph)) = C.apply dst morph var.upper

  let min (type a) (obj : a C.obj) = Amode (C.min obj)

  let max (type a) (obj : a C.obj) = Amode (C.max obj)

  let of_const _obj a = Amode a

  let apply_morphvar dst morph (Amorphvar (var, morph')) =
    Amorphvar (var, C.compose dst morph morph')

  let apply :
      type a b l r.
      b C.obj -> (a, b, l * r) C.morph -> (a, l * r) mode -> (b, l * r) mode =
   fun dst morph m ->
    match m with
    | Amode a -> Amode (C.apply dst morph a)
    | Amodevar mv -> Amodevar (apply_morphvar dst morph mv)
    | Amodejoin (a, vs) ->
      Amodejoin (C.apply dst morph a, VarMap.map (apply_morphvar dst morph) vs)
    | Amodemeet (a, vs) ->
      Amodemeet (C.apply dst morph a, VarMap.map (apply_morphvar dst morph) vs)

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let update_lower (type a) ~log (obj : a C.obj) v a =
    (match log with
    | None -> ()
    | Some log -> log := Clower (v, v.lower) :: !log);
    v.lower <- C.join obj v.lower a

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let update_upper (type a) ~log (obj : a C.obj) v a =
    (match log with
    | None -> ()
    | Some log -> log := Cupper (v, v.upper) :: !log);
    v.upper <- C.meet obj v.upper a

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vlower ~log v vlower =
    (match log with
    | None -> ()
    | Some log -> log := Cvlower (v, v.vlower) :: !log);
    v.vlower <- vlower

  let submode_cv : type a. log:_ -> a C.obj -> a -> a var -> (unit, a) Result.t
      =
    fun (type a) ~log (obj : a C.obj) a' v ->
     if C.le obj a' v.lower
     then Ok ()
     else if not (C.le obj a' v.upper)
     then Error v.upper
     else (
       update_lower ~log obj v a';
       if C.le obj v.upper v.lower then set_vlower ~log v VarMap.empty;
       Ok ())

  let submode_cmv :
      type a l.
      log:_ -> a C.obj -> a -> (a, l * allowed) morphvar -> (unit, a) Result.t =
   fun ~log obj a (Amorphvar (v, f) as mv) ->
    let mlower = mlower obj mv in
    let mupper = mupper obj mv in
    if C.le obj a mlower
    then Ok ()
    else if not (C.le obj a mupper)
    then Error mupper
    else
      (* At this point we know [a <= f v], therefore [a] is in the downward
         closure of [f]'s image. Therefore, asking [a <= f v] is equivalent to
         asking [f' a <= v]. *)
      let f' = C.left_adjoint obj f in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      submode_cv ~log src a' v |> Result.get_ok;
      Ok ()

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
    (read: strictly higher) guess to replace the constant argument that MIGHT
    succeed. *)
  let rec submode_vc :
      type a. log:_ -> a C.obj -> a var -> a -> (unit, a) Result.t =
    fun (type a) ~log (obj : a C.obj) v a' ->
     if C.le obj v.upper a'
     then Ok ()
     else if not (C.le obj v.lower a')
     then Error v.lower
     else (
       update_upper ~log obj v a';
       let r =
         v.vlower
         |> find_error (fun mu ->
                let r = submode_mvc ~log obj mu a' in
                (if Result.is_ok r
                then
                  (* Optimization: update [v.lower] based on [mlower u].*)
                  let mu_lower = mlower obj mu in
                  if not (C.le obj mu_lower v.lower)
                  then update_lower ~log obj v mu_lower);
                r)
       in
       if C.le obj v.upper v.lower then set_vlower ~log v VarMap.empty;
       r)

  and submode_mvc :
        'a 'r.
        log:change list ref option ->
        'a C.obj ->
        ('a, allowed * 'r) morphvar ->
        'a ->
        (unit, 'a) Result.t =
   fun ~log obj (Amorphvar (v, f) as mv) a ->
    (* See [submode_cmv] for why we need the following seemingly redundant
       lines. *)
    let mupper = mupper obj mv in
    let mlower = mlower obj mv in
    if C.le obj mupper a
    then Ok ()
    else if not (C.le obj mlower a)
    then Error mlower
    else
      let f' = C.right_adjoint obj f in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      (* If [mlower] was precise, then the check
         [not (C.le obj (mlower obj mv) a)] should guarantee the following call
         to return [Ok ()]. However, [mlower] is not precise *)
      (* not using [Result.map_error] to avoid allocating closure *)
      match submode_vc ~log src v a' with
      | Ok () -> Ok ()
      | Error e -> Error (C.apply obj f e)

  let eq_morphvar :
      type a l0 r0 l1 r1. (a, l0 * r0) morphvar -> (a, l1 * r1) morphvar -> bool
      =
   fun (Amorphvar (v0, f0) as mv0) (Amorphvar (v1, f1) as mv1) ->
    (* To align l0/l1, r0/r1; The existing disallow_left/right] is for [mode],
       not [morphvar]. *)
    Morphvar.(
      disallow_left (disallow_right mv0) == disallow_left (disallow_right mv1))
    || match C.eq_morph f0 f1 with None -> false | Some Refl -> v0 == v1

  let submode_mvmv (type a) ~log (dst : a C.obj) (Amorphvar (v, f) as mv)
      (Amorphvar (u, g) as mu) =
    if C.le dst (mupper dst mv) (mlower dst mu)
    then Ok ()
    else if eq_morphvar mv mu
    then Ok ()
    else
      (* The call f v <= g u translates to three steps:
         1. f v <= g u.upper
         2. f v.lower <= g u
         3. adding g' (f v) to the u.vlower, where g' is the left adjoint of g.
      *)
      match submode_mvc ~log dst mv (mupper dst mu) with
      | Error a -> Error (a, mupper dst mu)
      | Ok () -> (
        match submode_cmv ~log dst (mlower dst mv) mu with
        | Error a -> Error (mlower dst mv, a)
        | Ok () ->
          (* At this point, we know that [f v <= g u.upper], which means [f v]
             lies within the downward closure of [g]'s image. Therefore, asking [f
             v <= g u] is equivalent to asking [g' f v <= u] *)
          let g' = C.left_adjoint dst g in
          let src = C.src dst g in
          let g'f = C.compose src g' (C.disallow_right f) in
          let x = Amorphvar (v, g'f) in
          let key = get_key x in
          if not (VarMap.mem key u.vlower)
          then set_vlower ~log u (VarMap.add key x u.vlower);
          Ok ())

  let cnt_id = ref 0

  let fresh ?upper ?lower ?vlower obj =
    let id = !cnt_id in
    cnt_id := id + 1;
    let upper = Option.value upper ~default:(C.max obj) in
    let lower = Option.value lower ~default:(C.min obj) in
    let vlower = Option.value vlower ~default:VarMap.empty in
    { upper; lower; vlower; id }

  let submode (type a r l) (obj : a C.obj) (a : (a, allowed * r) mode)
      (b : (a, l * allowed) mode) ~log =
    let submode_cc ~log:_ obj left right =
      if C.le obj left right then Ok () else Error { left; right }
    in
    let submode_mvc ~log obj v right =
      Result.map_error
        (fun left -> { left; right })
        (submode_mvc ~log obj v right)
    in
    let submode_cmv ~log obj left v =
      Result.map_error
        (fun right -> { left; right })
        (submode_cmv ~log obj left v)
    in
    let submode_mvmv ~log obj v u =
      Result.map_error
        (fun (left, right) -> { left; right })
        (submode_mvmv ~log obj v u)
    in
    match a, b with
    | Amode left, Amode right -> submode_cc ~log obj left right
    | Amodevar v, Amode right -> submode_mvc ~log obj v right
    | Amode left, Amodevar v -> submode_cmv ~log obj left v
    | Amodevar v, Amodevar u -> submode_mvmv ~log obj v u
    | Amode a, Amodemeet (b, mvs) ->
      Result.bind (submode_cc ~log obj a b) (fun () ->
          find_error (fun mv -> submode_cmv ~log obj a mv) mvs)
    | Amodevar mv, Amodemeet (b, mvs) ->
      Result.bind (submode_mvc ~log obj mv b) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log obj mv mv') mvs)
    | Amodejoin (a, mvs), Amode b ->
      Result.bind (submode_cc ~log obj a b) (fun () ->
          find_error (fun mv' -> submode_mvc ~log obj mv' b) mvs)
    | Amodejoin (a, mvs), Amodevar mv ->
      Result.bind (submode_cmv ~log obj a mv) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log obj mv' mv) mvs)
    | Amodejoin (a, mvs), Amodemeet (b, mus) ->
      (* TODO: mabye create a intermediate variable? *)
      Result.bind (submode_cc ~log obj a b) (fun () ->
          Result.bind
            (find_error (fun mv -> submode_mvc ~log obj mv b) mvs)
            (fun () ->
              Result.bind
                (find_error (fun mu -> submode_cmv ~log obj a mu) mus)
                (fun () ->
                  find_error
                    (fun mu ->
                      find_error (fun mv -> submode_mvmv ~log obj mv mu) mvs)
                    mus)))

  let cons_dedup x xs = VarMap.add (get_key x) x xs

  let union_prefer_left t0 t1 = VarMap.union (fun _ a _b -> Some a) t0 t1

  let join (type a r) obj l =
    let rec loop :
        a ->
        (a, allowed * disallowed) morphvar VarMap.t ->
        (a, allowed * r) mode list ->
        (a, allowed * disallowed) mode =
     fun a mvs rest ->
      if C.le obj (C.max obj) a
      then Amode (C.max obj)
      else
        match rest with
        | [] -> Amodejoin (a, mvs)
        | mv :: xs -> (
          match disallow_right mv with
          | Amode b -> loop (C.join obj a b) mvs xs
          (* some minor optimization: if [a] is lower than [mlower mv], we
              should keep the latter instead. This helps to fail early in
             [submode] *)
          | Amodevar mv ->
            loop (C.join obj a (mlower obj mv)) (cons_dedup mv mvs) xs
          | Amodejoin (b, mvs') ->
            loop (C.join obj a b) (union_prefer_left mvs' mvs) xs)
    in
    loop (C.min obj) VarMap.empty l

  let meet (type a l) obj l =
    let rec loop :
        a ->
        (a, disallowed * allowed) morphvar VarMap.t ->
        (a, l * allowed) mode list ->
        (a, disallowed * allowed) mode =
     fun a mvs rest ->
      if C.le obj a (C.min obj)
      then Amode (C.min obj)
      else
        match rest with
        | [] -> Amodemeet (a, mvs)
        | mv :: xs -> (
          match disallow_left mv with
          | Amode b -> loop (C.meet obj a b) mvs xs
          (* some minor optimization: if [a] is higher than [mupper mv], we
              should keep the latter instead. This helps to fail early in
             [submode_log] *)
          | Amodevar mv ->
            loop (C.meet obj a (mupper obj mv)) (cons_dedup mv mvs) xs
          | Amodemeet (b, mvs') ->
            loop (C.meet obj a b) (union_prefer_left mvs' mvs) xs)
    in
    loop (C.max obj) VarMap.empty l

  let get_loose_ceil : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match m with
    | Amode a -> a
    | Amodevar mv -> mupper obj mv
    | Amodemeet (a, mvs) ->
      VarMap.fold (fun _ mv acc -> C.meet obj acc (mupper obj mv)) mvs a
    | Amodejoin (a, mvs) ->
      VarMap.fold (fun _ mv acc -> C.join obj acc (mupper obj mv)) mvs a

  let get_loose_floor : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match m with
    | Amode a -> a
    | Amodevar mv -> mlower obj mv
    | Amodejoin (a, mvs) ->
      VarMap.fold (fun _ mv acc -> C.join obj acc (mupper obj mv)) mvs a
    | Amodemeet (a, mvs) ->
      VarMap.fold (fun _ mv acc -> C.meet obj acc (mlower obj mv)) mvs a

  (* Due to our biased implementation, the ceil is precise. *)
  let get_ceil = get_loose_ceil

  let zap_to_ceil : type a l. a C.obj -> (a, l * allowed) mode -> log:_ -> a =
   fun obj m ~log ->
    let ceil = get_ceil obj m in
    submode obj (Amode ceil) m ~log |> Result.get_ok;
    ceil

  (** Zap [mv] to its lower bound. Returns the [log] of the zapping, in
      case the caller are only interested in the lower bound and wants to
      reverse the zapping.

      As mentioned in [var], [mlower mv] is not precise; to get the precise
      lower bound of [mv], we call [submode mv (mlower mv)]. This will propagate
      to all its children, which might fail because some children's lower bound
      [a] is more up-to-date than [mv]. In that case, we call [submode mv a]. We
      repeat this process until no failure, and we will get the precise lower
      bound.

      The loop is guaranteed to terminate, because for each iteration our
      guessed lower bound is strictly higher; and all lattices are finite.
      *)
  let zap_to_floor_morphvar_aux (type a r) (obj : a C.obj)
      (mv : (a, allowed * r) morphvar) =
    let rec loop lower =
      let log = ref empty_changes in
      let r = submode_mvc ~log:(Some log) obj mv lower in
      match r with
      | Ok () -> !log, lower
      | Error a ->
        undo_changes !log;
        loop (C.join obj a lower)
    in
    loop (mlower obj mv)

  (** Zaps a morphvar to its floor and returns the floor. [commit] could be
      [Some log], in which case the zapping is appended to [log]; it could also
      be [None], in which case the zapping is reverted. The latter is useful
      when the caller only wants to know the floor without zapping. *)
  let zap_to_floor_morphvar obj mv ~commit =
    let log_, lower = zap_to_floor_morphvar_aux obj mv in
    (match commit with
    | None -> undo_changes log_
    | Some log -> log := append_changes !log log_);
    lower

  let zap_to_floor : type a r. a C.obj -> (a, allowed * r) mode -> log:_ -> a =
   fun obj m ~log ->
    match m with
    | Amode a -> a
    | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:log
    | Amodejoin (a, mvs) ->
      let floor =
        VarMap.fold
          (fun _ mv acc ->
            C.join obj acc (zap_to_floor_morphvar obj mv ~commit:None))
          mvs a
      in
      VarMap.iter
        (fun _ mv -> submode_mvc obj mv floor ~log |> Result.get_ok)
        mvs;
      floor

  let get_floor : type a r. a C.obj -> (a, allowed * r) mode -> a =
   fun obj m ->
    match m with
    | Amode a -> a
    | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:None
    | Amodejoin (a, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          C.join obj acc (zap_to_floor_morphvar obj mv ~commit:None))
        mvs a

  let print :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?verbose (obj : a C.obj) ppf m ->
    let ceil = get_loose_ceil obj m in
    let floor = get_loose_floor obj m in
    if C.le obj ceil floor
    then C.print obj ppf ceil
    else print_raw ?verbose obj ppf m

  let newvar obj = Amodevar (Amorphvar (fresh obj, C.id))

  let newvar_above (type a r) (obj : a C.obj) (m : (a, allowed * r) mode) =
    match disallow_right m with
    | Amode a ->
      if C.le obj (C.max obj) a
      then Amode a, false
      else Amodevar (Amorphvar (fresh ~lower:a obj, C.id)), true
    | Amodevar mv ->
      (* [~lower] is not precise (because [mlower mv] is not precise), but
         it doesn't need to be *)
      ( Amodevar
          (Amorphvar
             ( fresh ~lower:(mlower obj mv)
                 ~vlower:(VarMap.singleton (get_key mv) mv)
                 obj,
               C.id )),
        true )
    | Amodejoin (a, mvs) ->
      (* [~lower] is not precise here, but it doesn't need to be *)
      Amodevar (Amorphvar (fresh ~lower:a ~vlower:mvs obj, C.id)), true

  let newvar_below (type a l) (obj : a C.obj) (m : (a, l * allowed) mode) =
    match disallow_left m with
    | Amode a ->
      if C.le obj a (C.min obj)
      then Amode a, false
      else Amodevar (Amorphvar (fresh ~upper:a obj, C.id)), true
    | Amodevar mv ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id) in
      submode_mvmv obj ~log:None mu mv |> Result.get_ok;
      allow_left (Amodevar mu), true
    | Amodemeet (a, mvs) ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id) in
      submode_mvc obj ~log:None mu a |> Result.get_ok;
      VarMap.iter
        (fun _ mv -> submode_mvmv obj ~log:None mu mv |> Result.get_ok)
        mvs;
      allow_left (Amodevar mu), true
end
[@@inline always]
