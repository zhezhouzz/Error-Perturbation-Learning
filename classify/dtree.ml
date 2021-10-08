module Epr = Specification.Specast
open Primitive;;
module P = Method_predicate
module T = Tp
module F = Feature
module FV = Feature_vector
module FastDT = Ml.FastDT.FastDT
open Cctx;;
open Basic_dt
open Printf
open Label
type value = Value.t
type feature = Feature.t
type feature_set = Feature.set

type 'a t =
  | T
  | F
  | Leaf of 'a
  | Node of 'a * 'a t * 'a t

let eval (dt: feature t) m : bool =
  let rec aux = function
    | T -> true
    | F -> false
    | Leaf feature -> F.eval feature m
    | Node (feature, l, r) ->
      if F.eval feature m then aux l else aux r
  in
  aux dt

let eval_vector (dt: feature t) (fl: feature list) (vec: bool list) : bool =
  let m = List.combine fl vec in
  let get_b f = snd @@ List.find "eval_raw" (fun (f', _) -> F.eq f f') m in
  let rec aux = function
    | T -> true
    | F -> false
    | Leaf feature -> get_b feature
    | Node (feature, l, r) ->
      if get_b feature then aux l else aux r
  in
  aux dt

let eval_vector_idx (dt: int t) (vec: bool array) : bool =
  let nth i = try vec.(i) with
    | _ -> raise @@ failwith "eval_vector_idx"
  in
  let rec aux = function
    | T -> true
    | F -> false
    | Leaf feature -> nth feature
    | Node (feature, l, r) ->
      if nth feature then aux l else aux r
  in
  aux dt

let rec layout = function
  | T -> "⊤"
  | F -> "⊥"
  | Leaf feature -> F.layout feature
  | Node (feature, l, r) ->
    sprintf "[%s](%s,%s)" (F.layout feature) (layout l) (layout r)

(* let get_vars (dtree: feature t) = *)
(*   let rec aux = function *)
(*     | T | F -> [], [] *)
(*     | Leaf feature -> F.get_vars feature *)
(*     | Node (feature, l, r) -> *)
(*       let dts, elems = F.get_vars feature in *)
(*       let (dts', elems'), (dts'', elems'') = map_double aux (l, r) in *)
(*       dts @ dts' @ dts'', elems @ elems' @ elems'' *)
(*   in *)
(*   map_double List.remove_duplicates_eq (aux dtree) *)

let to_prop dtree =
  let rec aux = function
    | T -> Epr.True
    | F -> Epr.Not Epr.True
    | Leaf feature -> F.to_prop feature
    | Node (feature, l, r) -> Epr.Ite (F.to_prop feature, aux l, aux r)
  in
  aux dtree

let to_epr_idx dtree vars =
  let nth i = match List.nth_opt vars i with
    | None -> raise @@ failwith "to_epr_idx"
    | Some b -> b
  in
  let rec aux = function
    | T -> Epr.True
    | F -> Epr.Not Epr.True
    | Leaf feature -> nth feature
    | Node (feature, l, r) -> Epr.Ite (nth feature, aux l, aux r)
  in
  aux dtree

(* TODO: handle types in predicates *)
let to_forallformula (qv: T.tvar list) (dtree: feature t) =
  qv, to_prop dtree
  (* let dts, elems = get_vars dtree in *)
  (* let vars = List.map (fun v -> T.Int, v) (dts @ elems) in *)
  (* vars, to_epr dtree *)

let to_spec (args:  T.tvar list) (qv: T.tvar list) (dtree: feature t) =
  Specification.Spec.({args = args; qv = qv; body = to_prop dtree})
  (* let dts, elems = get_vars dtree in *)
  (* let dts = List.map (fun v -> T.Int, v) dts in *)
  (* let elems = List.map (fun v -> T.Int, v) elems in *)
  (* dts, (elems, to_epr dtree) *)


let of_fastdt dt feature_set =
  let rec aux = function
    | FastDT.Leaf {c_t; c_f} ->
      let res =
        if (Float.abs c_t) < 1e-3 then F
        else if (Float.abs c_f) < 1e-3 then T
        else raise @@ failwith (sprintf "Bad Dt Result(%f, %f)" c_t c_f)
      in
      (* let _ = Printf.printf "leaf(%f,%f) ->(%f,%f,%f) = %s\n" c_t c_f
       *     (Float.abs c_t) (Float.abs c_f) (1e-3) (layout res) in *)
      res
    | FastDT.Node ({split;if_t;if_f}) ->
      match List.nth_opt feature_set split with
      | None -> raise @@ failwith "Bad Dt Result"
      | Some p -> Node (p, aux if_t, aux if_f)
  in
  aux dt

let of_fastdt_idx dt =
  let rec aux = function
    | FastDT.Leaf {c_t; c_f} ->
      let res =
        if (Float.abs c_t) < 1e-3 then F
        else if (Float.abs c_f) < 1e-3 then T
        else raise @@ failwith (sprintf "Bad Dt Result(%f, %f)" c_t c_f)
      in
      (* let _ = Printf.printf "leaf(%f,%f) ->(%f,%f,%f) = %s\n" c_t c_f
       *     (Float.abs c_t) (Float.abs c_f) (1e-3) (layout res) in *)
      res
    | FastDT.Node ({split;if_t;if_f}) ->
      Node (split, aux if_t, aux if_f)
  in
  aux dt

let len dt =
  let rec aux = function
    | T -> 0
    | F -> 0
    | Leaf _ -> 1
    | Node (_, l, r) -> aux l + aux r + 1
  in
  aux dt

let dt_summary dt fset =
  let len = (List.length fset) in
  let fv_arr = Array.init len (fun _ -> false) in
  let rec next idx =
    if idx >= len then None
    else if not (fv_arr.(idx)) then (Array.set fv_arr idx true; Some idx)
    else
      match next (idx + 1) with
      | None -> None
      | Some _ -> (Array.set fv_arr idx false; Some 0)
  in
  let posnum = ref 0 in
  let negnum = ref 0 in
  let rec aux idx =
    let fvec = Array.to_list fv_arr in
    let _ = Printf.printf "iter:%s\n" (List.split_by_comma string_of_bool fvec) in
    let _ = if eval_vector dt fset fvec
      then posnum := !posnum + 1
      else negnum := !negnum + 1
    in
    match next idx with
    | None -> ()
    | Some idx -> aux idx
  in
  (aux 0; (!posnum, !negnum))

let classify_ fset samples =
  let dt = FastDT.make_dt ~samples:samples ~max_d:50 in
  (* let _ = FastDT.print_tree' dt in *)
  (* let _ = if List.length labeled_vecs >= 3 then raise @@ failwith "class" else () in *)
  let res = of_fastdt dt fset in
  (* let posnum, negnum = dt_summary res dfeature_set in *)
  (* let _ = Printf.printf "summary: %i|%i\n" posnum negnum in *)
  res

let classify_hash fset htab is_pos =
  let samples = Array.init (Hashtbl.length htab)
      (fun _ -> false, Array.init (List.length fset) (fun _ -> false)) in
  let iter = ref 0 in
  let _ =
    Hashtbl.iter (fun f v ->
        let _ =
          if is_pos v
          then Array.set samples !iter (true, f)
          else Array.set samples !iter (false,f)
        in
        iter := !iter + 1
      ) htab
  in
  let dt = FastDT.make_dt ~samples:samples ~max_d:500 in
  let res = of_fastdt dt fset in
  let res_idx = of_fastdt_idx dt in
  res, res_idx

let classify ?(is_pos = Label.is_pos) ctx =
  classify_hash ctx.fset ctx.fvtab is_pos

let two_dt f fset dt1 dt2 =
  let len = List.length fset in
  let fv_arr = Array.init len (fun _ -> false) in
  let rec next idx =
    if idx >= len then None
    else if not (fv_arr.(idx)) then (Array.set fv_arr idx true; Some idx)
    else
      match next (idx + 1) with
      | None -> None
      | Some _ -> (Array.set fv_arr idx false; Some 0)
  in
  let ftab = Hashtbl.create 10000 in
  let rec aux idx =
    let fvec = fv_arr in
    (* let _ = Printf.printf "iter:%s\n" (boollist_to_string fvec) in *)
    let dt1_b = eval_vector_idx dt1 fvec in
    let dt2_b = eval_vector_idx dt2 fvec in
    let _ = if f dt1_b dt2_b then
        Hashtbl.add ftab fvec Pos
      else
        Hashtbl.add ftab fvec Neg
    in
    match next idx with
    | None -> ()
    | Some idx -> aux idx
  in
  (aux 0; classify_hash fset ftab is_pos)

let merge_dt fset dt1 dt2 =
  two_dt (fun dt1_b dt2_b -> dt1_b || dt2_b) fset dt1 dt2

let subtract_dt fset dt1 dt2 =
  two_dt (fun dt1_b dt2_b -> dt1_b && not dt2_b) fset dt1 dt2

open Yojson.Basic
let rec encode (encoder: 'a -> Yojson.Basic.t) tree =
  match tree with
  | T -> `Bool true
  | F -> `Bool false
  | Leaf a -> `Assoc ["fd", `String "Leaf"; "a", encoder a]
  | Node (a, l, r) ->
    `Assoc ["fd", `String "Node";
            "a", encoder a; "l", encode encoder l; "r", encode encoder r]

let rec decode decoder json =
  match json with
  | `Bool b -> if b then T else F
  | _ ->
    let fd = json |> Util.member "fd" |> Util.to_string in
    if String.equal fd "Leaf" then
      let a = json |> Util.member "a" |> decoder in
      Leaf a
    else if String.equal fd "Node" then
      let a = json |> Util.member "a" |> decoder in
      let l = json |> Util.member "l" |> decode decoder in
      let r = json |> Util.member "r" |> decode decoder in
      Node (a, l, r)
    else raise @@ failwith "Dt decode"

let label_encode = function
  | Pos -> `Int 0
  | MayNeg -> `Int 1
  | Neg -> `Int 2

let label_decode = function
  | `Int 0 -> Pos
  | `Int 1 -> MayNeg
  | `Int 2 -> Neg
  | _ -> raise @@ failwith "label_decode"

let label_eq a b =
  match a, b with
  | Pos, Pos | Neg, Neg | MayNeg, MayNeg -> true
  | _ -> false

let fvtab_eq_ tab tab' =
  Hashtbl.iter (fun vec label ->
      match Hashtbl.find_opt tab' vec with
      | Some label' ->
        if label_eq label label' then ()
        else raise @@ failwith "fvtab_eq_:label"
      | _ -> raise @@ failwith "fvtab_eq_:vec not found"
    ) tab

let fvtab_eq tab tab' =
  (fvtab_eq_ tab tab'; fvtab_eq_ tab' tab)

let map m dt =
  let rec aux = function
    | T -> T
    | F -> F
    | Leaf i ->
      (match IntMap.find_opt i m with
       | None -> raise @@ failwith "dt map"
       | Some i' -> Leaf i')
    | Node (i, l, r) ->
      (match IntMap.find_opt i m with
       | None -> raise @@ failwith "dt map"
       | Some i' -> Node (i', aux l, aux r))
  in
  aux dt

let count size dt =
  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n ->
      let b = pow a (n / 2) in
      b * b * (if n mod 2 = 0 then 1 else a) in
  let rec aux prefix = function
    | T -> pow 2 (size - (List.length prefix))
    | F -> 0
    | Leaf _ -> raise @@ failwith "never happen"
    | Node (x, l, r) ->
      if List.exists (fun y -> y == x) prefix
      then raise @@ failwith "never happen"
      else (aux (x :: prefix) l) + (aux (x :: prefix) r)
  in
  aux [] dt
