open Primitive;;
open Basic_dt;;

module F = Feature;;
module T = Tp;;
module V = Value;;

open Label;;
type vec = bool array
;;

open Cctx;;
module RealSample = struct
  let values_to_vec_ {args; qv; fset; _} values label =
    let vars = args @ qv in
    let make_env vars value = List.fold_left (fun m ((_, name), v) ->
        StrMap.add name v m
      ) StrMap.empty (List.combine vars value) in
    List.map (fun vec -> vec, label) @@
    List.map (fun value ->
        let m = make_env vars value in
        Array.of_list @@
        List.map (fun feature -> F.eval feature m) fset) values

  let values_to_vec ctx args_values label =
    let qv_values = List.map (fun i -> V.I i) Randomgen.paddled_small_nums in
    let qvs_values = List.choose_n qv_values (List.length ctx.qv) in
    let values = List.map (fun (a, b) -> a @ b) @@ List.cross args_values qvs_values in
    values_to_vec_ ctx values label
end
;;

let add_vecs_always ctx vecs =
  List.iter (fun (v, label) -> Hashtbl.add ctx.fvtab v label) vecs

let add_vecs_if_new ctx vecs =
  List.iter (fun (v, label) ->
      match Hashtbl.find_opt ctx.fvtab v with
      | Some _ -> ()
      | None -> Hashtbl.add ctx.fvtab v label) vecs

let layout_bool x = if x then "✓" else "𐄂"

let layout_vecs vecs =
  List.fold_lefti (fun table i (vec, label) ->
      let vec = List.split_by_comma layout_bool (Array.to_list vec) in
      Printf.sprintf "%s\n%s: %s [%i]" table (layout_label label) vec i
    ) "" vecs

let layout_fvctx {args; qv; fset; fvtab} =
  let args = Printf.sprintf "args: %s\n" @@ List.split_by_comma Tp.layouttvar args in
  let qv = Printf.sprintf "qv: %s\n" @@ List.split_by_comma Tp.layouttvar qv in
  let fset = Printf.sprintf "fset: %s\n" @@ F.layout_set fset in
  let labeled_vecs = layout_vecs @@ List.of_seq @@ Hashtbl.to_seq fvtab in
  Printf.sprintf "%s%s%s%s\n" args qv fset labeled_vecs

(* let remove_field {fset; vecs} target = *)
(*   let extract_feature = function *)
(*     | F.Pr (pred, [x], _) -> pred, x *)
(*     | F.Pr (_, _, _) -> raise @@ UndefExn "feature_vector_remove_field" *)
(*     | F.Base ("==", x, _) -> "==", x *)
(*     | F.Base (_, _, _) -> raise @@ InterExn "never happen(remove_field)" *)
(*     | F.Bo x -> "id", x *)
(*   in *)
(*   let farr = Array.of_list fset in *)
(*   let op, x = extract_feature target in *)
(*   let indicator = Array.init (Array.length farr) (fun i -> *)
(*       let op', x' = extract_feature farr.(i) in *)
(*       (String.equal op op') && (x == x') *)
(*     ) in *)
(*   let fset' = List.filter_mapi (fun i feature -> *)
(*       if indicator.(i) then None else Some feature *)
(*     ) fset in *)
(*   let vecs' = *)
(*     List.map (fun vec -> *)
(*         List.filter_mapi (fun i b -> *)
(*             if indicator.(i) then None else Some b *)
(*           ) vec) vecs in *)
(*   {fset = fset'; vecs = vecs'} *)
(* let get_field {fset; vecs} feature = *)
(*   let i = List.lookup (fun f1 f2 -> F.eq f1 f2) feature fset in *)
(*   List.map (fun vec -> List.nth vec i) vecs *)

(* let split feature_vectors target = *)
(*   let y = get_field feature_vectors target in *)
(*   let feature_vectors = remove_field feature_vectors target in *)
(*   let bv_tbl = Hashtbl.create (List.length feature_vectors.vecs) in *)
(*   let _ = List.iter (fun (y, bv) -> *)
(*       match Hashtbl.find_opt bv_tbl bv with *)
(*       | Some Unclear -> () *)
(*       | Some Pos -> if y then () else Hashtbl.replace bv_tbl bv Unclear *)
(*       | Some Neg -> if y then Hashtbl.replace bv_tbl bv Unclear else () *)
(*       | None -> Hashtbl.add bv_tbl bv (if y then Pos else Neg) *)
(*     ) (List.combine y feature_vectors.vecs) in *)
(*   let by_value v = *)
(*     Hashtbl.fold (fun k v' r -> *)
(*         if label_eq (v, v') *)
(*         then k :: r *)
(*         else r) bv_tbl [] *)
(*   in *)
(*   let pos, neg, unclear = by_value Pos, by_value Neg, by_value Unclear in *)
(*   (\* let dup = List.interset (fun l1 l2 -> List.eq (fun x y -> x == y) l1 l2) pos neg in *)
(*    * let _ = List.iter (fun l -> Printf.printf "%s\n" (boollist_to_string l)) dup in *\) *)
(*   let add_y y vecs = List.map (fun v -> (y, v)) vecs in *)
(*   if List.length unclear == 0 *)
(*   then Single {dfset = feature_vectors.fset; *)
(*                labeled_vecs = (add_y true pos) @ (add_y false neg)} *)
(*   else Double ({dfset = feature_vectors.fset; *)
(*                 labeled_vecs = (add_y true (pos @ unclear)) @ (add_y false neg)}, *)
(*                {dfset = feature_vectors.fset; *)
(*                 labeled_vecs = (add_y true pos) @ (add_y false (neg @ unclear))}) *)
