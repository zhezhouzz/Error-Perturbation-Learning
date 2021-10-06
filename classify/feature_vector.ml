open Primitive;;
open Basic_dt;;

module F = Feature;;
module T = Tp;;

type t = {args: T.tvar list; qv: T.tvar list; fset: F.set;
          labeled_vecs: (bool * (bool array)) list}
;;


module RealSample = struct
  (* type value = Value.t *)
  (* type sample = {vars: Tp.tvar list; *)
  (*                values: (value list) list} *)
  (* let append s1 s2 = *)
  (*   {vars = s1.vars @ s2.vars; *)
  (*    values = List.map (fun (a, b) -> a @ b) @@ List.cross s1.values s2.values} *)
  let add_to_feature_vectors {args; qv; fset; labeled_vecs} judge args_values qv_values =
    let values = List.map (fun (a, b) -> a @ b) @@ List.cross args_values qv_values in
    let vars = args @ qv in
    let make_env vars value = List.fold_left (fun m ((_, name), v) ->
        StrMap.add name v m
      ) StrMap.empty (List.combine vars value) in
    let ss = List.map (fun value ->
        let m = make_env vars value in
        Array.of_list @@
        List.map (fun feature -> F.eval feature m) fset) values in
    let hashtbl = Hashtbl.create (List.length ss) in
    let _ = List.iter (fun v ->
        match Hashtbl.find_opt hashtbl v with
        | Some _ -> ()
        | _ -> Hashtbl.add hashtbl v ()) ss in
    let vecs = Hashtbl.fold (fun v _ l -> (judge v, v) :: l) hashtbl [] in
    {args; qv; fset; labeled_vecs = labeled_vecs @ vecs}
end
;;
let layout {args; qv; fset; labeled_vecs} =
  let args = Printf.sprintf "args: %s\n" @@ List.split_by_comma Tp.layouttvar args in
  let qv = Printf.sprintf "qv: %s\n" @@ List.split_by_comma Tp.layouttvar qv in
  let fset = Printf.sprintf "fset: %s\n" @@ F.layout_set fset in
  let labeled_vecs = List.fold_lefti (fun table i (label, vec) ->
      let label = if label then "+" else "-" in
      let vec = List.split_by_comma (fun x -> if x then "t" else "f") (Array.to_list vec) in
      Printf.sprintf "%s\n%s: [%i] %s" table label i vec
    ) "" labeled_vecs in
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

type label = Pos | Neg | Unclear

let label_eq = function
  | Pos, Pos -> true
  | Neg, Neg -> true
  | Unclear, Unclear -> true
  | _, _ -> false

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
