module T = Tp;;
module V = Value;;
open Basic_dt;;
type t = string
type pred_info = {name:string;
                  poly_name: string;
                  tps: T.t list;
                  permu: bool;
                  imp: V.t list -> bool}
type poly_tp =
  | Elem
  | Dt
type poly_pred_info = {poly_tps: poly_tp list;}

let hd_apply  = function
  | [V.L l; V.I e] ->
    (match l with
     | [] -> false
     | h :: _ -> h == e)
  | [V.T t; V.I e] ->
    (match t with
     | Tree.Leaf -> false
     | Tree.Node (root, _, _) -> root == e)
  | [V.TI t; V.I e] ->
    (match t with
     | LabeledTree.Leaf -> false
     | LabeledTree.Node (_, root, _, _) -> root == e)
  | [V.TB t; V.I e] ->
    (match t with
     | LabeledTree.Leaf -> false
     | LabeledTree.Node (_, root, _, _) -> root == e)
  | _ -> raise @@ failwith "head_apply"

let mem_apply = function
  | [V.L l; V.I e] -> List.exists (fun x -> x == e) l
  | [V.T t; V.I e] -> Tree.exists (fun x -> x == e) t
  | [V.TI t; V.I e] -> LabeledTree.exists (fun x -> x == e) t
  | [V.TB t; V.I e] -> LabeledTree.exists (fun x -> x == e) t
  | _ -> raise @@ failwith "member_apply"

let lt_apply = function
  | [V.I a; V.I b] -> a < b
  | _ -> raise @@ failwith "lt_apply"

let eq_apply = function
  | [V.I a; V.I b] -> a == b
  | _ -> raise @@ failwith "eq_apply"

let mem_info =
  let poly_name = "mem" in
  [{poly_name = poly_name; name="list_mem"; tps = [T.IntList; T.Int]; permu=false; imp = mem_apply};
   {poly_name = poly_name; name="tree_mem"; tps = [T.IntTree; T.Int]; permu=false; imp = mem_apply};]

let hd_info =
  let poly_name = "hd" in
  [{poly_name = poly_name; name="list_hd"; tps = [T.IntList; T.Int]; permu=false; imp = hd_apply};
   {poly_name = poly_name; name="tree_hd"; tps = [T.IntTree; T.Int]; permu=false; imp = hd_apply};]

let lt_info =
  let poly_name = "lt" in
  [{poly_name = poly_name; name="<"; tps = [T.Int; T.Int]; permu=false; imp = lt_apply};]

let eq_info =
  let poly_name = "eq" in
  [{poly_name = poly_name; name="=="; tps = [T.Int; T.Int]; permu=false; imp = eq_apply};]

let mp_table = mem_info @ hd_info @ lt_info

let imp_map = List.fold_left (fun m r -> StrMap.add r.name r.imp m) StrMap.empty mp_table

let find_info_by_polyname_tps poly_name tps =
  match List.find_opt (fun r -> Tp.tps_eq r.tps tps && String.equal r.poly_name poly_name) mp_table with
  | Some r -> r
  | None -> raise @@ failwith
      (Printf.sprintf "cannot find method predicate(%s) with tps(%s)" poly_name (List.split_by_comma Tp.layout tps))
let instantization poly_name tps =
  let r = find_info_by_polyname_tps poly_name tps in
  r.name

let apply mp args = (StrMap.find "method predicate apply" imp_map mp) args
