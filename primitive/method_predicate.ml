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

let ord_apply = function
  | [V.L l; V.I e1; V.I e2] -> List.order (==) l e1 e2
  | _ -> raise @@ failwith "ord_apply"

let left_apply = function
  | [V.T t; V.I e1; V.I e2] -> Tree.left_child (==) t e1 e2
  | [V.TI t; V.I e1; V.I e2] -> LabeledTree.left_child (==) t e1 e2
  | [V.TB t; V.I e1; V.I e2] -> LabeledTree.left_child (==) t e1 e2
  | _ -> raise @@ failwith "left_apply"

let right_apply = function
  | [V.T t; V.I e1; V.I e2] -> Tree.right_child (==) t e1 e2
  | [V.TI t; V.I e1; V.I e2] -> LabeledTree.right_child (==) t e1 e2
  | [V.TB t; V.I e1; V.I e2] -> LabeledTree.right_child (==) t e1 e2
  | _ -> raise @@ failwith "right_apply"

let parallel_apply = function
  | [V.T t; V.I e1; V.I e2] -> Tree.parallel_child (==) t e1 e2
  | [V.TI t; V.I e1; V.I e2] -> LabeledTree.parallel_child (==) t e1 e2
  | [V.TB t; V.I e1; V.I e2] -> LabeledTree.parallel_child (==) t e1 e2
  | _ -> raise @@ failwith "parallel_apply"

let lt_apply = function
  | [V.I a; V.I b] -> a < b
  | _ -> raise @@ failwith "lt_apply"

let eq_apply = function
  | [V.I a; V.I b] -> a == b
  | _ -> raise @@ failwith "eq_apply"

let mem_info =
  let poly_name = "mem" in
  [{poly_name = poly_name; name="list_mem"; tps = [T.IntList; T.Int]; permu=false; imp = mem_apply};
   {poly_name = poly_name; name="tree_mem"; tps = [T.IntTree; T.Int]; permu=false; imp = mem_apply};
   {poly_name = poly_name; name="treei_mem"; tps = [T.IntTreeI; T.Int]; permu=false; imp = mem_apply};
   {poly_name = poly_name; name="treeb_mem"; tps = [T.IntTreeB; T.Int]; permu=false; imp = mem_apply};]

let hd_info =
  let poly_name = "hd" in
  [{poly_name = poly_name; name="list_hd"; tps = [T.IntList; T.Int]; permu=false; imp = hd_apply};
   {poly_name = poly_name; name="tree_hd"; tps = [T.IntTree; T.Int]; permu=false; imp = hd_apply};
   {poly_name = poly_name; name="treei_hd"; tps = [T.IntTreeI; T.Int]; permu=false; imp = hd_apply};
   {poly_name = poly_name; name="treeb_hd"; tps = [T.IntTreeB; T.Int]; permu=false; imp = hd_apply};]

let ord_info =
  let poly_name = "ord" in
  [{poly_name = poly_name; name="list_ord"; tps = [T.IntList; T.Int]; permu=false; imp = ord_apply};]

let left_info =
  let poly_name = "left" in
  [{poly_name = poly_name; name="tree_left"; tps = [T.IntTree; T.Int]; permu=false; imp = left_apply};
   {poly_name = poly_name; name="treei_left"; tps = [T.IntTreeI; T.Int]; permu=false; imp = left_apply};
   {poly_name = poly_name; name="treeb_left"; tps = [T.IntTreeB; T.Int]; permu=false; imp = left_apply};]

let right_info =
  let poly_name = "right" in
  [{poly_name = poly_name; name="tree_right"; tps = [T.IntTree; T.Int]; permu=false; imp = right_apply};
   {poly_name = poly_name; name="treei_right"; tps = [T.IntTreeI; T.Int]; permu=false; imp = right_apply};
   {poly_name = poly_name; name="treeb_right"; tps = [T.IntTreeB; T.Int]; permu=false; imp = right_apply};]

let para_info =
  let poly_name = "para" in
  [{poly_name = poly_name; name="tree_para"; tps = [T.IntTree; T.Int]; permu=false; imp = parallel_apply};
   {poly_name = poly_name; name="treei_para"; tps = [T.IntTreeI; T.Int]; permu=false; imp = parallel_apply};
   {poly_name = poly_name; name="treeb_para"; tps = [T.IntTreeB; T.Int]; permu=false; imp = parallel_apply};]

let lt_info =
  let poly_name = "<" in
  [{poly_name = poly_name; name="<"; tps = [T.Int; T.Int]; permu=false; imp = lt_apply};]

let eq_info =
  let poly_name = "==" in
  [{poly_name = poly_name; name="=="; tps = [T.Int; T.Int]; permu=false; imp = eq_apply};]

let mp_table =
  mem_info @ hd_info @ lt_info @ eq_info @ ord_info @ left_info @ right_info @ para_info

let imp_map = List.fold_left (fun m r -> StrMap.add r.name r.imp m) StrMap.empty mp_table

let find_info_by_polyname_tps poly_name tps =
  match List.find_opt (fun r -> Tp.tps_eq r.tps tps && String.equal r.poly_name poly_name) mp_table with
  | Some r -> r
  | None -> raise @@ failwith
      (Printf.sprintf "cannot find method predicate(%s) with tps(%s)" poly_name (List.split_by_comma Tp.layout tps))

let find_info_by_name name =
  match List.find_opt (fun r -> String.equal name r.name) mp_table with
  | Some r -> r
  | None -> raise @@ failwith
      (Printf.sprintf "cannot find method predicate(%s)" name)

let instantization poly_name tps =
  let r = find_info_by_polyname_tps poly_name tps in
  r.name

let poly_name name =
  let r = find_info_by_name name in
  r.poly_name

let apply mp args = (StrMap.find "method predicate apply" imp_map mp) args
