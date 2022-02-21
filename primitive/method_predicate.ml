module T = Tp
module V = Value
open Basic_dt

type t = string

type pred_info = {
  name : string;
  poly_name : string;
  tps : T.t list;
  permu : bool;
  imp : V.t list -> bool;
}

type poly_tp = Elem | Dt

type poly_pred_info = { poly_tps : poly_tp list }

let empty_apply = function
  | [ V.L l ] -> ( match l with [] -> true | _ -> false)
  | [ V.T t ] -> ( Tree.(match t with Leaf -> true | _ -> false))
  | [ V.TI t ] -> ( LabeledTree.(match t with Leaf -> true | _ -> false))
  | [ V.TB t ] -> ( LabeledTree.(match t with Leaf -> true | _ -> false))
  | _ -> raise @@ failwith "member_apply"

let hd_apply = function
  | [ V.L l; V.I e ] -> ( match l with [] -> false | h :: _ -> h == e)
  | [ V.T t; V.I e ] -> (
      match t with Tree.Leaf -> false | Tree.Node (root, _, _) -> root == e)
  | [ V.TI t; V.I e ] -> (
      match t with
      | LabeledTree.Leaf -> false
      | LabeledTree.Node (_, root, _, _) -> root == e)
  | [ V.TB t; V.I e ] -> (
      match t with
      | LabeledTree.Leaf -> false
      | LabeledTree.Node (_, root, _, _) -> root == e)
  | _ -> raise @@ failwith "head_apply"

let last_apply = function
  | [ V.L l; V.I e ] -> (
      match List.last_destruct_opt l with
      | Some (_, e') -> e == e'
      | None -> false)
  | [ V.T t; V.I e ] -> Tree.last t e
  | [ V.TI t; V.I e ] -> LabeledTree.last t e
  | [ V.TB t; V.I e ] -> LabeledTree.last t e
  | _ -> raise @@ failwith "last_apply"

let mem_apply = function
  | [ V.L l; V.I e ] -> List.exists (fun x -> x == e) l
  | [ V.T t; V.I e ] -> Tree.exists (fun x -> x == e) t
  | [ V.TI t; V.I e ] -> LabeledTree.exists (fun x -> x == e) t
  | [ V.TB t; V.I e ] -> LabeledTree.exists (fun x -> x == e) t
  | [ V.Binomialhp t; V.I e ] -> BinomialHeap.mem t e
  | [ V.Skewhp t; V.I e ] -> Skewhp.mem t e
  | [ V.Pairinghp t; V.I e ] -> Pairinghp.mem t e
  | _ -> raise @@ failwith "member_apply"

let size_apply = function
  | [ V.L l; V.I e ] -> e == List.length l
  | [ V.T t; V.I e ] -> e == Tree.size t
  | [ V.TI t; V.I e ] -> e == LabeledTree.size t
  | [ V.TB t; V.I e ] -> e == LabeledTree.size t
  | _ -> raise @@ failwith "size_apply"

let size_plus1_apply = function
  | [ V.L l1; V.L l2 ] -> List.length l1 + 1 == List.length l2
  | _ -> raise @@ failwith "size_plus1_apply"

let len_apply = function
  | [ V.L l; V.I e ] -> e == List.length l
  | [ V.T t; V.I e ] -> e == Tree.deep t
  | [ V.TI t; V.I e ] -> e == LabeledTree.deep t
  | [ V.TB t; V.I e ] -> e == LabeledTree.deep t
  | _ -> raise @@ failwith "len_apply"

let ord_apply = function
  | [ V.L l; V.I e1; V.I e2 ] -> List.order ( == ) l e1 e2
  | _ -> raise @@ failwith "ord_apply"

let once_apply = function
  | [ V.L l; V.I e ] -> List.once ( = ) l e
  | _ -> raise @@ failwith "once_apply"

let complete_apply = function
  | [ V.T t; V.I e ] -> Tree.complete t e
  | _ -> raise @@ failwith "complete_apply"

let depth_eq_apply = function
  | [ V.T t1; V.T t2 ] -> Tree.deep t1 == Tree.deep t2
  | _ -> raise @@ failwith "depth_eq_apply"

let depth_plus1_apply = function
  | [ V.T t1; V.T t2 ] -> Tree.deep t1 + 1 == Tree.deep t2
  | _ -> raise @@ failwith "depth_plus1_apply"

let left_apply = function
  | [ V.T t; V.I e1; V.I e2 ] -> Tree.left_child ( == ) t e1 e2
  | [ V.TI t; V.I e1; V.I e2 ] -> LabeledTree.left_child ( == ) t e1 e2
  | [ V.TB t; V.I e1; V.I e2 ] -> LabeledTree.left_child ( == ) t e1 e2
  | _ -> raise @@ failwith "left_apply"

let right_apply = function
  | [ V.T t; V.I e1; V.I e2 ] -> Tree.right_child ( == ) t e1 e2
  | [ V.TI t; V.I e1; V.I e2 ] -> LabeledTree.right_child ( == ) t e1 e2
  | [ V.TB t; V.I e1; V.I e2 ] -> LabeledTree.right_child ( == ) t e1 e2
  | _ -> raise @@ failwith "right_apply"

let parallel_apply = function
  | [ V.T t; V.I e1; V.I e2 ] -> Tree.parallel_child ( == ) t e1 e2
  | [ V.TI t; V.I e1; V.I e2 ] -> LabeledTree.parallel_child ( == ) t e1 e2
  | [ V.TB t; V.I e1; V.I e2 ] -> LabeledTree.parallel_child ( == ) t e1 e2
  | _ -> raise @@ failwith "parallel_apply"

let left_adj_apply = function
  | [ V.T t; V.I e1; V.I e2 ] -> Tree.left_adj_child ( == ) t e1 e2
  | [ V.TI t; V.I e1; V.I e2 ] -> LabeledTree.left_adj_child ( == ) t e1 e2
  | [ V.TB t; V.I e1; V.I e2 ] -> LabeledTree.left_adj_child ( == ) t e1 e2
  | _ -> raise @@ failwith "left_adj_apply"

let right_adj_apply = function
  | [ V.T t; V.I e1; V.I e2 ] -> Tree.right_adj_child ( == ) t e1 e2
  | [ V.TI t; V.I e1; V.I e2 ] -> LabeledTree.right_adj_child ( == ) t e1 e2
  | [ V.TB t; V.I e1; V.I e2 ] -> LabeledTree.right_adj_child ( == ) t e1 e2
  | _ -> raise @@ failwith "right_adj_apply"

let parallel_adj_apply = function
  | [ V.T t; V.I e1; V.I e2 ] -> Tree.parallel_adj_child ( == ) t e1 e2
  | [ V.TI t; V.I e1; V.I e2 ] -> LabeledTree.parallel_adj_child ( == ) t e1 e2
  | [ V.TB t; V.I e1; V.I e2 ] -> LabeledTree.parallel_adj_child ( == ) t e1 e2
  | _ -> raise @@ failwith "parallel_adj_apply"

let lt_apply = function
  | [ V.I a; V.I b ] -> a < b
  | _ -> raise @@ failwith "lt_apply"

let eq_apply = function
  | [ V.I a; V.I b ] -> a == b
  | _ -> raise @@ failwith "eq_apply"

let label_is_apply b = function
  | [ V.TB t; V.I x ] ->
      LabeledTree.exists_withlabel (fun label y -> label == b && x == y) t
  | _ -> raise @@ failwith "label_is_true"

let rb_balance_apply = function
  | [ V.TB t ] -> LabeledTree.rb_balance t
  | _ -> raise @@ failwith "rb_balance"

let rb_balance2_apply = function
  | [ V.TB t1; V.TB t2 ] -> LabeledTree.rb_balance2 t1 t2
  | _ -> raise @@ failwith "rb_balance2"

let leftist_apply = function
  | [ V.TI t ] -> LabeledTree.leftists t
  | _ -> raise @@ failwith "leftist"

let binomialhp_apply = function
  | [ V.Binomialhp t ] -> BinomialHeap.binomialhp t
  | _ -> raise @@ failwith "binomialhp"

let skewhp_apply = function
  | [ V.Skewhp t ] -> Skewhp.skewhp t
  | _ -> raise @@ failwith "skew"

let pairinghp_apply = function
  | [ V.Pairinghp t ] -> Pairinghp.pairinghp t
  | _ -> raise @@ failwith "pairing"

let pairinghp_sort_apply = function
  | [ V.Pairinghp t ] -> Pairinghp.pairinghp_sort t
  | _ -> raise @@ failwith "pairing_sort"

let empty_info =
  let poly_name = "empty" in
  [
    {
      poly_name;
      name = "list_empty";
      tps = [ T.IntList ];
      permu = false;
      imp = empty_apply;
    };
    {
      poly_name;
      name = "tree_empty";
      tps = [ T.IntTree ];
      permu = false;
      imp = empty_apply;
    };
    {
      poly_name;
      name = "treei_empty";
      tps = [ T.IntTreeI ];
      permu = false;
      imp = empty_apply;
    };
    {
      poly_name;
      name = "treeb_empty";
      tps = [ T.IntTreeB ];
      permu = false;
      imp = empty_apply;
    };
  ]

let mem_info =
  let poly_name = "mem" in
  [
    {
      poly_name;
      name = "list_mem";
      tps = [ T.IntList; T.Int ];
      permu = false;
      imp = mem_apply;
    };
    {
      poly_name;
      name = "tree_mem";
      tps = [ T.IntTree; T.Int ];
      permu = false;
      imp = mem_apply;
    };
    {
      poly_name;
      name = "treei_mem";
      tps = [ T.IntTreeI; T.Int ];
      permu = false;
      imp = mem_apply;
    };
    {
      poly_name;
      name = "treeb_mem";
      tps = [ T.IntTreeB; T.Int ];
      permu = false;
      imp = mem_apply;
    };
    {
      poly_name;
      name = "binomialhp_mem";
      tps = [ T.Uninterp "binomialhp"; T.Int ];
      permu = false;
      imp = mem_apply;
    };
    {
      poly_name;
      name = "skewhp_mem";
      tps = [ T.Uninterp "skewhp"; T.Int ];
      permu = false;
      imp = mem_apply;
    };
    {
      poly_name;
      name = "pairinghp_mem";
      tps = [ T.Uninterp "pairinghp"; T.Int ];
      permu = false;
      imp = mem_apply;
    };
  ]

let hd_info =
  let poly_name = "hd" in
  [
    {
      poly_name;
      name = "list_hd";
      tps = [ T.IntList; T.Int ];
      permu = false;
      imp = hd_apply;
    };
    {
      poly_name;
      name = "tree_hd";
      tps = [ T.IntTree; T.Int ];
      permu = false;
      imp = hd_apply;
    };
    {
      poly_name;
      name = "treei_hd";
      tps = [ T.IntTreeI; T.Int ];
      permu = false;
      imp = hd_apply;
    };
    {
      poly_name;
      name = "treeb_hd";
      tps = [ T.IntTreeB; T.Int ];
      permu = false;
      imp = hd_apply;
    };
  ]

let last_info =
  let poly_name = "last" in
  [
    {
      poly_name;
      name = "list_last";
      tps = [ T.IntList; T.Int ];
      permu = false;
      imp = last_apply;
    };
    {
      poly_name;
      name = "tree_last";
      tps = [ T.IntTree; T.Int ];
      permu = false;
      imp = last_apply;
    };
    {
      poly_name;
      name = "treei_last";
      tps = [ T.IntTreeI; T.Int ];
      permu = false;
      imp = last_apply;
    };
    {
      poly_name;
      name = "treeb_last";
      tps = [ T.IntTreeB; T.Int ];
      permu = false;
      imp = last_apply;
    };
  ]

(* TODO: limit size only works for nat *)
let size_info =
  let poly_name = "size" in
  [
    {
      poly_name;
      name = "list_size";
      tps = [ T.IntList; T.Int ];
      permu = false;
      imp = size_apply;
    };
    {
      poly_name;
      name = "list_size";
      tps = [ T.IntList; T.Nat ];
      permu = false;
      imp = size_apply;
    };
    {
      poly_name;
      name = "tree_size";
      tps = [ T.IntTree; T.Int ];
      permu = false;
      imp = size_apply;
    };
    {
      poly_name;
      name = "treei_size";
      tps = [ T.IntTreeI; T.Int ];
      permu = false;
      imp = size_apply;
    };
    {
      poly_name;
      name = "treeb_size";
      tps = [ T.IntTreeB; T.Int ];
      permu = false;
      imp = size_apply;
    };
  ]

let size_plus1_info =
  let poly_name = "size_plus1" in
  [
    {
      poly_name;
      name = "list_size_plus1";
      tps = [ T.IntList; T.IntList ];
      permu = false;
      imp = size_plus1_apply;
    };
  ]

let len_info =
  let poly_name = "len" in
  [
    {
      poly_name;
      name = "list_len";
      tps = [ T.IntList; T.Int ];
      permu = false;
      imp = len_apply;
    };
    {
      poly_name;
      name = "tree_len";
      tps = [ T.IntTree; T.Int ];
      permu = false;
      imp = len_apply;
    };
    {
      poly_name;
      name = "treei_len";
      tps = [ T.IntTreeI; T.Int ];
      permu = false;
      imp = len_apply;
    };
    {
      poly_name;
      name = "treeb_len";
      tps = [ T.IntTreeB; T.Int ];
      permu = false;
      imp = len_apply;
    };
  ]

let ord_info =
  let poly_name = "ord" in
  [
    {
      poly_name;
      name = "list_ord";
      tps = [ T.IntList; T.Int; T.Int ];
      permu = false;
      imp = ord_apply;
    };
  ]

let once_info =
  let poly_name = "once" in
  [
    {
      poly_name;
      name = "list_once";
      tps = [ T.IntList; T.Int ];
      permu = false;
      imp = once_apply;
    };
  ]

let left_info =
  let poly_name = "left" in
  [
    {
      poly_name;
      name = "tree_left";
      tps = [ T.IntTree; T.Int; T.Int ];
      permu = false;
      imp = left_apply;
    };
    {
      poly_name;
      name = "treei_left";
      tps = [ T.IntTreeI; T.Int; T.Int ];
      permu = false;
      imp = left_apply;
    };
    {
      poly_name;
      name = "treeb_left";
      tps = [ T.IntTreeB; T.Int; T.Int ];
      permu = false;
      imp = left_apply;
    };
  ]

let right_info =
  let poly_name = "right" in
  [
    {
      poly_name;
      name = "tree_right";
      tps = [ T.IntTree; T.Int; T.Int ];
      permu = false;
      imp = right_apply;
    };
    {
      poly_name;
      name = "treei_right";
      tps = [ T.IntTreeI; T.Int; T.Int ];
      permu = false;
      imp = right_apply;
    };
    {
      poly_name;
      name = "treeb_right";
      tps = [ T.IntTreeB; T.Int; T.Int ];
      permu = false;
      imp = right_apply;
    };
  ]

let para_info =
  let poly_name = "para" in
  [
    {
      poly_name;
      name = "tree_para";
      tps = [ T.IntTree; T.Int; T.Int ];
      permu = false;
      imp = parallel_apply;
    };
    {
      poly_name;
      name = "treei_para";
      tps = [ T.IntTreeI; T.Int; T.Int ];
      permu = false;
      imp = parallel_apply;
    };
    {
      poly_name;
      name = "treeb_para";
      tps = [ T.IntTreeB; T.Int; T.Int ];
      permu = false;
      imp = parallel_apply;
    };
  ]

let left_adj_info =
  let poly_name = "left_adj" in
  [
    {
      poly_name;
      name = "tree_left_adj";
      tps = [ T.IntTree; T.Int; T.Int ];
      permu = false;
      imp = left_adj_apply;
    };
    {
      poly_name;
      name = "treei_left_adj";
      tps = [ T.IntTreeI; T.Int; T.Int ];
      permu = false;
      imp = left_adj_apply;
    };
    {
      poly_name;
      name = "treeb_left_adj";
      tps = [ T.IntTreeB; T.Int; T.Int ];
      permu = false;
      imp = left_adj_apply;
    };
  ]

let right_adj_info =
  let poly_name = "right_adj" in
  [
    {
      poly_name;
      name = "tree_right_adj";
      tps = [ T.IntTree; T.Int; T.Int ];
      permu = false;
      imp = right_adj_apply;
    };
    {
      poly_name;
      name = "treei_right_adj";
      tps = [ T.IntTreeI; T.Int; T.Int ];
      permu = false;
      imp = right_adj_apply;
    };
    {
      poly_name;
      name = "treeb_right_adj";
      tps = [ T.IntTreeB; T.Int; T.Int ];
      permu = false;
      imp = right_adj_apply;
    };
  ]

let para_adj_info =
  let poly_name = "para_adj" in
  [
    {
      poly_name;
      name = "tree_para_adj";
      tps = [ T.IntTree; T.Int; T.Int ];
      permu = false;
      imp = parallel_adj_apply;
    };
    {
      poly_name;
      name = "treei_para_adj";
      tps = [ T.IntTreeI; T.Int; T.Int ];
      permu = false;
      imp = parallel_adj_apply;
    };
    {
      poly_name;
      name = "treeb_para_adj";
      tps = [ T.IntTreeB; T.Int; T.Int ];
      permu = false;
      imp = parallel_adj_apply;
    };
  ]

let complete_info =
  let poly_name = "complete" in
  [
    {
      poly_name;
      name = "tree_complete";
      tps = [ T.IntTree; T.Int ];
      permu = false;
      imp = complete_apply;
    };
  ]

let depth_eq_info =
  let poly_name = "depth_eq" in
  [
    {
      poly_name;
      name = "tree_depth_eq";
      tps = [ T.IntTree; T.IntTree ];
      permu = false;
      imp = depth_eq_apply;
    };
  ]

let depth_plus1_info =
  let poly_name = "depth_plus1" in
  [
    {
      poly_name;
      name = "tree_depth_plus1";
      tps = [ T.IntTree; T.IntTree ];
      permu = false;
      imp = depth_plus1_apply;
    };
  ]

let lt_info =
  let poly_name = "<" in
  [
    {
      poly_name;
      name = "<";
      tps = [ T.Int; T.Int ];
      permu = false;
      imp = lt_apply;
    };
    {
      poly_name;
      name = "<";
      tps = [ T.Nat; T.Nat ];
      permu = false;
      imp = lt_apply;
    };
  ]

let eq_info =
  let poly_name = "==" in
  [
    {
      poly_name;
      name = "==";
      tps = [ T.Int; T.Int ];
      permu = false;
      imp = eq_apply;
    };
    {
      poly_name;
      name = "==";
      tps = [ T.Nat; T.Nat ];
      permu = false;
      imp = eq_apply;
    };
  ]

let rb_info =
  [
    {
      poly_name = "rb_balance";
      name = "rb_balance";
      tps = [ T.IntTreeB ];
      permu = false;
      imp = rb_balance_apply;
    };
    {
      poly_name = "rb_balance2";
      name = "rb_balance2";
      tps = [ T.IntTreeB; T.IntTreeB ];
      permu = false;
      imp = rb_balance2_apply;
    };
    {
      poly_name = "label_is_true";
      name = "label_is_true";
      tps = [ T.IntTreeB; T.Int ];
      permu = false;
      imp = label_is_apply true;
    };
    {
      poly_name = "label_is_false";
      name = "label_is_false";
      tps = [ T.IntTreeB; T.Int ];
      permu = false;
      imp = label_is_apply false;
    };
  ]

let leftist_info =
  [
    {
      poly_name = "leftist";
      name = "leftist";
      tps = [ T.IntTreeI ];
      permu = false;
      imp = leftist_apply;
    };
  ]

let binomialhp_info =
  [
    {
      poly_name = "binomialhp";
      name = "binomialhp";
      tps = [ T.Uninterp "binomialhp" ];
      permu = false;
      imp = binomialhp_apply;
    };
  ]

let skewhp_info =
  [
    {
      poly_name = "skewhp";
      name = "skewhp";
      tps = [ T.Uninterp "skewhp" ];
      permu = false;
      imp = skewhp_apply;
    };
  ]

let pairinghp_info =
  [
    {
      poly_name = "pairinghp";
      name = "pairinghp";
      tps = [ T.Uninterp "pairinghp" ];
      permu = false;
      imp = pairinghp_apply;
    };
    {
      poly_name = "pairinghp_sort";
      name = "pairinghp_sort";
      tps = [ T.Uninterp "pairinghp" ];
      permu = false;
      imp = pairinghp_sort_apply;
    };
  ]

let mp_table =
  empty_info @ mem_info @ hd_info @ lt_info @ eq_info @ ord_info
  @ (left_info @ right_info @ para_info)
  @ (left_adj_info @ right_adj_info @ para_adj_info)
  @ size_info @ size_plus1_info @ len_info @ last_info @ once_info @ rb_info
  @ leftist_info @ binomialhp_info @ skewhp_info @ pairinghp_info

let imp_map =
  List.fold_left (fun m r -> StrMap.add r.name r.imp m) StrMap.empty mp_table

let find_info_by_polyname_tps poly_name tps =
  (* let () = Printf.printf "poly_name: %s; tps: %s\n" poly_name @@ List.split_by_comma Tp.layout tps in *)
  match
    List.find_opt
      (fun r -> String.equal r.poly_name poly_name && Tp.tps_eq r.tps tps)
      mp_table
  with
  | Some r -> r
  | None ->
      raise
      @@ failwith
           (Printf.sprintf "cannot find method predicate(%s) with tps(%s)"
              poly_name
              (List.split_by_comma Tp.layout tps))

let find_info_by_name name =
  match List.find_opt (fun r -> String.equal name r.name) mp_table with
  | Some r -> r
  | None ->
      raise @@ failwith (Printf.sprintf "cannot find method predicate(%s)" name)

let instantization poly_name tps =
  let r = find_info_by_polyname_tps poly_name tps in
  r.name

let poly_name name =
  let r = find_info_by_name name in
  r.poly_name

let apply mp args =
  (StrMap.find
     (Printf.sprintf "cannot find method predicate(%s)when applying" mp)
     imp_map mp)
    args
