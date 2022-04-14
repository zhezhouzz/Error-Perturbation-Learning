type t = string

type info = {
  name : string;
  poly_tp : (Tp.t list * Tp.t list) list;
  higher_order : bool;
  imp : Value.t list -> Value.t list option;
  nondet : bool;
}

open Basic_dt

let unused = "unused"

let is_unused op = String.equal unused op

(* TODO: unify the name of operators *)

let bool_ops = [ "random_bool"; "neg"; "consttrue"; "constfalse" ]

let nat_ops = []

let int_ops =
  [
    "plus";
    "minus";
    "plus1";
    "minus1";
    "random_int";
    "const0";
    "const1";
    "const2";
  ]

let list_ops =
  [
    "replace";
    "insert";
    "cons";
    "append";
    "top";
    "bottom";
    "tail";
    "list_len";
    "list_head";
    "list_tail";
    "list_last";
    "list_destruct";
    "list_last_destruct";
    "list_mid_partition";
    "list_alter_partition";
    "max";
    "min";
    "list_upper_bound";
    "list_lower_bound";
    "list_single";
    "nil";
    "list_id";
  ]

let iblist_ops = [ "iblist_destruct"; "iblist_cons" ]

let biblist_ops = [ "biblist_destruct"; "biblist_cons" ]

let tree_ops =
  [
    "tree_node";
    "tree_node_single";
    "tree_left_right_subtree";
    "tree_root";
    "tree_flip";
    "tree_rec_flip";
    "tree_rotation_left";
    "tree_rotation_right";
    "tree_append_to_left_most";
    "tree_append_to_right_most";
    "tree_max";
    "tree_min";
    "tree_upper_bound";
    "tree_lower_bound";
    "tree_add_to_bottom_left";
    "tree_add_to_bottom_right";
    "tree_drop_bottom";
    "tree_destruct";
  ]

let treei_ops =
  [
    "treei_node";
    "treei_left_right_subtree";
    "treei_root";
    "treei_flip";
    "treei_rec_flip";
    "treei_rotation_left";
    "treei_rotation_right";
    "treei_append_to_left_most";
    "treei_append_to_right_most";
    "treei_max";
    "treei_min";
    "treei_upper_bound";
    "treei_lower_bound";
    "treei_destruct";
    "treei_drop_bottom";
  ]

let treeb_ops =
  [
    "treeb_node";
    "treeb_left_right_subtree";
    "treeb_root";
    "treeb_flip";
    "treeb_rec_flip";
    "treeb_rotation_left";
    "treeb_rotation_right";
    "treeb_append_to_left_most";
    "treeb_append_to_right_most";
    "treeb_max";
    "treeb_min";
    "treeb_upper_bound";
    "treeb_lower_bound";
    "treeb_destruct";
    "treeb_drop_bottom";
    "treeb_single";
  ]

let binomialhp_ops =
  [
    "binomialhp_list_last_destruct";
    "binomialhp_list_destruct";
    "binomialhp_list_cons";
    "binomialhp_list_append";
    "binomialhp_single";
    "binomialhp_top";
    "binomialhp_lower_bound";
    "binomialhp_upper_bound";
    "binomialt_head";
    "binomialhp_head_update";
  ]

let skewhp_ops =
  [
    "skewhp_list_last_destruct";
    "skewhp_list_destruct";
    "skewhp_list_cons";
    "skewhp_list_append";
    "skewhp_single";
    "skewhp_top";
    "skewhp_lower_bound";
    "skewhp_upper_bound";
    "skewt_head";
    "skewt_head_l";
    "skewt_head_update";
    "skewt_head_l_update";
  ]

let pairinghp_ops =
  [
    "pairinghp_node";
    "pairinghp_cons";
    "pairinghp_append_to_left_most";
    "pairinghp_append_to_right_most";
    "pairinghp_max";
    "pairinghp_min";
    "pairinghp_upper_bound";
    "pairinghp_lower_bound";
    "pairinghp_destruct";
    "pairinghp_drop_bottom";
    "pairinghp_single";
  ]

let theta =
  [
    ("bool", bool_ops);
    ("nat", nat_ops);
    ("int", int_ops);
    ("list", list_ops);
    ("iblist", iblist_ops);
    ("biblist", biblist_ops);
    ("tree", tree_ops);
    ("treei", treei_ops);
    ("treeb", treeb_ops);
    ("binomialhp", binomialhp_ops);
    ("skewhp", skewhp_ops);
    ("pairinghp", pairinghp_ops);
  ]

let basic_op_pool = [ "const0"; "const1"; "plus1"; "minus1" ]

let ind_op_pool =
  [
    "replace";
    "insert";
    "cons";
    "append";
    "list_head";
    "list_tail";
    "list_last";
    "list_destruct";
    "list_last_destruct";
    "list_mid_partition";
    "list_alter_partition";
    "max";
    "min";
    "list_upper_bound";
    "list_lower_bound";
    "list_single";
  ]

let get_pool_by_name name =
  match List.find_opt (fun (x, _) -> String.equal x name) theta with
  | None ->
      raise @@ failwith @@ spf "cannot find operators for data type %s" name
  | Some (_, x) -> x

let info_table =
  let known_operators = List.concat @@ snd @@ List.split theta in
  let make_info name =
    let imps =
      List.filter (fun imp -> String.equal name imp.Imp.imp_name) Imps.imps
    in
    match imps with
    | [] -> raise @@ failwith (spf "operator(%s) cannot find imp..." name)
    | [ imp ] ->
        {
          name;
          poly_tp = [ (imp.Imp.imp_itps, imp.Imp.imp_otps) ];
          higher_order = false;
          nondet = imp.Imp.nondet;
          imp = imp.Imp.imp_exec;
        }
    | _ -> raise @@ failwith (spf "poly operator(%s) do not impelemented" name)
  in
  List.fold_left (fun m info -> StrMap.add info.name info m) StrMap.empty
  @@ List.map make_info known_operators

let get_tp_one (op : string) =
  let info = StrMap.find (spf "operator::get_tp_one (%s)" op) info_table op in
  match info.poly_tp with
  | [] -> raise @@ failwith "operator::get_tp_one"
  | h :: _ -> h

let get_imp (op : string) =
  let info = StrMap.find (spf "operator::get_imp: %s" op) info_table op in
  info.imp

let check_non_det (op : string) =
  let info = StrMap.find "operator::check_non_det" info_table op in
  info.nondet

let layout op = op

let apply_type_check op tps =
  let info = StrMap.find "operator::apply_type_check" info_table op in
  match List.find_opt (fun (tps', _) -> Tp.tps_eq tps tps') info.poly_tp with
  | Some _ -> true
  | None -> false

let op_pool = StrMap.to_key_list info_table
