let theta_int = [ "const0"; "plus1"; "minus1"; "const1"; "max"; "min" ]

let theta_list =
  [
    "cons";
    "append";
    "list_destruct";
    "list_last_destruct";
    "list_mid_partition";
    "list_alter_partition";
    "list_head";
    "list_last";
    "list_upper_bound";
    "list_lower_bound";
  ]

let theta_stream =
  [
    "cons";
    "append";
    "list_destruct";
    "list_last_destruct";
    "list_mid_partition";
    "list_alter_partition";
    "list_head";
    "list_last";
    "list_single";
    "list_upper_bound";
    "list_lower_bound";
  ]

let theta_tree =
  [
    "tree_node";
    "tree_node_single";
    "tree_destruct";
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
    "tree_drop_bottom";
  ]

let theta_treeb =
  [
    "treeb_node";
    "treeb_destruct";
    "treeb_rotation_left";
    "treeb_rotation_right";
    "treeb_append_to_left_most";
    "treeb_append_to_right_most";
    "treeb_upper_bound";
    "treeb_lower_bound";
    "treeb_drop_bottom";
    "treeb_single";
  ]

let theta_treei =
  [
    "treei_node";
    "treei_destruct";
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
    "treei_drop_bottom";
  ]

let theta_binomialhp =
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

let theta_pairinghp =
  [
    "pairinghp_node";
    "pairinghp_destruct";
    "pairinghp_append_to_left_most";
    "pairinghp_append_to_right_most";
    "pairinghp_max";
    "pairinghp_min";
    "pairinghp_upper_bound";
    "pairinghp_lower_bound";
    "pairinghp_drop_bottom";
    "pairinghp_single";
  ]

let theta_skewhp =
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
    "skewt_head_update";
  ]

let theta_table =
  [
    ("theta_int", theta_int);
    ("theta_list", theta_list);
    ("theta_stream", theta_stream);
    ("theta_tree", theta_tree);
    ("theta_treeb", theta_treeb);
    ("theta_treei", theta_treei);
    ("theta_binomialhp", theta_binomialhp);
    ("theta_pairinghp", theta_pairinghp);
    ("theta_skewhp", theta_skewhp);
  ]

let find_theta l =
  List.flatten
  @@ List.map
       (fun name ->
         match
           List.find_opt (fun x -> String.equal name (fst x)) theta_table
         with
         | None -> raise @@ failwith (Printf.sprintf "unknown theta: %s" name)
         | Some (_, ops) -> ops)
       l
