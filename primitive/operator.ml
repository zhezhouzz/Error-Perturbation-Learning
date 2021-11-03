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

let info_table =
  let known_operators =
    [
      (* bool *)
      "random_int";
      "neg";
      "consttrue";
      "constfalse";
      (* int *)
      "plus1";
      "minus1";
      "random_int";
      "const0";
      "const1";
      "const2";
      (* list *)
      "insert";
      "replace";
      "cons";
      "append";
      "top";
      "bottom";
      "max";
      "min";
      (* tree *)
      "tree_node";
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
      (* treei *)
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
      (* treeb *)
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
    ]
  in
  let make_info name =
    let imps =
      List.filter (fun imp -> String.equal name imp.Imp.imp_name)
      @@ Imp_const.table @ Imp_list.table @ Imp_tree.table @ Imp_treei.table
      @ Imp_treeb.table
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
  let info = StrMap.find "operator::get_tp_one" info_table op in
  match info.poly_tp with
  | [] -> raise @@ failwith "operator::get_tp_one"
  | h :: _ -> h

let get_imp (op : string) =
  let info = StrMap.find "operator::get_imp" info_table op in
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
