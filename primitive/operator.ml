type t = string
type info = {name:string;
             poly_tp: ((Tp.t list) * (Tp.t list)) list;
             higher_order: bool;
             imp: Value.t list -> (Value.t list) option;
             nondet: bool;
            }
;;
open Basic_dt;;

let unused = "unused"
let is_unused op = String.equal unused op

(* let find_imp name itps otps = *)
(*   let open Imp in *)
(*   let impinfo = *)
(*   List.find (spf "operator(%s) cannot find imp..." name) *)
(*     (fun (imp : Imp.imp) -> String.equal imp.imp_name name && Tp.tps_eq itps imp.imp_itps && Tp.tps_eq otps imp.imp_otps) *)
(*     Imp.table in *)
(*   impinfo.imp_exec *)
let info_table = (
  let known_operators = [
    (* int *)
    "plus1"; "minus1"; "random_int"; "const0"; "const1"; "const2";
    (* list *)
    "insert"; "replace"; "cons"; "append";
    "top"; "bottom";
    "max"; "min";
    (* tree *)
    "tree_node"; "tree_left_right_subtree"; "tree_root";
    "tree_flip"; "tree_rec_flip";
    "tree_rotation_left"; "tree_rotation_right";
    "tree_append_to_left_most"; "tree_append_to_right_most";
    "tree_max"; "tree_min";
  ] in
  let make_info name =
    let imps = List.filter (fun imp -> String.equal name imp.Imp.imp_name) Imp.table in
    match imps with
    | [] -> raise @@ failwith (spf "operator(%s) cannot find imp..." name)
    | [imp] -> {name = name; poly_tp = [imp.Imp.imp_itps, imp.Imp.imp_otps];
                higher_order = false;
                nondet = imp.Imp.nondet;
                imp = imp.Imp.imp_exec}
    | _ -> raise @@ failwith (spf "poly operator(%s) do not impelemented" name)
  in
  List.fold_left (fun m info -> StrMap.add info.name info m) StrMap.empty @@ List.map make_info known_operators
)
(* let l = *)
(*   [ { name = "insert"; *)
(*       poly_tp = [([IntList; Int; Int], [IntList])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = find_imp name poly_tp}; *)
(*     { name = "replace"; *)
(*       poly_tp = [([IntList; Int; Int], [IntList])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.replace}; *)
(*     (\* { name = "swap"; *\) *)
(*     (\*   poly_tp = [([IntList; Int; Int], [IntList])]; *\) *)
(*     (\*   higher_order = false; *\) *)
(*     (\*   imp = Imp.swap}; *\) *)
(*     { name = "cons"; *)
(*       poly_tp = [([IntList; Int;], [IntList])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.cons}; *)
(*     { name = "append"; *)
(*       poly_tp = [([IntList; Int;], [IntList])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.append}; *)
(*     { name = "plus1"; *)
(*       poly_tp = [([Int;], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.plus1}; *)
(*     { name = "minus1"; *)
(*       poly_tp = [([Int;], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.minus1}; *)
(*     (\* { name = "zip"; *)
(*      *   poly_tp = [([IntList; IntList;], [IntList])]; *)
(*      *   higher_order = false; *)
(*      *   imp = Imp.insert}; *\) *)
(*     { name = "top"; *)
(*       poly_tp = [([IntList;], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.top}; *)
(*     { name = "bottom"; *)
(*       poly_tp = [([IntList;], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.bottom}; *)
(*     { name = "max"; *)
(*       poly_tp = [([IntList;], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.max}; *)
(*     { name = "min"; *)
(*       poly_tp = [([IntList;], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.min}; *)
(*     { name = "random_int"; *)
(*       poly_tp = [([], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = true; *)
(*       imp = Imp.random_int}; *)
(*     { name = "const0"; *)
(*       poly_tp = [([], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.const_value 0}; *)
(*     { name = "const1"; *)
(*       poly_tp = [([], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.const_value 1}; *)
(*     { name = "const2"; *)
(*       poly_tp = [([], [Int])]; *)
(*       higher_order = false; *)
(*       nondet = false; *)
(*       imp = Imp.const_value 2}; *)
(*   ] *)


let get_tp_one (op: string) =
  let info = StrMap.find "operator::get_tp_one" info_table op in
  match info.poly_tp with
  | [] -> raise @@ failwith "operator::get_tp_one"
  | h :: _ -> h

let get_imp (op: string) =
  let info = StrMap.find "operator::get_imp" info_table op in
  info.imp

let check_non_det (op: string) =
  let info = StrMap.find "operator::check_non_det" info_table op in
  info.nondet

let layout op = op

let apply_type_check op tps =
  let info = StrMap.find "operator::apply_type_check" info_table op in
  match List.find_opt (fun (tps', _) -> Tp.tps_eq tps tps') info.poly_tp with
  | Some _ -> true
  | None -> false

let op_pool = StrMap.to_key_list info_table
