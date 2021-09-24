type t = string
type info = {name:string;
             poly_tp: ((Tp.t list) * (Tp.t list)) list;
             higher_order: bool;
             imp: Value.t list -> (Value.t list) option
            }
;;
open Basic_dt;;

let info_table = (
  let open Tp in
  let l =
  [
    { name = "insert";
      poly_tp = [([IntList; Int; Int], [IntList])];
      higher_order = false;
      imp = Imp.insert};
    { name = "replace";
      poly_tp = [([IntList; Int; Int], [IntList])];
      higher_order = false;
      imp = Imp.replace};
    (* { name = "swap";
     *   poly_tp = [([IntList; Int; Int], [IntList])];
     *   higher_order = false;
     *   imp = Imp.insert};
     * { name = "cons";
     *   poly_tp = [([IntList; Int;], [IntList])];
     *   higher_order = false;
     *   imp = Imp.insert};
     * { name = "append";
     *   poly_tp = [([IntList; Int;], [IntList])];
     *   higher_order = false;
     *   imp = Imp.insert};
     * { name = "zip";
     *   poly_tp = [([IntList; IntList;], [IntList])];
     *   higher_order = false;
     *   imp = Imp.insert}; *)
    { name = "top";
      poly_tp = [([IntList;], [Int])];
      higher_order = false;
      imp = Imp.top};
    (* { name = "bottom";
     *   poly_tp = [([IntList;], [Int])];
     *   higher_order = false;
     *   imp = Imp.insert};
     * { name = "max";
     *   poly_tp = [([IntList;], [Int])];
     *   higher_order = false;
     *   imp = Imp.insert};
     * { name = "min";
     *   poly_tp = [([IntList;], [Int])];
     *   higher_order = false;
     *   imp = Imp.insert};
     * { name = "n";
     *   poly_tp = [([], [Int])];
     *   higher_order = false;
     *   imp = Imp.insert};
     * { name = "const0";
     *   poly_tp = [([], [Int])];
     *   higher_order = false;
     *   imp = Imp.insert};
     * { name = "const1";
     *   poly_tp = [([], [Int])];
     *   higher_order = false;
     *   imp = Imp.insert};
     * { name = "const2";
     *   poly_tp = [([], [Int])];
     *   higher_order = false;
     *   imp = Imp.insert}; *)
  ]
  in
  List.fold_left (fun m info -> StrMap.add info.name info m) StrMap.empty l
)

let get_tp_one (op: string) =
  let info = StrMap.find "operator::get_tp_one" info_table op in
  match info.poly_tp with
  | [] -> raise @@ failwith "operator::get_tp_one"
  | h :: _ -> h

let get_imp (op: string) =
  let info = StrMap.find "operator::get_imp" info_table op in
  info.imp

let layout op = op

let apply_type_check op tps =
  let info = StrMap.find "operator::apply_type_check" info_table op in
  match List.find_opt (fun (tps', _) -> Tp.tps_eq tps tps') info.poly_tp with
  | Some _ -> true
  | None -> false
