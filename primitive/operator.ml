type t = string
type info = {name:string;
             poly_tp: ((Tp.t list) * (Tp.t list)) list;
             higher_order: bool
            }
;;
open Basic_dt;;
let info_table =
  let open Tp in
  let l =
  [
    { name = "insert";
      poly_tp = [([IntList; Int; Int], [IntList])];
      higher_order = false};
    { name = "replace";
      poly_tp = [([IntList; Int; Int], [IntList])];
      higher_order = false};
    { name = "swap";
      poly_tp = [([IntList; Int; Int], [IntList])];
      higher_order = false};
    { name = "cons";
      poly_tp = [([IntList; Int;], [IntList])];
      higher_order = false};
    { name = "append";
      poly_tp = [([IntList; Int;], [IntList])];
      higher_order = false};
    { name = "zip";
      poly_tp = [([IntList; IntList;], [IntList])];
      higher_order = false};
    { name = "first";
      poly_tp = [([IntList;], [Int])];
      higher_order = false};
    { name = "last";
      poly_tp = [([IntList;], [Int])];
      higher_order = false};
    { name = "max";
      poly_tp = [([IntList;], [Int])];
      higher_order = false};
    { name = "min";
      poly_tp = [([IntList;], [Int])];
      higher_order = false};
    { name = "n";
      poly_tp = [([], [Int])];
      higher_order = false};
    { name = "const0";
      poly_tp = [([], [Int])];
      higher_order = false};
    { name = "const1";
      poly_tp = [([], [Int])];
      higher_order = false};
    { name = "const2";
      poly_tp = [([], [Int])];
      higher_order = false};
  ]
  in
  List.fold_left (fun m info -> StrMap.add info.name info m) StrMap.empty l

let layout op = op

let apply_type_check op tps =
  let info = StrMap.find "operator::apply_type_check" info_table op in
  match List.find_opt (fun (tps', _) -> Tp.tps_eq tps tps') info.poly_tp with
  | Some _ -> true
  | None -> false
