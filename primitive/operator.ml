type t = string
type info = {name:string;
             poly_tp: ((Tp.t list) * (Tp.t list)) list;
             higher_order: bool;
             imp: Value.t list -> (Value.t list) option
            }
;;
open Basic_dt;;

let unused = "unused"
let is_unused op = String.equal unused op

let info_table = (
  let open Tp in
  let l =
    [
      { name = unused;
        poly_tp = [([], [])];
        higher_order = false;
        imp = Imp.unused};
      { name = "insert";
        poly_tp = [([IntList; Int; Int], [IntList])];
        higher_order = false;
        imp = Imp.insert};
      { name = "replace";
        poly_tp = [([IntList; Int; Int], [IntList])];
        higher_order = false;
        imp = Imp.replace};
      (* { name = "swap"; *)
      (*   poly_tp = [([IntList; Int; Int], [IntList])]; *)
      (*   higher_order = false; *)
      (*   imp = Imp.swap}; *)
      { name = "cons";
        poly_tp = [([IntList; Int;], [IntList])];
        higher_order = false;
        imp = Imp.cons};
      { name = "append";
        poly_tp = [([IntList; Int;], [IntList])];
        higher_order = false;
        imp = Imp.append};
      { name = "plus1";
        poly_tp = [([Int;], [Int])];
        higher_order = false;
        imp = Imp.plus1};
      { name = "minus1";
        poly_tp = [([Int;], [Int])];
        higher_order = false;
        imp = Imp.minus1};
      (* { name = "zip";
       *   poly_tp = [([IntList; IntList;], [IntList])];
       *   higher_order = false;
       *   imp = Imp.insert}; *)
      { name = "top";
        poly_tp = [([IntList;], [Int])];
        higher_order = false;
        imp = Imp.top};
      { name = "bottom";
        poly_tp = [([IntList;], [Int])];
        higher_order = false;
        imp = Imp.bottom};
      (* { name = "max"; *)
      (*   poly_tp = [([IntList;], [Int])]; *)
      (*   higher_order = false; *)
      (*   imp = Imp.max}; *)
      (* { name = "min"; *)
      (*   poly_tp = [([IntList;], [Int])]; *)
      (*   higher_order = false; *)
      (*   imp = Imp.min}; *)
      { name = "random_int";
        poly_tp = [([], [Int])];
        higher_order = false;
        imp = Imp.random_int};
      (* { name = "const0"; *)
      (*   poly_tp = [([], [Int])]; *)
      (*   higher_order = false; *)
      (*   imp = Imp.const_value 0}; *)
      (* { name = "const1"; *)
      (*   poly_tp = [([], [Int])]; *)
      (*   higher_order = false; *)
      (*   imp = Imp.const_value 1}; *)
      (* { name = "const2";
       *   poly_tp = [([], [Int])];
       *   higher_order = false;
       *   imp = Imp.const_value 2}; *)
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

let op_pool = StrMap.to_key_list info_table
