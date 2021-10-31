open Primitive
open Basic_dt

type tvar = Tp.t * int

(* type tvaropt = Tp.t * (int option) *)
type instr = { op : string; args : tvar list; res : tvar list }

type t = { fin : tvar list; body : instr list; fout : tvar list }

let extract_tps { fin; _ } = List.map fst fin

let extract_ops { body; _ } = List.map (fun instr -> instr.op) body

let compare_tvar (t1, name1) (t2, name2) =
  let c1 = Tp.compare t1 t2 in
  if c1 == 0 then compare name1 name2 else c1

let compare_tvarl l1 l2 = List.compare compare_tvar l1 l2

let compare_prog t1 t2 =
  let c1 = compare_tvarl t1.fin t2.fin in
  if c1 != 0 then c1
  else
    let c2 = compare_tvarl t1.fout t2.fout in
    if c2 != 0 then c2
    else
      List.compare
        (fun instr1 instr2 ->
          let c1 = String.compare instr1.op instr2.op in
          if c1 != 0 then c1
          else
            let c2 = compare_tvarl instr1.args instr2.args in
            if c2 != 0 then c2 else compare_tvarl instr1.res instr2.res)
        t1.body t2.body

let subst_args m args =
  List.map
    (fun (tp, place) ->
      match IntMap.find_opt m place with
      | Some source -> (tp, source)
      | None -> raise @@ failwith "wrong argument assignment")
    args

let subst_instr m { op; args; res } = { op; args = subst_args m args; res }

let subst m { fin; body; fout } =
  { fin; body = List.map (subst_instr m) body; fout = subst_args m fout }

let initial_naming (input_tp : Tp.t list) (ops : string list) =
  let idx_counter1 = ref 0 in
  let newname1 () =
    let x = !idx_counter1 in
    idx_counter1 := x + 1;
    x
  in
  let idx_counter2 = ref 0 in
  let newname2 () =
    let x = !idx_counter2 in
    idx_counter2 := x + 1;
    x
  in
  let f r op =
    let inptps, outtps = Operator.get_tp_one op in
    let res = List.map (fun tp -> (tp, newname1 ())) outtps in
    let args = List.map (fun tp -> (tp, newname2 ())) inptps in
    { op; args; res } :: r
  in
  let fin = List.map (fun tp -> (tp, newname1 ())) input_tp in
  let body = List.rev @@ List.fold_left f [] ops in
  { fin; body; fout = List.map (fun tp -> (tp, newname2 ())) input_tp }

(* For Parsing *)
let layout_var (_, idx) =
  (* let name tp = *)
  (*   match tp with *)
  (*   | Tp.Bool -> "b" *)
  (*   | Tp.IntList -> "l" *)
  (*   | Tp.IntTree | Tp.IntTreeB | Tp.IntTreeI -> "t" *)
  (*   | _ -> "i" *)
  (* in *)
  spf "%s%i" "x" idx

let layout_tvar (tp, idx) =
  Printf.sprintf "%s: %s" (layout_var (tp, idx)) (Tp.layout tp)

let layout_instr { op; args; res } =
  Printf.sprintf "(%s) = %s(%s)"
    (List.split_by_comma layout_tvar res)
    op
    (List.split_by_comma (fun x -> Printf.sprintf "%s" (layout_tvar x)) args)

let layout { fin; body; fout } =
  let open Printf in
  let fin_str = sprintf "IN: (%s)" (List.split_by_comma layout_tvar fin) in
  let body_str = List.split_by ";\n" layout_instr body in
  let fout_str =
    sprintf "OUT: (%s)"
      (List.split_by_comma (fun x -> Printf.sprintf "%s" (layout_tvar x)) fout)
  in
  sprintf "%s {\n%s}\n%s\n" fin_str body_str fout_str

let check_non_det { body; fout; _ } =
  let exists_non_det args m =
    List.exists (function None -> false | _ -> true)
    @@ List.map (fun (_, idx) -> IntMap.find_opt m idx) args
  in
  let rec symbolic_exec body non_det_m =
    match body with
    | [] -> exists_non_det fout non_det_m
    | { op; args; res } :: body ->
        let non_det_m =
          if exists_non_det args non_det_m || Operator.check_non_det op then
            List.fold_left (fun m (_, idx) -> IntMap.add idx () m) non_det_m res
          else non_det_m
        in
        symbolic_exec body non_det_m
  in
  symbolic_exec body IntMap.empty

(*TEST*)

let test_tps_ops =
  ([ Tp.IntList ], [ "random_int"; "random_int"; "random_int"; "replace" ])

let test_prog =
  let tps, ops = test_tps_ops in
  initial_naming tps ops

let test () =
  let _ = Printf.printf "%s\n" (layout test_prog) in
  ()
