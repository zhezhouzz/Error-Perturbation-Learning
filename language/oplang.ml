open Primitive;;
open Basic_dt;;
type tvar = Tp.t * int
(* type tvaropt = Tp.t * (int option) *)
type instr = {op: string; args: tvar list; res: tvar list}
type t = {fin: tvar list; body: instr list; fout: tvar list}

let subst_args m args =
  List.map (fun (tp, place) ->
      match IntMap.find_opt m place with
      | Some source -> (tp, source)
      | None -> raise @@ failwith "wrong argument assignment"
    ) args

let subst_instr m {op; args; res} =
  {op = op; args = subst_args m args; res = res}

let subst m {fin; body; fout} =
  {fin = fin; body = List.map (subst_instr m) body; fout = subst_args m fout}

let initial_naming (input_tp: Tp.t list) (ops: string list) =
  let idx_counter1 = ref 0 in
  let newname1 () = let x = !idx_counter1 in idx_counter1 := x + 1; x in
  let idx_counter2 = ref 0 in
  let newname2 () = let x = !idx_counter2 in idx_counter2 := x + 1; x in
  let f r op =
    let inptps, outtps = Operator.get_tp_one op in
    let res = List.map (fun tp -> (tp, newname1 ())) outtps in
    let args = List.map (fun tp -> (tp, newname2 ())) inptps in
    {op = op; args = args; res = res} :: r
  in
  let fin = List.map (fun tp -> (tp, newname1 ())) input_tp in
  let body = List.rev @@ List.fold_left f [] ops in
  {fin = fin; body = body; fout = List.map (fun tp -> (tp, newname2 ())) input_tp}

let layout_tvar (tp, idx) = Printf.sprintf "%s_%i" (Tp.layout tp) idx

let layout_instr {op; args; res} =
  Printf.sprintf "(%s) = %s(%s)" (List.split_by_comma layout_tvar res) op
    (List.split_by_comma (fun x -> Printf.sprintf "[%s]" (layout_tvar x)) args)

let layout {fin; body; fout} =
  let open Printf in
  let fin_str = sprintf "IN: %s\n" (List.split_by_comma layout_tvar fin) in
  let body_str = List.fold_left (fun r instr -> sprintf "%s%s\n" r (layout_instr instr)) "" body in
  let fout_str = sprintf "OUT: %s\n" (List.split_by_comma (fun x -> Printf.sprintf "[%s]" (layout_tvar x)) fout) in
  sprintf "%s%s%s" fin_str body_str fout_str

(*TEST*)

let test_tps_ops = [Tp.IntList], ["random_int";"random_int" ; "unused"; "replace"]

let test_prog =
  let tps, ops = test_tps_ops in
  initial_naming tps ops

let test () =
  let _ = Printf.printf "%s\n" (layout test_prog) in
  ()
