open Primitive
open Basic_dt
open Oplang
module V = Value

(* let w_diff = 10.0 *)

let w_other = 2.0

let list_sum = List.fold_left (fun sum x -> sum +. x) 0.0

let type_weight tp = if Tp.is_dt tp then 3.0 else 1.0

let op_weight name =
  let a, b = Operator.get_tp_one name in
  let c x = list_sum @@ List.map type_weight x in
  min 1.2 (max 0.5 (c b /. c a))

let distance num_args a trace =
  let d_2 = 1.0 /. float_of_int num_args in
  let d_a =
    min 1.0 (if trace.(a) < 0.01 then 0.6 else (0.5 *. d_2 *. trace.(a)) -. 1.0)
  in
  let d =
    if num_args == 1 then d_a
    else
      let d_others =
        list_sum @@ Array.to_list
        @@ Array.mapi (fun i x -> if i == a then 0.0 else x) trace
      in
      let d_others = min 1.0 (d_others /. float_of_int num_args *. d_2) in
      ((w_other *. d_others) +. d_a) /. (w_other +. 1.0)
  in
  (* let _ = Zlog.log_write @@ spf "x_%i: %f" a d in *)
  d

let distance_n fin fout m =
  let len = List.length fout in
  let aux ((_, iname), (_, oname)) =
    let trace = Hashtbl.find m oname in
    distance len iname trace
  in
  (list_sum @@ List.map aux (List.combine fin fout)) /. float_of_int len

let insteresting { fin; body; fout } =
  let len = List.length fin in
  let m = Hashtbl.create 10 in
  let () =
    List.iter
      (fun (_, name) ->
        let arr =
          Array.init (len + 2) (fun i -> if i == name then 1.0 else 0.0)
        in
        Hashtbl.add m name arr)
      fin
  in
  let arr_add_to w a b =
    Array.iteri (fun i v -> b.(i) <- b.(i) +. (w *. v)) a
  in
  let add_to w a b =
    let a = Hashtbl.find m a in
    let b = Hashtbl.find m b in
    arr_add_to w a b
  in
  let aux { op; args; res } =
    List.iter
      (fun (_, r) ->
        let () = Hashtbl.add m r (Array.init (len + 2) (fun _ -> 0.0)) in
        List.iter (fun (_, arg) -> add_to (op_weight op) arg r) args)
      res
  in
  let () = List.iter aux body in
  min 1.0 @@ max 0.0 @@ distance_n fin fout m
