open Primitive
open Basic_dt
open Oplang
module V = Value

(* let w_diff = 10.0 *)

let w_other = 2.0

let list_sum = List.fold_left (fun sum x -> sum +. x) 0.0

let distance num_args a trace =
  let d_2 = 1.0 /. float_of_int num_args in
  let m = IntMap.empty in
  let m =
    List.fold_left
      (fun m name ->
        match IntMap.find_opt m name with
        | None -> IntMap.add name 1 m
        | Some n -> IntMap.add name (n + 1) m)
      m trace
  in
  let d_a =
    min 1.0
      (match IntMap.find_opt m a with
      | None -> 0.6
      | Some n -> 0.5 *. d_2 *. float_of_int (n - 1))
  in
  let d =
    if num_args == 1 then d_a
    else
      let m =
        IntMap.filter_map (fun name v -> if name == a then None else Some v) m
      in
      let d_others =
        list_sum
        @@ List.map (fun (_, n) -> float_of_int n)
        @@ IntMap.to_kv_list m
      in
      let d_others = min 1.0 (d_others /. float_of_int num_args *. d_2) in
      ((w_other *. d_others) +. d_a) /. (w_other +. 1.0)
  in
  (* let _ = Zlog.log_write @@ spf "x_%i: %f" a d in *)
  d

let distance_n fout m =
  let len = List.length fout in
  let aux (_, name) =
    let trace = IntMap.find "distance_n" m name in
    distance len name trace
  in
  (list_sum @@ List.map aux fout) /. float_of_int len

let insteresting { fin; body; fout } =
  let m = IntMap.empty in
  let m =
    List.fold_left (fun m (_, name) -> IntMap.add name [ name ] m) m fin
  in
  let aux m { args; res; _ } =
    let news =
      List.flatten
      @@ List.map (fun (_, name) -> IntMap.find "trace" m name) args
    in
    let m = List.fold_left (fun m (_, name) -> IntMap.add name news m) m res in
    m
  in
  let m = List.fold_left aux m body in
  distance_n fout m
