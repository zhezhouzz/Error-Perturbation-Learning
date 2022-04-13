open Primitive
module V = Value

let cond_length cond l =
  let rec aux n = function
    | [] -> n
    | h :: t ->
        let n' = if cond h then n + 1 else n in
        aux n' t
  in
  aux l

type evaluation_stat = {
  sampling_num : int;
  total_num : int;
  succ_num : int;
  in_sigma_num : int;
  in_sigma_out_phi_num : int;
  in_sigma_out_phi_unique_num : int;
}

let stat_empty =
  {
    sampling_num = 0;
    total_num = 0;
    succ_num = 0;
    in_sigma_num = 0;
    in_sigma_out_phi_num = 0;
    in_sigma_out_phi_unique_num = 0;
  }

let stat_merge s1 s2 =
  {
    sampling_num = s1.sampling_num + s2.sampling_num;
    total_num = s1.total_num + s2.total_num;
    succ_num = s1.succ_num + s2.succ_num;
    in_sigma_num = s1.in_sigma_num + s2.in_sigma_num;
    in_sigma_out_phi_num = s1.in_sigma_out_phi_num + s2.in_sigma_out_phi_num;
    in_sigma_out_phi_unique_num =
      s1.in_sigma_out_phi_unique_num + s2.in_sigma_out_phi_unique_num;
  }

let layout
    {
      sampling_num;
      total_num;
      succ_num;
      in_sigma_num;
      in_sigma_out_phi_num;
      in_sigma_out_phi_unique_num;
    } =
  let aux x =
    Printf.sprintf "%i(%f%s)" x
      (100.0 *. float_of_int x /. float_of_int sampling_num)
      "%"
  in
  let aux2 x = 100.0 *. float_of_int x /. float_of_int sampling_num in
  Printf.sprintf
    "sampled: %i\n\
    \  total: %s\n\
    \  succ: %s\n\
    \  in sigma: %s\n\
    \  in sigma out phi: %s\n\
    \  in sigma out phi unique: %s\n\n\
    \    $%.3f$ & $%.3f$ & $%.3f$\n"
    sampling_num (aux total_num) (aux succ_num) (aux in_sigma_num)
    (aux in_sigma_out_phi_num)
    (aux in_sigma_out_phi_unique_num)
    (aux2 in_sigma_num)
    (aux2 in_sigma_out_phi_num)
    (aux2 in_sigma_out_phi_unique_num)

let layout_eval benchname stat cost_time =
  let avg_time =
    match stat.in_sigma_out_phi_unique_num with
    | 0 -> "inf"
    | n ->
        Printf.sprintf "%0.3f (Murphy: %0.3f)"
          (cost_time *. 1000.0 /. float_of_int n)
          (cost_time *. 5000.0 /. float_of_int n)
  in
  Printf.sprintf "%s:\ncost time:%f(s)\navg time:%s(ms/instance)\n%s\n"
    benchname cost_time avg_time
  @@ layout stat

open Basic_dt

let filter_out (data : V.t list list) (sigma : V.t list -> bool)
    (client : V.t list -> V.t list option) (phi : V.t list -> bool) =
  List.filter
    (fun input ->
      match client input with
      | None -> false
      | Some output -> sigma input && (not @@ phi (input @ output)))
    data

let evaluation (num_none : int) (data : V.t list list)
    (sigma : V.t list -> bool) (client : V.t list -> V.t list option)
    (phi : V.t list -> bool) =
  let sampling_num = num_none + List.length data in
  let () = Zlog.log_write @@ Printf.sprintf "sampling num: %i\n" sampling_num in
  let arr = Array.of_list data in
  let total = List.init (List.length data) (fun i -> i) in
  (* let c = ref 0 in *)
  let total_num = List.length total in
  (* let () = Printf.printf "total num: %i\n" total_num in *)
  (* let () = raise @@ failwith "end" in *)
  (* let _ = *)
  (*   List.iter *)
  (*     (fun idx -> *)
  (*       Zlog.log_write @@ spf "%s" *)
  (*       @@ List.split_by_comma (fun v -> string_of_int @@ V.len v) arr.(idx)) *)
  (*     total *)
  (* in *)
  let succ_data =
    List.filter_map
      (fun idx ->
        match client arr.(idx) with None -> None | Some _ -> Some idx)
      total
  in
  let succ_num = List.length succ_data in
  (* let () = Printf.printf "succ num: %i\n" succ_num in *)
  let in_sigma_data = List.filter (fun idx -> sigma arr.(idx)) succ_data in
  let in_sigma_num = List.length in_sigma_data in
  (* let () = Printf.printf "in_sigma_num: %i\n" in_sigma_num in *)
  let in_sigma_out_phi_data =
    List.filter
      (fun idx ->
        match client arr.(idx) with
        | None -> false
        | Some outp -> not @@ phi (arr.(idx) @ outp))
      in_sigma_data
  in
  let in_sigma_out_phi_num = List.length in_sigma_out_phi_data in
  (* let () = *)
  (*   Zlog.log_write *)
  (*   @@ Printf.sprintf "in_sigma_out_phi:\n%s\n" *)
  (*   @@ Basic_dt.List.split_by "\n" *)
  (*        (fun idx -> *)
  (*          Printf.sprintf "%s -> %s" *)
  (*            (V.layout_l arr.(idx)) *)
  (*            (match client arr.(idx) with *)
  (*            | None -> "none" *)
  (*            | Some out -> V.layout_l out)) *)
  (*        in_sigma_out_phi_data *)
  (* in *)
  let in_sigma_out_phi_unique_data =
    Value_aux.remove_duplicates_arr arr in_sigma_out_phi_data
  in
  let in_sigma_out_phi_unique_num = List.length in_sigma_out_phi_unique_data in
  {
    sampling_num;
    total_num;
    succ_num;
    in_sigma_num;
    in_sigma_out_phi_num;
    in_sigma_out_phi_unique_num;
  }

let evaluation_opt (data : V.t list option list) (sigma : V.t list -> bool)
    (client : V.t list -> V.t list option) (phi : V.t list -> bool) =
  let num_none, data =
    List.fold_left
      (fun (n, res) x ->
        match x with None -> (n + 1, res) | Some d -> (n, d :: res))
      (0, []) data
  in
  evaluation num_none data sigma client phi

module E = Sampling.Engine

let gen_num = function E.QCState _ -> 50000 | E.PerbState _ -> 5000

let measured_num = function E.QCState _ -> 100 | E.PerbState _ -> 4000

let timed_evaluation expected_time (engine : E.state)
    (measure : V.t list -> bool) (sigma : V.t list -> bool)
    (client : V.t list -> V.t list option) (phi : V.t list -> bool) =
  let rec loop (engine, stat, cost_time) =
    let rec aux (engine, n, res) =
      if List.length res > measured_num engine then (engine, n, res)
      else
        let engine, _, tmp = E.sampling (gen_num engine) engine in
        let tmp = List.filter measure tmp in
        let none_num = gen_num engine - List.length tmp in
        aux (engine, n + none_num, res @ tmp)
    in
    let (engine, none_num, data), d_time =
      Zlog.event_time_ "timed_evaluation" (fun () -> aux (engine, 0, []))
    in
    let stat_one = evaluation none_num data sigma client phi in
    let () = Zlog.log_write @@ layout_eval "" stat_one d_time in
    let () =
      Zlog.log_write
      @@ Printf.sprintf "expected_time: %f\ncost_time: %f" expected_time
           cost_time
    in
    let stat = stat_merge stat stat_one in
    let cost_time = cost_time +. d_time in
    if expected_time > cost_time then loop (engine, stat, cost_time)
    else (engine, stat, cost_time)
  in
  let _, stat, cost_time = loop (engine, stat_empty, 0.0) in
  (stat, cost_time)
