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
  Printf.sprintf
    "sampled: %i\n\
    \  total: %s\n\
    \  succ: %s\n\
    \  in sigma: %s\n\
    \  in sigma out phi: %s\n\
    \  in sigma out phi unique: %s\n"
    sampling_num (aux total_num) (aux succ_num) (aux in_sigma_num)
    (aux in_sigma_out_phi_num)
    (aux in_sigma_out_phi_unique_num)

let no_dup_counter () = Hashtbl.create 20000

let add_one tbl e = Hashtbl.add tbl e ()

let get_count tbl = Hashtbl.length tbl

let evaluation (num_none : int) (data : V.t list list)
    (sigma : V.t list -> bool) (client : V.t list -> V.t list option)
    (phi : V.t list -> bool) =
  let sampling_num = num_none + List.length data in
  let () = Printf.printf "sampling num: %i\n" sampling_num in
  let arr = Array.of_list data in
  let total = List.init (List.length data) (fun i -> i) in
  (* let c = ref 0 in *)
  let total_num = List.length total in
  let () = Printf.printf "total num: %i\n" total_num in
  (* let () = raise @@ failwith "end" in *)
  let succ_data =
    List.filter_map
      (fun idx ->
        match client arr.(idx) with None -> None | Some _ -> Some idx)
      total
  in
  let succ_num = List.length succ_data in
  let () = Printf.printf "succ num: %i\n" succ_num in
  let in_sigma_data = List.filter (fun idx -> sigma arr.(idx)) succ_data in
  let in_sigma_num = List.length in_sigma_data in
  let () = Printf.printf "in_sigma_num: %i\n" in_sigma_num in
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
  (*     (fun (i, o) -> Printf.sprintf "%s -> %s" (V.layout_l i) (V.layout_l o)) *)
  (*     in_sigma_out_phi_data *)
  (* in *)
  let c = no_dup_counter () in
  let () = List.iter (fun idx -> add_one c arr.(idx)) in_sigma_out_phi_data in
  let in_sigma_out_phi_unique_num = get_count c in
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
