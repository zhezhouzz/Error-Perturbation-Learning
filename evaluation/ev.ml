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
  data_num : int;
  total_num : int;
  succ_num : int;
  in_sigma_num : int;
  in_sigma_out_phi_num : int;
  in_sigma_out_phi_unique_num : int;
}

let layout
    {
      data_num;
      total_num;
      succ_num;
      in_sigma_num;
      in_sigma_out_phi_num;
      in_sigma_out_phi_unique_num;
    } =
  let aux x =
    Printf.sprintf "%i(%f%s)" x
      (100.0 *. float_of_int x /. float_of_int data_num)
      "%"
  in
  Printf.sprintf
    "sampled: %i\n\
    \  total: %s\n\
    \  succ: %s\n\
    \  in sigma: %s\n\
    \  in sigma out phi: %s\n\
    \  in sigma out phi unique: %s\n"
    data_num (aux total_num) (aux succ_num) (aux in_sigma_num)
    (aux in_sigma_out_phi_num)
    (aux in_sigma_out_phi_unique_num)

let evaluation (data : V.t list option list) (sigma : V.t list -> bool)
    (client : V.t list -> V.t list option) (phi : V.t list -> bool) =
  let data_num = List.length data in
  let total =
    List.filter_map
      (fun inp ->
         match inp with None -> None | Some inp -> Some (inp, client inp))
      data
  in
  let total_num = List.length total in
  let succ_data =
    List.filter_map
      (fun (inp, output) ->
         match output with None -> None | Some output -> Some (inp, output))
      total
  in
  let succ_num = List.length succ_data in
  let in_sigma_data = List.filter (fun (inp, _) -> sigma inp) succ_data in
  let in_sigma_num = List.length in_sigma_data in
  let in_sigma_out_phi_data =
    List.filter (fun (inp, output) -> not @@ phi @@ inp @ output) in_sigma_data
  in
  let in_sigma_out_phi_num = List.length in_sigma_out_phi_data in
  let () =
    Zlog.log_write
    @@ Printf.sprintf "in_sigma_out_phi:\n%s\n"
    @@ Basic_dt.List.split_by "\n"
      (fun (i, o) -> Printf.sprintf "%s -> %s" (V.layout_l i) (V.layout_l o))
      in_sigma_out_phi_data
  in
  let in_sigma_out_phi_unique_data =
    Basic_dt.List.remove_duplicates
      (fun (inp, _) (inp', _) -> List.equal V.eq inp inp')
      in_sigma_out_phi_data
  in
  let in_sigma_out_phi_unique_num = List.length in_sigma_out_phi_unique_data in
  {
    data_num;
    total_num;
    succ_num;
    in_sigma_num;
    in_sigma_out_phi_num;
    in_sigma_out_phi_unique_num;
  }
