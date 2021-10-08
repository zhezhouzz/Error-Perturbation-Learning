open Classify;;
open Primitive;;
module V = Value;;

let perturbation_pre_infer (cctx: Cctx.t) (acache: Sampling.cache) (sigma: V.t list -> bool) (phi: V.t list -> bool) =
  let valuem, io_list = Sampling.pure_sampled_input_output acache in
  let io_list = List.filter (fun (in_idx, _) -> sigma @@ Hashtbl.find valuem in_idx) io_list in
  match io_list with
  | [] -> raise @@ failwith (Printf.sprintf "even have no initial error input in %s" __FUNCTION__)
  | _ ->
    let to_value (in_idx, _) = Hashtbl.find valuem in_idx in
    let to_label (_, out_idx) = not (phi @@ Hashtbl.find valuem out_idx) in
    let _ = Infer.spec_infer cctx io_list to_value to_label in
    ()
