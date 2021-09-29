open Primitive;;
open Sampling;;
module V = Value;;

let alpha_in_pre_is_err = 0.0
let alpha_out_pre_is_err = 0.3
let alpha_out_pre_not_err = 0.7
let alpha_none = 1.0
let cost_valid_iter (sigma: V.t list -> bool) (prog: V.t list -> (V.t list) option) (phi: V.t list -> bool) m jump_entry =
  let num = Array.length jump_entry in
  let r = Array.fold_left (fun r j ->
      match j with
      | None -> alpha_none
      | Some j ->
        let v = Hashtbl.find m j in
        let delta =
          match prog v with
          | None -> alpha_none
          | Some v' ->
            if phi v'
            then alpha_out_pre_not_err
            else (if sigma v
                  then alpha_in_pre_is_err
                  else alpha_out_pre_is_err)
        in
        r +. delta
    ) 0.0 jump_entry
  in
  r /. float_of_int num

let cal_cost (sigma: V.t list -> bool) (prog: V.t list -> (V.t list) option) (phi: V.t list -> bool) (cache: cache) =
  let sum = List.fold_left (fun sum j ->
      sum +. (cost_valid_iter sigma prog phi cache.datam_rev j)
    ) 0.0 cache.jump_table in
  sum /. (float_of_int (List.length cache.jump_table))

let cost_ (sigma: V.t list -> bool) (client: V.t list -> (V.t list) option) (phi: V.t list -> bool) init_errs tps prog num =
  let () = Printf.printf "prog:\n%s\n" (Language.Oplang.layout prog) in
  let cache = Sampling.cost_sampling_ tps init_errs prog num in
  let () = Printf.printf "sample cache:\n%s\n" (Sampling.cache_layout cache) in
  let cost = cal_cost sigma client phi cache in
  let () = Printf.printf "cost = %f\n" cost in
  cost

let cost (env: Env.t) =
  let open Env in
  let () = Printf.printf "prog:\n%s\n" (Language.Oplang.layout env.cur_p.prog) in
  let scache = Sampling.cost_sampling env in
  let () = Printf.printf "sample cache:\n%s\n" (Sampling.cache_layout scache) in
  let cost = cal_cost env.sigma env.client env.phi scache in
  let () = Printf.printf "cost = %f\n" cost in
  cost
