open Primitive;;
open Sampling;;
module V = Value;;
open Basic_dt;;

let alpha_in_pre_is_err = 0.1
let alpha_out_pre_is_err = 0.5
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

let identity_panalty len = 1.5 *. float_of_int len
let duplicate_panalty = 2.0
let duplicate_level gh i j =
  if i == j then identity_panalty @@ List.length gh 
  else if j < i then
    let dup_times x = fst @@ List.fold_left (fun (dup_times, gidx) gtotal ->
        if gidx < gtotal then (dup_times + 1, gidx) else (dup_times, gidx)) (0, x) gh in
    let dup_times = dup_times j in
    let _ = Log.log_write @@ (Printf.sprintf "dup_times:%i" dup_times) in
    float_of_int @@ dup_times
  else 1.0
let cost_weighted_valid_iter
    (sigma: V.t list -> bool) (prog: V.t list -> (V.t list) option) (phi: V.t list -> bool) gh m jump_entry =
  let f i jopt =
    match jopt with
    | None -> alpha_none
    | Some j ->
      let k_dupliate = duplicate_level gh i j in
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
      delta *. k_dupliate
  in
  Array.meani f jump_entry

let cost_duplicate_iter jump_entry =
  let bound = Array.length jump_entry in
  let r = Array.fold_left (fun r j ->
      match j with
      | None -> 0.0
      | Some j ->
        if j >= bound then r else 1.0 +. r
    ) 0.0 jump_entry in
  r /. float_of_int (Array.length jump_entry)

let k_duplicate = 0.5
let k_valid = 0.5
let cal_cost (sigma: V.t list -> bool) (prog: V.t list -> (V.t list) option) (phi: V.t list -> bool) (cache: cache) =
  let sum = List.fold_lefti (fun sum i j ->
      (* let cost_valid = cost_valid_iter sigma prog phi cache.datam_rev j in *)
      (* let cost_duplicate = cost_duplicate_iter j in *)
      (* let () = Log.log_write (Printf.sprintf "cost_valid[%i]: %f" i cost_valid) in *)
      (* let () = Log.log_write (Printf.sprintf "cost_duplicate[%i]: %f" i cost_duplicate) in *)
      let cost = cost_weighted_valid_iter sigma prog phi cache.generation_hierarchy_rev cache.datam_rev j in
      let () = Log.log_write (Printf.sprintf "cost[%i]: %f" i cost) in
      sum +. cost
    ) 0.0 cache.jump_table in
  sum /. (float_of_int (List.length cache.jump_table))

let cost_ (sigma: V.t list -> bool) (client: V.t list -> (V.t list) option) (phi: V.t list -> bool) init_errs tps prog num =
  let () = Printf.printf "prog:\n%s\n" (Language.Oplang.layout prog) in
  let cache = Sampling.cost_sampling_ tps init_errs prog num in
  (* let () = Printf.printf "sample cache:\n%s\n" (Sampling.cache_layout cache) in *)
  let cost = cal_cost sigma client phi cache in
  let () = Printf.printf "cost = %f\n" cost in
  cost

(* TODO: Assert [random_int; cons] is much better than f(x, y) = (y, x) *)
let cost (env: Env.t) =
  let open Env in
  let () = Printf.printf "prog:\n%s\n" (Language.Oplang.layout env.cur_p.prog) in
  let () = Log.log_write (Printf.sprintf "prog:\n%s\n" (Language.Oplang.layout env.cur_p.prog)) in
  let scache = Sampling.cost_sampling env in
  let () = Log.log_write (Printf.sprintf "sample cache:\n%s\n" (Sampling.cache_layout scache)) in
  let cost = cal_cost env.sigma env.client env.phi scache in
  let () = Printf.printf "cost = %f\n" cost in
  cost
