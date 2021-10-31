module Spec = Specification.Spec
module F = Language.Oplang
module FInterp = Language.Oplang_interp
open Basic_dt
module P = Language.Piecewise

let iter_bound = 10

let synthesize_piecewise env max_length num_burn_in num_sampling =
  let rec force_converge cases =
    match cases with
    | [] -> raise @@ failwith (spf "Cannot find any perturbation function within the iteration bound(%i)" iter_bound)
    | [_, f] -> [], f
    | hd :: tl ->
      let cases, default = force_converge tl in hd :: cases, default
  in
  let rec loop current iter =
    if iter >= iter_bound || List.length current > max_length then
      force_converge current
    else
      let pres = List.map fst current in
      let bias x = List.for_all (fun pre -> Spec.eval pre x) pres in
      let env, cost =
        Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "") (
          fun () ->
            Mcmc.metropolis_hastings
              ~burn_in: num_burn_in
              ~sampling_steps: num_sampling
              ~proposal_distribution: Mutate.mutate
              ~cost_function: (Cost.biased_cost bias)
              ~init_distribution: env
        )
      in
      match env.cur_p with
      | None ->
        Zlog.log_write @@ Printf.sprintf "No result;";
        loop current (iter + 1)
      | Some cur_p ->
        Zlog.log_write @@ Printf.sprintf "prog(cost: %f):\n%s\n" cost (Language.Oplang.layout cur_p.prog);
        let pre = Pre.pre_infer_from_env env 2 in
        if Spec.is_true pre
        then current, cur_p.prog
        else if Spec.is_false pre
        then
          (Zlog.log_write ~log_level:LWarning @@ Printf.sprintf "Pre condition is false...";
           loop current (iter + 1))
        else loop (current @ [pre, cur_p.prog]) (iter + 1)
  in
  loop [] 0

let synthesize_one env num_burn_in num_sampling =
  synthesize_piecewise env 0 num_burn_in num_sampling
