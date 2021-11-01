module Spec = Specification.Spec
module F = Language.Oplang
module FInterp = Language.Oplang_interp
open Basic_dt
module P = Language.Piecewise
module V = Primitive.Value

let iter_bound = 10

type piecewise_state =
  | InitF of Env.t
  | NextPre of (Spec.t * F.t) list * F.t * Env.t
  | NextF of (Spec.t * F.t) list * F.t * Spec.t * V.t list list * Env.t

let length = function
  | InitF _ -> 0
  | NextPre (cases, _, _) -> List.length cases
  | NextF (cases, _, _, _, _) -> List.length cases

let force_converge current =
  match current with
  | InitF _ ->
      raise
      @@ failwith
           (spf
              "Cannot find any perturbation function within the iteration \
               bound(%i)"
              iter_bound)
  | NextPre (cases, f, _) | NextF (cases, f, _, _, _) ->
      Zlog.log_write ~log_level:LWarning @@ Printf.sprintf "force_converge!";
      (cases, f)

let synthesize_f bias samples num_burn_in num_sampling env =
  let env' = Mkenv.random_init_prog env in
  let env' = Mkenv.update_init_sampling_set env' samples in
  let env, cost =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Mcmc.metropolis_hastings ~burn_in:num_burn_in
          ~sampling_steps:num_sampling ~proposal_distribution:Mutate.mutate
          ~cost_function:(Cost.biased_cost bias) ~init_distribution:env')
  in
  match env.cur_p with
  | None ->
      Zlog.log_write @@ Printf.sprintf "No result;";
      None
  | Some cur_p ->
      Zlog.log_write
      @@ Printf.sprintf "prog(cost: %f):\n%s\n" cost
           (Language.Oplang.layout cur_p.prog);
      Some (env, cur_p.prog)

type state = FGoodEnough | FTotalWrong | FIsOK of (Spec.t * V.t list list)

let synthesize_pre env =
  let pre, (in_pre, out_pre) = Pre.pre_infer_from_env env 2 in
  if List.length out_pre == 0 then FGoodEnough
  else if List.length in_pre == 0 then (
    Zlog.log_write ~log_level:LWarning
    @@ Printf.sprintf "Pre condition is false...";
    FTotalWrong)
  else FIsOK (pre, out_pre)

(* let synthesize_piecewise_one current num_burn_in num_sampling env = *)
(*   match  *)
(*   let pres = List.map fst current in *)
(*   let bias = *)
(*     match pres with *)
(*     | [] -> fun _ -> false *)
(*     | _ -> fun x -> not @@ List.for_all (fun pre -> Spec.eval pre x) pres *)
(*   in *)
(*   match synthesize_f bias samples num_burn_in num_sampling env with *)
(*   | None -> None *)
(*   |  *)
(*   let env, cost = *)
(*     Zlog.event_ *)
(*       (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "") *)
(*       (fun () -> *)
(*         Mcmc.metropolis_hastings ~burn_in:num_burn_in *)
(*           ~sampling_steps:num_sampling ~proposal_distribution:Mutate.mutate *)
(*           ~cost_function:(Cost.biased_cost bias) *)
(*           ~init_distribution:(Mkenv.random_init_prog env)) *)
(*   in *)
(*   match env.cur_p with *)
(*   | None -> *)
(*       Zlog.log_write @@ Printf.sprintf "No result;"; *)
(*       None *)
(*   | Some cur_p -> *)
(*       Zlog.log_write *)
(*       @@ Printf.sprintf "prog(cost: %f):\n%s\n" cost *)
(*            (Language.Oplang.layout cur_p.prog); *)
(*       let pre = Pre.pre_infer_from_env env 2 in *)
(*       if Spec.is_true pre then Some (current, Some cur_p.prog) *)
(*       else if Spec.is_false pre then ( *)
(*         Zlog.log_write ~log_level:LWarning *)
(*         @@ Printf.sprintf "Pre condition is false..."; *)
(*         None) *)
(* else Some (current @ [ (pre, cur_p.prog) ], None) *)

let synthesize_piecewise env max_length num_burn_in num_sampling =
  let rec loop current iter =
    if iter >= iter_bound || length current > max_length then
      force_converge current
    else
      match current with
      | InitF env -> (
          match
            synthesize_f
              (fun _ -> true)
              [ env.i_err ] num_burn_in num_sampling env
          with
          | None -> loop current (iter + 1)
          | Some (env, f) -> loop (NextPre ([], f, env)) (iter + 1))
      | NextPre (cases, f, env) -> (
          match synthesize_pre env with
          | FGoodEnough -> (cases, f)
          | FTotalWrong -> loop current (iter + 1)
          | FIsOK (pre, samples) ->
              loop (NextF (cases, f, pre, samples, env)) (iter + 1))
      | NextF (cases, f, pre, samples, env) -> (
          let pres = List.map fst cases in
          let bias x =
            not @@ List.for_all (fun pre -> Spec.eval pre x) (pres @ [ pre ])
          in
          match synthesize_f bias samples num_burn_in num_sampling env with
          | None -> loop current (iter + 1)
          | Some (env, f') ->
              loop (NextPre (cases @ [ (pre, f) ], f', env)) (iter + 1))
  in
  loop (InitF env) 0

let synthesize_one env num_burn_in num_sampling =
  synthesize_piecewise env 0 num_burn_in num_sampling
