module Spec = Specification.Spec
module F = Language.Oplang
module FInterp = Language.Oplang_interp
open Basic_dt
module P = Language.Piecewise
module V = Primitive.Value

let iter_bound = 10

type piecewise_state =
  | InitF of Env.t
  (* | NextPre of (Spec.t * F.t) list * F.t * Env.t *)
  | NextF of (Spec.t * F.t) list * F.t * Spec.t * V.t list list * Env.t

let length = function
  | InitF _ -> 0
  (* | NextPre (cases, _, _) -> List.length cases *)
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
  (* | NextPre (cases, f, _) *)
  | NextF (cases, f, _, _, _) ->
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

let synthesize_pre env init_set =
  let pre, (in_pre, out_pre) = Pre.pre_infer_from_env env init_set 2 in
  if List.length out_pre == 0 then FGoodEnough
  else if List.length in_pre == 0 then (
    Zlog.log_write ~log_level:LWarning
    @@ Printf.sprintf "Pre condition is false...";
    FTotalWrong)
  else
    let samples =
      List.filter
        (fun inp ->
          let _, output = env.client env.library_inspector inp in
          match output with
          | None -> false
          | Some output -> not @@ env.phi (inp @ output))
        (in_pre @ out_pre)
    in
    FIsOK (pre, samples)

let synthesize_piecewise env max_length num_burn_in num_sampling =
  let rec loop current iter =
    if iter >= iter_bound || length current >= max_length then
      force_converge current
    else
      let prev_cases, f, init_set =
        match current with
        | InitF env ->
            ( [],
              synthesize_f
                (fun _ -> true)
                [ env.i_err ] num_burn_in num_sampling env,
              [ env.i_err ] )
        | NextF (cases, f, pre, samples, env) ->
            let pres = List.map fst cases in
            let bias x =
              not @@ List.for_all (fun pre -> Spec.eval pre x) (pres @ [ pre ])
            in
            ( cases @ [ (pre, f) ],
              synthesize_f bias samples num_burn_in num_sampling env,
              samples )
      in
      match f with
      | None -> loop current (iter + 1)
      | Some (env, f) -> (
          if length current + 1 >= max_length then ([], f)
          else
            match synthesize_pre env init_set with
            | FGoodEnough -> (prev_cases, f)
            | FTotalWrong -> loop current (iter + 1)
            | FIsOK (pre, samples) ->
                Zlog.log_write @@ spf "Init Samples:\n%s\n"
                @@ List.split_by "\n" V.layout_l samples;
                loop (NextF (prev_cases, f, pre, samples, env)) (iter + 1))
  in
  loop (InitF env) 0

let synthesize_one env num_burn_in num_sampling =
  synthesize_piecewise env 0 num_burn_in num_sampling

(* bias on size of the result *)

let i_err_compare vl1 vl2 =
  List.compare (fun v1 v2 -> compare (V.size v1) (V.size v2)) vl1 vl2

let max_init_set = 5

let init_set_filter init_set =
  if List.length init_set < max_init_set then init_set
  else List.sublist (List.sort i_err_compare init_set) (0, max_init_set)

let log_show_init_set iter init_set =
  let () =
    Zlog.log_write
    @@ spf "iter(%i)\ninit_set:len:%i\n" iter
    @@ List.length init_set
  in
  let () =
    Zlog.log_write @@ spf "each lenL %s\n"
    @@ List.split_by "\n" V.layout_l_size init_set
  in
  let () =
    Zlog.log_write @@ spf "init_set:\n%s\n"
    @@ List.split_by "\n" V.layout_l init_set
  in
  ()

module S = Sampling.Scache
open Primitive

let synthesize_multi env max_length num_burn_in num_sampling =
  let rec loop current init_set iter =
    if iter >= iter_bound || List.length current >= max_length then current
    else
      match
        synthesize_f (fun _ -> true) init_set num_burn_in num_sampling env
      with
      | None -> raise @@ failwith "synthesize_f fails"
      | Some (env, new_f) ->
          let conds =
            S.mk_conds
              (Measure.mk_measure_cond env.i_err)
              env.sigma
              (fun v -> snd @@ env.client env.library_inspector v)
              env.phi
              (fun _ -> true)
          in
          let scache =
            S.mk_generation Config.Correct init_set conds new_f
              env.sampling_rounds
          in
          let good_list = S.Mem.all_outs_unique scache.mem in
          if List.length good_list == 0 then
            raise @@ failwith "synthesized f has no good result"
          else
            let init_set =
              Primitive.Value_aux.remove_duplicates_l (init_set @ good_list)
            in
            let () = log_show_init_set iter init_set in
            let init_set = init_set_filter init_set in
            let () = log_show_init_set iter init_set in
            loop (current @ [ new_f ]) init_set (iter + 1)
  in
  let fs : F.t list = loop [] [ env.i_err ] 0 in
  match fs with
  | [] -> raise @@ failwith "no perturbation function"
  | h :: t -> (List.map (fun f -> (Spec.dummy_pre env.tps, f)) t, h)
