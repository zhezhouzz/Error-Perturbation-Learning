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

type syn_bound = TimeBound of float | IterBound of int * int

let get_result_from_mcmc (env, cost) =
  let open Env in
  match env.cur_p with
  | None ->
      Zlog.log_write @@ Printf.sprintf "No result;";
      None
  | Some cur_p ->
      Zlog.log_write
      @@ Printf.sprintf "prog(cost: %f):\n%s\n" cost
           (Language.Piecewise.layout_with_i_err env.i_err ([], cur_p.prog));
      Some (env, cur_p.prog)

let synthesize_f_moti bound env =
  let env' = Mkenv.random_init_prog env in
  let stop_step, (env, cost) =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        match bound with
        | IterBound (num_burn_in, num_sampling) ->
            Mcmc.metropolis_hastings_moti ~burn_in:num_burn_in
              ~sampling_steps:num_sampling ~proposal_distribution:Mutate.mutate
              ~cost_function:(Cost.biased_cost (fun _ -> true))
              ~init_distribution:env'
        | TimeBound time_bound ->
            Mcmc.metropolis_hastings_time_moti ~time_bound
              ~proposal_distribution:Mutate.mutate
              ~cost_function:(Cost.biased_cost (fun _ -> true))
              ~init_distribution:env')
  in
  let res = get_result_from_mcmc (env, cost) in
  (res, stop_step)

let synthesize_f_moti_record interval bound env =
  let env' = Mkenv.random_init_prog env in
  let rcd =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        match bound with
        | IterBound (num_burn_in, num_sampling) ->
            Mcmc.metropolis_hastings_moti_record ~burn_in:num_burn_in
              ~sampling_steps:num_sampling ~proposal_distribution:Mutate.mutate
              ~cost_function:(Cost.biased_cost (fun _ -> true))
              ~init_distribution:env' ~interval
        | TimeBound _ -> raise @@ failwith "record cannot accept time bound")
  in
  rcd

let synthesize_f bias samples bound env =
  let env' = Mkenv.random_init_prog env in
  let env' = Mkenv.update_init_sampling_set env' samples in
  let env, cost =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        match bound with
        | IterBound (num_burn_in, num_sampling) ->
            Mcmc.metropolis_hastings ~burn_in:num_burn_in
              ~sampling_steps:num_sampling ~proposal_distribution:Mutate.mutate
              ~cost_function:(Cost.biased_cost bias) ~init_distribution:env'
        | TimeBound time_bound ->
            Mcmc.metropolis_hastings_time ~time_bound
              ~proposal_distribution:Mutate.mutate
              ~cost_function:(Cost.biased_cost bias) ~init_distribution:env')
  in
  get_result_from_mcmc (env, cost)

let synthesize_f_free _ samples bound env =
  let env' = Mkenv.random_init_prog env in
  let env' = Mkenv.update_init_sampling_set env' samples in
  let env, cost =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        match bound with
        | IterBound (num_burn_in, num_sampling) ->
            Mcmc.metropolis_hastings ~burn_in:num_burn_in
              ~sampling_steps:num_sampling ~proposal_distribution:Mutate.mutate
              ~cost_function:(Cost.biased_cost ~if_free:true (fun _ -> true))
              ~init_distribution:env'
        | TimeBound time_bound ->
            Mcmc.metropolis_hastings_time ~time_bound
              ~proposal_distribution:Mutate.mutate
              ~cost_function:(Cost.biased_cost ~if_free:true (fun _ -> true))
              ~init_distribution:env')
  in
  get_result_from_mcmc (env, cost)

type state = FIsOK of (Spec.t * V.t list list)

(* let synthesize_pre env init_set = *)
(*   let pre, (in_pre, out_pre) = Pre.pre_infer_from_env env init_set 2 in *)
(*   if List.length out_pre == 0 then FGoodEnough *)
(*   else if List.length in_pre == 0 then ( *)
(*     Zlog.log_write ~log_level:LWarning *)
(*     @@ Printf.sprintf "Pre condition is false..."; *)
(*     FTotalWrong) *)
(*   else *)
(*     let samples = *)
(*       List.filter *)
(*         (fun inp -> *)
(*           let _, output = env.client env.library_inspector inp in *)
(*           match output with *)
(*           | None -> false *)
(*           | Some output -> not @@ env.phi (inp @ output)) *)
(*         (in_pre @ out_pre) *)
(*     in *)
(*     FIsOK (pre, samples) *)

module E = Sampling.Engine
open Primitive

let init_set_size = 4

let synthesize_pre_v2 env qc_conf prog =
  let sigma = env.Env.sigma_raw in
  let discovered_pre = Pre.infer_verified_pre env qc_conf prog sigma in
  let neg_engine =
    E.mk_perb_engine [ env.i_err ] (Measure.mk_measure_cond env.i_err) prog
  in
  let neg_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> not @@ env.phi (inp @ outp)
  in
  let _, init_set = E.sampling_num neg_filter init_set_size neg_engine in
  FIsOK (discovered_pre, init_set)

let synthesize_pre_multi env qc_conf prog =
  let sigma = env.Env.sigma_raw in
  let discovered_pre = Pre.infer_pre_multi env qc_conf prog sigma in
  discovered_pre

let synthesize_erroneous_pre env pos prog =
  let sigma = env.Env.sigma_raw in
  let discovered_pre = Pre.infer_erroneous_pre env pos prog sigma in
  discovered_pre

let synthesize_erroneous_pre_v3 env pos prog =
  let sigma = env.Env.sigma_raw in
  let discovered_pre = Pre.infer_erroneous_pre_v3 env pos prog sigma in
  discovered_pre

let synthesize_erroneous_pre_moti env qc_conf prog =
  let sigma = env.Env.sigma_raw in
  let discovered_pre = Pre.infer_erroneous_pre_moti env qc_conf prog sigma in
  discovered_pre

let synthesize_erroneous_pre_moti_pos env pos prog =
  let sigma = env.Env.sigma_raw in
  let discovered_pre = Pre.infer_erroneous_pre_moti_pos env pos prog sigma in
  discovered_pre

let synthesize_piecewise env qc_conf max_length bound =
  let rec loop current iter =
    let () = Zlog.log_write @@ spf "iter: %i" iter in
    if iter >= iter_bound || length current >= max_length then
      force_converge current
    else
      let prev_cases, f, _ =
        match current with
        | InitF env ->
            let res = synthesize_f (fun _ -> true) [ env.i_err ] bound env in
            let () =
              Zlog.log_write
              @@ spf "init f:\n %s"
                   (match res with
                   | None -> "none"
                   | Some (_, f) -> Language.Oplang.layout f)
            in
            ([], res, [ env.i_err ])
        | NextF (cases, f, pre, samples, env) ->
            (* let pres = List.map fst cases in *)
            (* let bias x = *)
            (*   not @@ List.for_all (fun pre -> Spec.eval pre x) (pres @ [ pre ]) *)
            (* in *)
            let bias x = Spec.eval pre x in
            let res = synthesize_f bias samples bound env in
            let () =
              Zlog.log_write
              @@ spf "init f:\n %s"
                   (match res with
                   | None -> "none"
                   | Some (_, f) -> Language.Oplang.layout f)
            in
            (cases @ [ (pre, f) ], res, samples)
      in
      match f with
      | None -> loop current (iter + 1)
      | Some (env, f) -> (
          if List.length prev_cases + 1 >= max_length then (prev_cases, f)
          else
            match synthesize_pre_v2 env qc_conf (prev_cases, f) with
            (* | FGoodEnough -> *)
            (*     Zlog.log_write @@ spf "FGoodEnough\n"; *)
            (*     (prev_cases, f) *)
            (* | FTotalWrong -> loop current (iter + 1) *)
            | FIsOK (pre, samples) ->
                Zlog.log_write
                @@ spf "Pre:\n%s\n"
                     (Spec.layout
                     @@ {
                          pre with
                          Spec.body =
                            Specification.Simplify.simplify_ite pre.Spec.body;
                        });
                Zlog.log_write @@ spf "Init Samples:\n%s\n"
                @@ List.split_by "\n" V.layout_l samples;
                loop (NextF (prev_cases, f, pre, samples, env)) (iter + 1))
  in
  loop (InitF env) 0

let synthesize_one env qc_conf bound = synthesize_piecewise env qc_conf 0 bound

(* bias on size of the result *)

let i_err_compare vl1 vl2 =
  List.compare (fun v1 v2 -> compare (V.len v1) (V.len v2)) vl1 vl2

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
    @@ List.split_by "\n" V.layout_l_len init_set
  in
  let () =
    Zlog.log_write @@ spf "init_set:\n%s\n"
    @@ List.split_by "\n" V.layout_l init_set
  in
  ()

module S = Sampling.Scache

exception SynthesisHasNoGoodResult

let synthesize_multi_core env bias max_length bound =
  let rec loop current init_set iter =
    if iter >= iter_bound || List.length current >= max_length then current
    else
      match synthesize_f bias init_set bound env with
      | None -> raise @@ failwith "synthesize_f fails"
      | Some (env, new_f) ->
          let conds =
            S.mk_conds env.measure_cond env.sigma
              (fun v -> snd @@ env.client env.library_inspector v)
              env.phi
              (fun _ -> true)
          in
          let scache =
            S.mk_generation Config.Correct init_set conds new_f
              env.sampling_rounds
          in
          let good_list = S.Mem.all_outs_unique scache.mem in
          if List.length good_list == 0 then raise SynthesisHasNoGoodResult
          else
            let init_set =
              Primitive.Value_aux.remove_duplicates_l (init_set @ good_list)
            in
            (* let () = log_show_init_set iter init_set in *)
            let init_set = init_set_filter init_set in
            (* let () = log_show_init_set iter init_set in *)
            loop (current @ [ new_f ]) init_set (iter + 1)
  in
  let fs : F.t list = loop [] [ env.i_err ] 0 in
  match fs with
  | [] -> raise @@ failwith "no perturbation function"
  | h :: t -> (List.map (fun f -> (Spec.dummy_pre env.tps, f)) t, h)

let synthesize_multif env bias times bound =
  let len (fs, default) =
    match default with None -> List.length fs | Some _ -> 1 + List.length fs
  in
  let counter = ref 0 in
  let rec loop (fs, default) =
    let () = counter := !counter + 1 in
    if !counter > (2 * times) + 1 then
      raise @@ failwith "synthesize_multi_times fails"
    else if len (fs, default) >= times then (fs, default)
    else
      let get_f if_free env bias =
        match
          (if if_free then synthesize_f_free else synthesize_f)
            bias [ env.Env.i_err ] bound env
        with
        | None -> raise @@ failwith "synthesize_multi_times fails"
        | Some (_, new_f) -> new_f
      in
      match default with
      | None -> loop (fs, Some (get_f false env bias))
      | Some _ -> loop (get_f true env bias :: fs, default)
  in
  let fs, default = loop ([], None) in
  match default with
  | None -> raise @@ failwith "die"
  | Some default ->
      let () = Zlog.log_write "synthesize_multif done" in
      (List.map (fun x -> (Spec.dummy_pre env.tps, x)) fs, default)

let synthesize_multi env max_length num_burn_in num_sampling =
  synthesize_multi_core env
    (fun _ -> true)
    max_length
    (IterBound (num_burn_in, num_sampling))

let synthesize_multi_time env max_length time_bound =
  synthesize_multi_core env (fun _ -> true) max_length (TimeBound time_bound)

let synthesize_multi_time_bias env bias time_bound =
  synthesize_multi_core env bias 1 (TimeBound time_bound)

let synthesize_multif_time env max_length time_bound =
  synthesize_multif env (fun _ -> true) max_length (TimeBound time_bound)
