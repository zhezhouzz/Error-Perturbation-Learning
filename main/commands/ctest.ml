open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let test_mcmc env =
  let open Synthesizer in
  let env, cost =
    Mcmc.metropolis_hastings ~burn_in:300 ~sampling_steps:30
      ~proposal_distribution:Mutate.mutate
      ~cost_function:(Cost.biased_cost (fun _ -> true))
      ~init_distribution:env
  in
  let () =
    match env.cur_p with
    | Some cur_p ->
        Printf.printf "prog(cost: %f):\n%s\n" cost
          (Language.Oplang.layout cur_p.prog)
    | None -> Printf.printf "No result; prog(cost: ??):\n/?\n"
  in
  env

let test_oplang () =
  let open Synthesizer in
  let () = Language.Oplang.test () in
  ()

let test_feature () =
  let qv = [ (Primitive.Tp.Int, "u"); (Primitive.Tp.Int, "v") ] in
  let args = [ (Primitive.Tp.IntList, "l1"); (Primitive.Tp.IntList, "l2") ] in
  let mps = [ "mem"; "hd"; "<" ] in
  let inferctx = Classify.Cctx.mk_cctx args qv mps in
  let () =
    Printf.printf "fset: %s\n"
      (Classify.Feature.layout_set inferctx.Classify.Cctx.fset)
  in
  let module V = Primitive.Value in
  let spec =
    Classify.Infer.spec_infer inferctx
      [ [ V.L [ 1; 2 ]; V.L [ 3; 4 ] ]; [ V.L [ 3; 4 ]; V.L [ 1; 2 ] ] ]
      (fun x -> x)
      (function [ V.L [ 1; 2 ]; V.L [ 3; 4 ] ] -> true | _ -> false)
    (* (fun v -> not (Primitive.Imp.phi_merge v)) *)
    (* Primitive.Imp.sigma_merge *)
  in
  ()

(* type gen_version = *)
(*    | QcGen | RGen *)
open Basic_dt

let test_random_gen conf tp num =
  let res = QCheck.Gen.generate ~n:num @@ Zquickcheck.Qc.choose_gen conf tp in
  Printf.printf "%s:\n%s\n" (Primitive.Tp.layout tp)
  @@ List.split_by "\n\n"
       (fun x ->
         match x with None -> "none" | Some x -> Primitive.Value.layout x)
       res

let test_pre_infer env =
  let prog = Parse.parse "data/pre.prog" in
  let open Synthesizer in
  let scache =
    Sampling.Scache.mk_generation_measure_only
      (Primitive.Measure.mk_measure_cond env.Env.i_err)
      [ env.Env.i_err ] prog env.sampling_rounds
  in
  let qv = [ (Primitive.Tp.Int, "u"); (Primitive.Tp.Int, "v") ] in
  let args = prog.fin in
  let mps = [ "mem"; "hd"; "<" ] in
  let cctx =
    Classify.Cctx.mk_cctx
      [ (Primitive.Tp.IntList, "x1"); (Primitive.Tp.IntList, "x2") ]
      qv mps
  in
  let () =
    Printf.printf "fset: %s\n"
      (Classify.Feature.layout_set cctx.Classify.Cctx.fset)
  in
  let spec =
    Pre.perturbation_pre_infer cctx scache env.Synthesizer.Env.sigma
      (fun v -> snd @@ env.client env.library_inspector v)
      env.Synthesizer.Env.phi
  in
  ()

let batched_test source_file meta_file num_times num_burn_in num_sampling =
  let open Synthesizer in
  let rec aux n =
    if n >= num_times then ()
    else
      let env =
        Zlog.event_
          (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
          (fun () -> mk_env_from_files source_file meta_file)
      in
      let env, cost =
        Zlog.event_
          (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
          (fun () ->
            Mcmc.metropolis_hastings ~burn_in:num_burn_in
              ~sampling_steps:num_sampling ~proposal_distribution:Mutate.mutate
              ~cost_function:(Cost.biased_cost (fun _ -> true))
              ~init_distribution:env)
      in
      let () =
        match env.cur_p with
        | Some cur_p ->
            Zlog.log_write
            @@ Printf.sprintf "prog(cost: %f):\n%s\n" cost
                 (Language.Oplang.layout cur_p.prog)
        | None ->
            Zlog.log_write @@ Printf.sprintf "No result; prog(cost: ??):\n/?\n"
      in
      (* let () = Zlog.log_write @@ Printf.sprintf "prog(cost: %f):\n%s\n" cost (Language.Oplang.layout env.cur_p.prog) in *)
      let () = Config.refresh_logfile (string_of_int n) in
      aux (n + 1)
  in
  aux 0

let test_ifc () =
  let () = Ifc.Test.test () in
  ()

let test =
  Command.basic ~summary:"test."
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and test_name = anon ("test name" %: string) in
      fun () ->
        Config.exec_main configfile (fun () ->
            (* event "test" (fun () -> Printf.printf "test!\n"; Language.Arg_solving.test ()) *)
            match test_name with
            | "mcmc" ->
                Zlog.event_
                  (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                     "") (fun () ->
                    Printf.printf "test!\n";
                    let env =
                      Zlog.event_
                        (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__
                           __FUNCTION__ "") (fun () ->
                          Synthesizer.Mkenv.random_init_prog
                          @@ mk_standard_env ())
                    in
                    let env = test_mcmc env in
                    ())
            | "oplang" ->
                Zlog.event_
                  (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                     "") (fun () ->
                    Printf.printf "test!\n";
                    test_oplang ())
            | "feature" ->
                Zlog.event_
                  (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                     "") (fun () ->
                    Printf.printf "test!\n";
                    test_feature ())
            | "pre" ->
                Zlog.event_
                  (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                     "") (fun () ->
                    Printf.printf "test!\n";
                    let env =
                      Zlog.event_
                        (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__
                           __FUNCTION__ "") (fun () ->
                          Synthesizer.Mkenv.random_init_prog
                          @@ mk_standard_env ())
                    in
                    test_pre_infer env)
            | "ifc" ->
                Zlog.event_
                  (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                     "") (fun () ->
                    Printf.printf "test!\n";
                    test_ifc ())
            | _ -> raise @@ failwith "unknown test name"))

let batched_test =
  Command.basic ~summary:"batched test."
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and num_times = anon ("num times" %: int)
      and num_burn_in = anon ("num burn-in" %: int)
      and num_sampling = anon ("num sampling" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            batched_test source_file meta_file num_times num_burn_in
              num_sampling))

let qcgen_test =
  Command.basic ~summary:"batched test."
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and qc_file = anon ("qcconfigfile" %: regular_file)
      and tp = anon ("datatype" %: string)
      and num = anon ("num times" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let qc_conf = Qc_config.load_config qc_file in
            let tp = Primitive.Tp.of_string tp in
            test_random_gen qc_conf tp num))
