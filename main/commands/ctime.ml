open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let run_time env time_in_second gen ev_tp =
  let measure = Primitive.Measure.mk_measure_cond env.Synthesizer.Env.i_err in
  let stat, cost_time =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
         "baseline evaluation") (fun () ->
        Evaluation.Ev.timed_evaluation
          (float_of_int time_in_second)
          gen measure env.sigma
          (fun inp -> snd @@ env.client env.library_inspector inp)
          env.phi ev_tp)
  in
  let () =
    Printf.printf "%s\n" @@ Evaluation.Ev.layout_eval "" stat cost_time
  in
  ()

let baseline_time =
  Command.basic ~summary:"baseline-time"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and time_in_second = anon ("total_time in second" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let env = mk_env_from_files source_file meta_file in
            let qc_conf = Qc_config.load_config qc_file in
            let gen num =
              Zquickcheck.Qc_baseline.baseline qc_conf env.tps num
            in
            run_time env time_in_second gen Evaluation.Ev.Qc))

let sampling_time =
  Command.basic ~summary:"sampling-time"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and prog_file = anon ("prog file" %: regular_file)
      and time_in_second = anon ("total_time in second" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let env = mk_env_from_files source_file meta_file in
            let i_err, prog = Parse.parse_piecewise prog_file in
            let env = Synthesizer.Mkenv.update_i_err env i_err in
            let gen num =
              Sampling.Scache.eval_sampling [ env.i_err ]
                ((List.map ~f:snd @@ fst prog) @ [ snd prog ])
                (Primitive.Measure.mk_measure_cond env.i_err)
                num
            in
            run_time env time_in_second gen Evaluation.Ev.Perturb))

let run_time_all source_configfile time_in_second test_output_dir gen_gen ev_tp
    =
  let open Synthesizer.Env in
  let benchmarks = parse_benchmark_config source_configfile in
  let aux bench =
    let { benchname; source_file; meta_file; time_bound; _ } = bench in
    let env = mk_env_from_files source_file meta_file in
    let measure = Primitive.Measure.mk_measure_cond env.i_err in
    let env, gen = gen_gen bench env in
    let stat, cost_time =
      Zlog.event_
        (Printf.sprintf "%s:%i[%s]-%s evaluation" __FILE__ __LINE__ __FUNCTION__
           benchname) (fun () ->
          Evaluation.Ev.timed_evaluation
            (float_of_int time_in_second)
            gen measure env.sigma
            (fun inp -> snd @@ env.client env.library_inspector inp)
            env.phi ev_tp)
    in
    let output_file = sprintf "%s/%s.baseline" test_output_dir benchname in
    let learning_time =
      match ev_tp with
      | Evaluation.Ev.Qc -> 0.0
      | Evaluation.Ev.Perturb -> time_bound
    in
    let () =
      Core.Out_channel.write_all output_file
        ~data:
          (Evaluation.Ev.layout_eval benchname stat
             (cost_time +. learning_time))
    in
    ()
  in
  let () = List.iter ~f:aux benchmarks in
  ()

let baseline_time_all =
  Command.basic ~summary:"baseline-time-all"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_configfile = anon ("source config file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and output_dir = anon ("output dir" %: string)
      and time_in_second = anon ("total_time in second" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let qc_conf = Qc_config.load_config qc_file in
            let gen_gen _ env =
              ( env,
                fun num ->
                  Zquickcheck.Qc_baseline.baseline qc_conf
                    env.Synthesizer.Env.tps num )
            in
            let learning_time _ = 0.0 in
            run_time_all source_configfile time_in_second output_dir gen_gen
              Evaluation.Ev.Qc))

(* TODO: mkdir if not exists *)
let sampling_time_all =
  Command.basic ~summary:"sampling-time-all"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_configfile = anon ("source config file" %: regular_file)
      and test_output_dir = anon ("test output dir" %: string)
      and time_in_second = anon ("total_time in second" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let gen_gen { output_dir; _ } env =
              let prog_file = sprintf "%s/0.prog" output_dir in
              let i_err, prog = Parse.parse_piecewise prog_file in
              let env = Synthesizer.Mkenv.update_i_err env i_err in
              ( env,
                fun num ->
                  Sampling.Scache.eval_sampling [ env.i_err ]
                    ((List.map ~f:snd @@ fst prog) @ [ snd prog ])
                    (Primitive.Measure.mk_measure_cond env.i_err)
                    num )
            in
            run_time_all source_configfile time_in_second test_output_dir
              gen_gen Evaluation.Ev.Perturb))
