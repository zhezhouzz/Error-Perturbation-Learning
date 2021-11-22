open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let baseline =
  Command.basic ~summary:"baseline"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and num = anon ("sampling number" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let env = mk_env_from_files source_file meta_file in
            let () =
              Printf.printf "init err: %s\n"
                (Primitive.Value.layout_l env.i_err)
            in
            let qc_conf = Qc_config.load_config qc_file in
            let (none_num, data), cost_time =
              Zlog.event_time_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                   "baseline sampling") (fun () ->
                  Zquickcheck.Qc_baseline.baseline qc_conf env.tps num)
            in
            let stat =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                   "baseline evaluation") (fun () ->
                  Evaluation.Ev.evaluation none_num data env.sigma
                    (fun inp -> snd @@ env.client env.library_inspector inp)
                    env.phi)
            in
            (* let () = Printf.printf "%s\n" @@ Evaluation.Ev.layout stat in *)
            let () =
              Printf.printf "%s\n"
              @@ Evaluation.Ev.layout_eval "" stat cost_time
            in
            ()))

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
            let () =
              Printf.printf "init err: %s\n"
                (Primitive.Value.layout_l env.i_err)
            in
            let qc_conf = Qc_config.load_config qc_file in
            let gen num =
              Zquickcheck.Qc_baseline.baseline qc_conf env.tps num
            in
            let measure = Primitive.Measure.mk_measure_cond env.i_err in
            let stat, cost_time =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                   "baseline evaluation") (fun () ->
                  Evaluation.Ev.timed_evaluation
                    (float_of_int time_in_second)
                    gen measure env.sigma
                    (fun inp -> snd @@ env.client env.library_inspector inp)
                    env.phi)
            in

            let () =
              Printf.printf "%s\n"
              @@ Evaluation.Ev.layout_eval "" stat cost_time
            in
            ()))

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
            let benchmarks = parse_benchmark_config source_configfile in
            let aux benchname source_file meta_file =
              let env = mk_env_from_files source_file meta_file in
              let () =
                Printf.printf "init err: %s\n"
                  (Primitive.Value.layout_l env.i_err)
              in
              let qc_conf = Qc_config.load_config qc_file in
              let gen num =
                Zquickcheck.Qc_baseline.baseline qc_conf env.tps num
              in
              let measure = Primitive.Measure.mk_measure_cond env.i_err in
              let stat, cost_time =
                Zlog.event_
                  (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                     "baseline evaluation") (fun () ->
                    Evaluation.Ev.timed_evaluation
                      (float_of_int time_in_second)
                      gen measure env.sigma
                      (fun inp -> snd @@ env.client env.library_inspector inp)
                      env.phi)
              in
              let output_file = sprintf "%s/%s.baseline" output_dir benchname in
              let () =
                Core.Out_channel.write_all output_file
                  ~data:(Evaluation.Ev.layout_eval benchname stat cost_time)
              in
              ()
            in
            let () =
              List.iter
                ~f:(fun (benchname, source_file, meta_file, _, _, _, _) ->
                  aux benchname source_file meta_file)
                benchmarks
            in
            ()))
