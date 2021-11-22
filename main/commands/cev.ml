open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let eval_baseline =
  Command.basic ~summary:"eval-baseline"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_configfile = anon ("source config file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and num_baseline = anon ("baseline sampling number" %: int)
      and num = anon ("sampling number" %: int) in
      fun () ->
        let qc_conf = Qc_config.load_config qc_file in
        let benchmarks = parse_benchmark_config source_configfile in
        let () =
          Config.exec_main configfile (fun () ->
              List.iter
                ~f:
                  (fun ( benchname,
                         source_file,
                         meta_file,
                         max_length,
                         num_burn_in,
                         num_sampling,
                         output_dir ) ->
                  let env = mk_env_from_files source_file meta_file in
                  let (none_num, data), cost_time =
                    Zlog.event_time_
                      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__
                         __FUNCTION__ "baseline sampling") (fun () ->
                        Zquickcheck.Qc_baseline.baseline qc_conf env.tps
                          num_baseline)
                  in
                  let stat =
                    Zlog.event_
                      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__
                         __FUNCTION__ "baseline evaluation") (fun () ->
                        Evaluation.Ev.evaluation none_num data env.sigma
                          (fun inp ->
                            snd @@ env.client env.library_inspector inp)
                          env.phi)
                  in
                  let () =
                    Printf.printf "%s\n"
                    @@ Evaluation.Ev.layout_eval benchname stat cost_time
                  in
                  ())
                benchmarks)
        in
        ())

let analysis =
  Command.basic ~summary:"analysis"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and prog_file = anon ("prog file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and baseline_num = anon ("baseline sampling number" %: int)
      and num = anon ("sampling number" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let print_data_to_log name data =
              Zlog.log_write
              @@ Printf.sprintf "%s(%i):\n%s" name (List.length data)
              @@ Basic_dt.List.split_by "\n" Primitive.Value.layout_l data
            in
            let env = mk_env_from_files source_file meta_file in
            let () =
              Printf.printf "init err: %s\n"
                (Primitive.Value.layout_l env.i_err)
            in
            let qc_conf = Qc_config.load_config qc_file in
            let (none_num, baseline_data), cost_time =
              Zlog.event_time_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                   "baseline sampling") (fun () ->
                  Zquickcheck.Qc_baseline.baseline qc_conf env.tps baseline_num)
            in
            let baseline_data =
              List.filter
                ~f:(fun inp ->
                  let open Synthesizer.Env in
                  if env.sigma inp then
                    match env.client env.library_inspector inp with
                    | _, None -> false
                    | _, Some outp -> not @@ env.phi (inp @ outp)
                  else false)
                baseline_data
            in
            let () = print_data_to_log "baseline_data" baseline_data in
            let nmethod = Analysis.Iso.Value_shift_to_min_zero_normalize in
            let baseline_data' = Analysis.Iso.normalize nmethod baseline_data in
            let () = print_data_to_log "baseline_data'" baseline_data' in
            let i_err, prog = Parse.parse_piecewise prog_file in
            let env = Synthesizer.Mkenv.update_i_err env i_err in
            let (num_none, pertur_data), cost_time =
              Zlog.event_time_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                   "sampling") (fun () ->
                  Sampling.Scache.eval_sampling [ env.i_err ]
                    ((List.map ~f:snd @@ fst prog) @ [ snd prog ])
                    (Primitive.Measure.mk_measure_cond env.i_err)
                    num)
            in
            let () = print_data_to_log "pertur_data" pertur_data in
            let pertur_data' = Analysis.Iso.normalize nmethod pertur_data in
            let () = print_data_to_log "pertur_data'" pertur_data' in
            let only_baseline, only_pertur, common =
              Primitive.Value_aux.intersection baseline_data' pertur_data'
            in
            let () = print_data_to_log "only_baseline" only_baseline in
            let () = print_data_to_log "only_pertur" only_pertur in
            let () = print_data_to_log "common" common in
            let () =
              Printf.printf "baseline -> (%i ( %i ) %i) <- pertur\n"
                (List.length only_baseline)
                (List.length common) (List.length only_pertur)
            in
            ()))

let eval_result =
  Command.basic ~summary:"eval-result"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_configfile = anon ("source config file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and num = anon ("sampling number" %: int) in
      fun () ->
        let qc_conf = Qc_config.load_config qc_file in
        let benchmarks = parse_benchmark_config source_configfile in
        let () =
          Config.exec_main configfile (fun () ->
              List.iter
                ~f:
                  (fun ( benchname,
                         source_file,
                         meta_file,
                         max_length,
                         num_burn_in,
                         num_sampling,
                         output_dir ) ->
                  let env = mk_env_from_files source_file meta_file in
                  let i_err, prog =
                    Parse.parse_piecewise (sprintf "%s/0.prog" output_dir)
                  in
                  let env = Synthesizer.Mkenv.update_i_err env i_err in
                  let (none_num, data), cost_time =
                    Zlog.event_time_
                      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__
                         __FUNCTION__ "sampling") (fun () ->
                        Sampling.Scache.eval_sampling [ env.i_err ]
                          ((List.map ~f:snd @@ fst prog) @ [ snd prog ])
                          (Primitive.Measure.mk_measure_cond env.i_err)
                          num_sampling)
                  in
                  let stat =
                    Zlog.event_
                      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__
                         __FUNCTION__ "evaluation") (fun () ->
                        Evaluation.Ev.evaluation none_num data env.sigma
                          (fun inp ->
                            snd @@ env.client env.library_inspector inp)
                          env.phi)
                  in
                  let () =
                    Printf.printf "%s\n"
                    @@ Evaluation.Ev.layout_eval benchname stat cost_time
                  in
                  ())
                benchmarks)
        in
        ())

let sampling =
  Command.basic ~summary:"sampling"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and prog_file = anon ("prog file" %: regular_file)
      and num_sampling = anon ("num sampling" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let env = mk_env_from_files source_file meta_file in
            let i_err, prog = Parse.parse_piecewise prog_file in
            let env = Synthesizer.Mkenv.update_i_err env i_err in
            let (num_none, data), cost_time =
              Zlog.event_time_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                   "sampling") (fun () ->
                  Sampling.Scache.eval_sampling [ env.i_err ]
                    ((List.map ~f:snd @@ fst prog) @ [ snd prog ])
                    (Primitive.Measure.mk_measure_cond env.i_err)
                    num_sampling)
            in
            let stat =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                   "evaluation") (fun () ->
                  Evaluation.Ev.evaluation num_none data env.sigma
                    (fun inp -> snd @@ env.client env.library_inspector inp)
                    env.phi)
            in
            let () =
              Printf.printf "%s\n"
              @@ Evaluation.Ev.layout_eval "" stat cost_time
            in
            ()))

let run_cost env progs =
  let open Synthesizer.Env in
  let cost i_err name prog =
    let env = Synthesizer.Mkenv.update_i_err env i_err in
    let measure = Primitive.Measure.mk_measure_cond env.i_err in
    let () = Zlog.log_write @@ sprintf "[%s]" name in
    let conds =
      Sampling.Scache.mk_conds measure env.sigma
        (fun v -> snd @@ env.client env.library_inspector v)
        env.phi
        (fun _ -> true)
    in
    let scache =
      Sampling.Scache.mk_generation !Config.conf.bias_method [ env.i_err ] conds
        prog env.sampling_rounds
    in
    let cost =
      Synthesizer.Cost.cal_cost conds
        (fun v -> env.client env.library_inspector v)
        env.i_err_non_trivial_info scache
    in
    cost
  in
  List.iter
    ~f:(fun (i_err, (name, f)) ->
      Printf.printf "%s\ncost: %f\n\n" name (cost i_err name f))
    progs

let costing =
  Command.basic ~summary:"costing"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and prog_config = anon ("prog config" %: regular_file) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let env = mk_env_from_files source_file meta_file in
            let progs =
              let open Yojson.Basic.Util in
              let progs = Config.load_json prog_config |> to_list in
              let progs =
                List.map
                  ~f:(fun x ->
                    let i_err, prog = Parse.parse_piecewise @@ to_string x in
                    (i_err, (to_string x, snd @@ prog)))
                  progs
              in
              progs
            in
            let () = run_cost env progs in
            ()))
