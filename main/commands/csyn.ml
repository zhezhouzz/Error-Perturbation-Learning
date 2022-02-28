open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let syn_piecewise source_file meta_file qc_file max_length bound =
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Mkenv.random_init_prog
        @@ mk_env_from_files source_file meta_file)
  in
  let qc_conf = Qc_config.load_config qc_file in
  let result =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Syn.synthesize_piecewise env qc_conf max_length bound)
  in
  (env.i_err, result)

let syn source_file meta_file max_length bound =
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Mkenv.random_init_prog
        @@ mk_env_from_files source_file meta_file)
  in
  let result =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        (* Synthesizer.Syn.synthesize_piecewise env max_length num_burn_in *)
        (*   num_sampling *)
        Synthesizer.Syn.synthesize_multi_core env
          (fun _ -> true)
          max_length bound)
  in
  (env.i_err, result)

let pie_times = 30

let syn_pie name qc_file num_qc bound =
  let env = Zpie.Syn_pre.setting_decode_to_env name in
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> Synthesizer.Mkenv.random_init_prog env)
  in
  let qc_conf = Qc_config.load_config qc_file in
  let client_cond inp =
    match snd @@ env.client env.library_inspector inp with
    | None -> false
    | Some outp -> env.sigma inp && (not @@ env.phi (inp @ outp))
  in
  let test _ =
    let gtests, btests =
      Zquickcheck.Qc_baseline.make_tests qc_conf env.tps client_cond num_qc
    in
    (* let () = *)
    (*   List.iter *)
    (*     ~f:(fun d -> Printf.printf "data: %s\n" @@ Primitive.Value.layout_l d) *)
    (*     (gtests @ btests) *)
    (* in *)
    let pie_precond, pie_pre, pie_pre_correct, pie_precond_str =
      Zpie.Syn_pre.pie name (gtests, btests)
    in
    let () = Zlog.log_write @@ Printf.sprintf "pie pre: %s\n" pie_precond_str in
    (* let () = raise @@ failwith "zz" in *)
    let bias = pie_precond in
    try
      let prog = Synthesizer.Syn.synthesize_multi_time_bias env bias bound in
      let (_, num_none, data), cost_time =
        Zlog.event_time_
          (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
             "sampling") (fun () ->
            Sampling.Scache.eval_sampling [ env.i_err ]
              ((List.map ~f:snd @@ fst prog) @ [ snd prog ])
              (Primitive.Measure.mk_measure_cond env.i_err)
              env.sampling_rounds)
      in
      let data = List.filter ~f:client_cond data in
      (* let () = *)
      (*   List.iter *)
      (*     ~f:(fun d -> Printf.printf "data: %s\n" @@ Primitive.Value.layout_l d) *)
      (*     data *)
      (* in *)
      let gtests, btests = (gtests, btests @ data) in
      let pie_precond', pie_pre', pie_pre_correct', pie_precond_str' =
        Zpie.Syn_pre.pie name (gtests, btests)
      in
      let () =
        Zlog.log_write @@ Printf.sprintf "pie pre': %s\n" pie_precond_str'
      in
      (pie_pre_correct, pie_pre_correct')
    with _ -> (pie_pre_correct, false)
  in
  let res = List.init ~f:test pie_times in
  (* let c, _ = Basic_dt.List.split res in *)
  let bad = List.map ~f:(fun (a, b) -> not a) res in
  let improves = List.map ~f:(fun (a, b) -> (not a) && b) res in
  let cal_rate res =
    (float_of_int @@ List.length @@ List.filter ~f:(fun x -> x) res)
    /. (float_of_int @@ List.length res)
  in
  (cal_rate bad, cal_rate improves /. cal_rate bad)

let synthesize_piecewise =
  Command.basic ~summary:"synthesize-piecewise"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and max_length = anon ("maximal length of the pieces" %: int)
      and num_burn_in = anon ("num burn-in" %: int)
      and num_sampling = anon ("num sampling" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let i_err, result =
              syn_piecewise source_file meta_file qc_file max_length
                (Synthesizer.Syn.IterBound (num_burn_in, num_sampling))
            in
            let () =
              Printf.printf "%s\n"
              @@ Language.Piecewise.layout_with_i_err i_err result
            in
            ()))

let synthesize =
  Command.basic ~summary:"synthesize"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and max_length = anon ("maximal length of the pieces" %: int)
      and num_burn_in = anon ("num burn-in" %: int)
      and num_sampling = anon ("num sampling" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let i_err, result =
              syn source_file meta_file max_length
                (Synthesizer.Syn.IterBound (num_burn_in, num_sampling))
            in
            let () =
              Printf.printf "%s\n"
              @@ Language.Piecewise.layout_with_i_err i_err result
            in
            ()))

let synthesize_time =
  Command.basic ~summary:"synthesize-time"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and max_length = anon ("maximal length of the pieces" %: int)
      and bound_time = anon ("time in second" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let i_err, result =
              syn source_file meta_file max_length
                (Synthesizer.Syn.TimeBound (float_of_int bound_time))
            in
            let () =
              Printf.printf "%s\n"
              @@ Language.Piecewise.layout_with_i_err i_err result
            in
            ()))

let synthesize_pie =
  Command.basic ~summary:"synthesize-pie"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and name = anon ("pie benchmark name" %: string)
      and qc_file = anon ("qc file" %: regular_file)
      and num_qc = anon ("num qc" %: int)
      and bound_time = anon ("time in second" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let c, c_impr =
              syn_pie name qc_file num_qc (float_of_int bound_time)
            in
            let () =
              Printf.printf
                "name:%s\ninit_wrong_rate: %f%%\nimprove_rate: %f%%\n" name
                (c *. 100.0) (c_impr *. 100.0)
            in
            ()))

type bound_method = TimeBound | IterBound

let synthesize_all =
  Command.basic ~summary:"synthesize-all"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_configfile = anon ("source config file" %: regular_file)
      and num_syn = anon ("num synthesize" %: int)
      and bound_method = anon ("bound method" %: string) in
      fun () ->
        let bound_method =
          match bound_method with
          | "time" -> TimeBound
          | "iter" -> IterBound
          | _ ->
              raise @@ failwith "synthesize all: bad bound method [time|iter]"
        in
        let benchmarks = parse_benchmark_config source_configfile in
        let () =
          List.iter
            ~f:
              (fun {
                     benchname;
                     source_file;
                     meta_file;
                     max_length;
                     num_burn_in;
                     num_sampling;
                     time_bound;
                     output_dir;
                   } ->
              for i = 0 to num_syn - 1 do
                Config.make_dir output_dir;
                Config.exec_main configfile (fun () ->
                    let i_err, result =
                      syn source_file meta_file max_length
                        (match bound_method with
                        | IterBound ->
                            Synthesizer.Syn.IterBound (num_burn_in, num_sampling)
                        | TimeBound -> Synthesizer.Syn.TimeBound time_bound)
                    in
                    let output_file = sprintf "%s/%i.prog" output_dir i in
                    let () =
                      Core.Out_channel.write_all output_file
                        ~data:
                          (Language.Piecewise.layout_with_i_err i_err result)
                    in
                    let () =
                      Config.refresh_logfile (sprintf "%s_%i" benchname i)
                    in
                    ())
              done)
            benchmarks
        in
        ())
