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

let pie_times = 1

let pf_additional_num = 5

let syn_pie name qc_file num_qc num_qc2 bound =
  let qc_conf = Qc_config.load_config qc_file in
  let tps, client_cond = Zpie.Syn_pre.setting_decode_to_cond name in
  let test _ =
    let gtests, btests =
      Zquickcheck.Qc_baseline.make_tests qc_conf tps client_cond num_qc
    in
    (* let () = *)
    (*   List.iter *)
    (*     ~f:(fun d -> Printf.printf "data: %s\n" @@ Primitive.Value.layout_l d) *)
    (*     (gtests @ btests) *)
    (* in *)
    let pie_precond, pie_pre, pie_pre_correct, pie_precond_str =
      Zpie.Syn_pre.pie name (gtests @ btests)
    in
    let () = Zlog.log_write @@ Printf.sprintf "pie pre: %s\n" pie_precond_str in
    let gtests2, btests2 =
      Zquickcheck.Qc_baseline.make_tests qc_conf tps client_cond num_qc2
    in
    let pie_precond2, pie_pre2, pie_pre_correct2, pie_precond_str2 =
      Zpie.Syn_pre.pie name (gtests @ btests @ gtests2 @ btests2)
    in
    let () =
      Zlog.log_write @@ Printf.sprintf "pie pre2: %s\n" pie_precond_str2
    in
    (* let () = raise @@ failwith "zz" in *)
    let bias = pie_precond in
    try
      let i_err =
        if Int.equal 0 @@ List.length btests then None
        else Some (QCheck.Gen.generate1 (QCheck.Gen.oneofl btests))
      in
      let env = Zpie.Syn_pre.setting_decode_to_env name i_err in
      let env =
        Zlog.event_
          (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
          (fun () -> Synthesizer.Mkenv.random_init_prog env)
      in
      let prog = Synthesizer.Syn.synthesize_multi_time_bias env bias bound in
      let (_, num_none, data), cost_time =
        Zlog.event_time_
          (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
             "sampling") (fun () ->
            Sampling.Scache.eval_sampling [ env.i_err ]
              ((List.map ~f:snd @@ fst prog) @ [ snd prog ])
              (Primitive.Measure.mk_measure_cond env.i_err)
              pf_additional_num)
      in
      (* let () = *)
      (*   List.iter *)
      (*     ~f:(fun d -> Printf.printf "data: %s\n" @@ Primitive.Value.layout_l d) *)
      (*     data *)
      (* in *)
      let pie_precond', pie_pre', pie_pre_correct', pie_precond_str' =
        Zpie.Syn_pre.pie name (gtests @ btests @ data)
      in
      let () =
        Zlog.log_write @@ Printf.sprintf "pie pre': %s\n" pie_precond_str'
      in
      (pie_pre_correct, pie_pre_correct2, pie_pre_correct')
    with _ -> (pie_pre_correct, pie_pre_correct2, false)
  in
  let res = List.init ~f:test pie_times in
  (* let c, _ = Basic_dt.List.split res in *)
  let qc = List.map ~f:(fun (a, _, _) -> a) res in
  let qc2 = List.map ~f:(fun (_, b, _) -> b) res in
  (* let () = *)
  (*   Printf.printf "qc:\n%s\n" (Basic_dt.List.to_string string_of_bool qc) *)
  (* in *)
  let qc_pf = List.map ~f:(fun (_, _, c) -> c) res in
  (* let improves = List.map ~f:(fun (a, b) -> (not a) && b) res in *)
  let cal_rate res =
    (float_of_int @@ List.length @@ List.filter ~f:(fun x -> x) res)
    /. (float_of_int @@ List.length res)
  in
  (cal_rate qc, cal_rate qc2, cal_rate qc_pf)

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
      and num_qc2 = anon ("num qc2" %: int)
      and bound_time = anon ("time in second" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let c, c2, c_impr =
              syn_pie name qc_file num_qc num_qc2 (float_of_int bound_time)
            in
            let () =
              Printf.printf
                "name:%s\nqc_acc: %f%%\nqc2_acc: %f%%\npf_acc: %f%%\n" name
                (c *. 100.0) (c2 *. 100.0) (c_impr *. 100.0)
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
