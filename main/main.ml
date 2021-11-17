open Zlog
open Printf

let ppf = Format.err_formatter

open Core

let mk_standard_env () =
  let open Primitive in
  let i_err = [ Value.L [ 1; 2 ]; Value.L [ 3; 4 ] ] in
  let tps = [ Tp.IntList; Tp.IntList ] in
  let sampling_rounds = 6 in
  let prog_size = 4 in
  let libraries = [ "List" ] in
  let op_pool =
    [
      "insert";
      "replace";
      "cons";
      "append";
      "plus1";
      "minus1";
      "top";
      "bottom";
      "max";
      "min";
      "random_int";
      "const0";
      "const1";
    ]
  in
  let preds = [ "hd"; "mem" ] in
  let inspector = Language.Bblib.invocation_inspector_init libraries in
  Synthesizer.Mkenv.mk_env_v2_ Imp_const.sigma_merge Language.Bblib.merge
    inspector Imp_const.phi_merge tps i_err op_pool preds sampling_rounds
    prog_size

let mk_env_from_files source_file meta_file =
  let prog = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
  let meta = Ocaml_parser.Frontend.parse ~sourcefile:meta_file in
  let ( sigma,
        client,
        libs,
        i_err,
        phi,
        tps,
        op_pool,
        preds,
        sampling_rounds,
        p_size ) =
    Language.Of_ocamlast.load_client_and_meta prog meta
  in
  Synthesizer.Mkenv.mk_env_v2 sigma client libs phi tps i_err op_pool preds
    sampling_rounds p_size

let test_cost env progs =
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

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")

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

let parse_result =
  Command.basic ~summary:"parse-result"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let i_err, prog =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                (fun () ->
                  let i_err, prog = Parse.parse_piecewise source_file in
                  ( Primitive.Value.formal_layout_l i_err,
                    Language.Piecewise.layout prog ))
            in
            Printf.printf "let i_err = %s\n%s\n" i_err prog))

let parse_result_one =
  Command.basic ~summary:"parse-result-one"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let prog =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                (fun () -> Language.Oplang.layout @@ Parse.parse source_file)
            in
            Printf.printf "%s\n" prog))

let parse_input =
  Command.basic ~summary:"parse-input"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let env =
              Synthesizer.Mkenv.random_init_prog
              @@ mk_env_from_files source_file meta_file
            in
            let stat, result =
              Synthesizer.Env.(env.client env.library_inspector env.i_err)
            in
            let () =
              match result with
              | None -> Printf.printf "execption...\n"
              | Some vs ->
                  let in_sigma = env.sigma env.i_err in
                  let in_phi = env.phi (env.i_err @ vs) in
                  Printf.printf "[%s](in_sigma:%b) ==> [%s](in_phi:%b)\n"
                    (Primitive.Value.layout_l env.i_err)
                    in_sigma
                    (Primitive.Value.layout_l vs)
                    in_phi
            in
            let () =
              Printf.printf "stat: %s\n"
              @@ Basic_dt.List.split_by_comma string_of_int stat
            in
            (* let () = test_cost env in *)
            (* let env = Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "") ( *)
            (*     fun () -> *)
            (*       test_mcmc env) in *)
            (* let spec = Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "") *)
            (*     (fun () -> Synthesizer.Pre.pre_infer_from_env env 2) in *)
            (* let () = Printf.printf "infered precondition: %s\n" @@ Specification.Spec.layout spec in *)
            ()))

let syn source_file meta_file max_length num_burn_in num_sampling =
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
        Synthesizer.Syn.synthesize_multi env max_length num_burn_in num_sampling)
  in
  (env.i_err, result)

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
              syn source_file meta_file max_length num_burn_in num_sampling
            in
            let () =
              Printf.printf "%s\n"
              @@ Language.Piecewise.layout_with_i_err i_err result
            in
            ()))

let parse_benchmark_config source_configfile =
  let open Yojson.Basic.Util in
  let cases =
    Config.load_json source_configfile |> member "benchmarks" |> to_list
  in
  let get_setting j =
    let benchname = j |> member "name" |> to_string in
    let source_file = j |> member "source_file" |> to_string in
    let meta_file = j |> member "meta_file" |> to_string in
    let max_length = j |> member "max_length" |> to_int in
    let num_burn_in = j |> member "num_burn_in" |> to_int in
    let num_sampling = j |> member "num_sampling" |> to_int in
    let output_dir = j |> member "output_dir" |> to_string in
    ( benchname,
      source_file,
      meta_file,
      max_length,
      num_burn_in,
      num_sampling,
      output_dir )
  in
  List.map ~f:get_setting cases

let synthesize_all =
  Command.basic ~summary:"synthesize-all"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_configfile = anon ("source config file" %: regular_file)
      and num_syn = anon ("num synthesize" %: int) in
      fun () ->
        let benchmarks = parse_benchmark_config source_configfile in
        let () =
          List.iter
            ~f:
              (fun ( benchname,
                     source_file,
                     meta_file,
                     max_length,
                     num_burn_in,
                     num_sampling,
                     output_dir ) ->
              for i = 0 to num_syn - 1 do
                Config.make_dir output_dir;
                Config.exec_main configfile (fun () ->
                    let i_err, result =
                      syn source_file meta_file max_length num_burn_in
                        num_sampling
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

let layout_eval benchname stat cost_time =
  let open Evaluation.Ev in
  let avg_time =
    match stat.in_sigma_out_phi_unique_num with
    | 0 -> "inf"
    | n -> sprintf "%f" (cost_time *. 1000000.0 /. float_of_int n)
  in
  Printf.printf "%s:\ncost time:%f(s)\navg time:%s(us/instance)\n%s\n" benchname
    cost_time avg_time
  @@ Evaluation.Ev.layout stat

let eval_baseline =
  Command.basic ~summary:"eval-baseline"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_configfile = anon ("source config file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and num = anon ("sampling number" %: int) in
      fun () ->
        let qc_conf = Zquickcheck.Qc.load_config qc_file in
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
                  let data, cost_time =
                    Zlog.event_time_
                      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__
                         __FUNCTION__ "baseline sampling") (fun () ->
                        Zquickcheck.Qc_baseline.baseline qc_conf env.tps num)
                  in
                  let stat =
                    Zlog.event_
                      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__
                         __FUNCTION__ "baseline evaluation") (fun () ->
                        Evaluation.Ev.evaluation 0 data env.sigma
                          (fun inp ->
                            snd @@ env.client env.library_inspector inp)
                          env.phi)
                  in
                  let () = layout_eval benchname stat cost_time in
                  ())
                benchmarks)
        in
        ())

let eval_result =
  Command.basic ~summary:"eval-result"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_configfile = anon ("source config file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and num = anon ("sampling number" %: int) in
      fun () ->
        let qc_conf = Zquickcheck.Qc.load_config qc_file in
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
                  let () = layout_eval benchname stat cost_time in
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
            (* let () = *)
            (*   let open Basic_dt.Tree in *)
            (*   let tree = *)
            (*     Node (6, Node (3, snode 7, snode 3), Node (4, snode 8, snode 1)) *)
            (*   in *)
            (*   let () = Printf.printf "tree: %s\n" (layout string_of_int tree) in *)
            (*   let tree' = rec_flip tree in *)
            (*   let () = *)
            (*     Printf.printf "tree': %s\n" (layout string_of_int tree') *)
            (*   in *)
            (*   let () = raise @@ failwith "end" in *)
            (*   () *)
            (* in *)
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
            let () = layout_eval "" stat cost_time in
            ()))

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
            let () = test_cost env progs in
            ()))

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
            let qc_conf = Zquickcheck.Qc.load_config qc_file in
            let data, cost_time =
              Zlog.event_time_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                   "baseline sampling") (fun () ->
                  Zquickcheck.Qc_baseline.baseline qc_conf env.tps num)
            in
            let stat =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
                   "baseline evaluation") (fun () ->
                  Evaluation.Ev.evaluation 0 data env.sigma
                    (fun inp -> snd @@ env.client env.library_inspector inp)
                    env.phi)
            in
            (* let () = Printf.printf "%s\n" @@ Evaluation.Ev.layout stat in *)
            let () = layout_eval "" stat cost_time in
            ()))

let command =
  Command.group ~summary:"Error Perturbation Learning"
    [
      ("test", test);
      ("baseline", baseline);
      ("batched-test", batched_test);
      ("parse-input", parse_input);
      ("parse-result", parse_result);
      ("parse-result-one", parse_result_one);
      ("sampling", sampling);
      ("synthesize", synthesize);
      ("synthesize-all", synthesize_all);
      ("eval-baseline", eval_baseline);
      ("eval-result", eval_result);
      ("costing", costing);
    ]

let () = Command.run command
