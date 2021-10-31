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
  Synthesizer.Mkenv.mk_env_v2_ Imp.sigma_merge Language.Bblib.merge inspector
    Imp.phi_merge tps i_err op_pool preds sampling_rounds prog_size

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

let test_cost env = Synthesizer.Cost.test env

let test_mcmc env =
  let open Synthesizer in
  let env, cost =
    Mcmc.metropolis_hastings ~burn_in:300 ~sampling_steps:30
      ~proposal_distribution:Mutate.mutate ~cost_function:Cost.cost
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
    Sampling.cost_sampling_ env.Env.tps [ env.Env.i_err ] prog
      env.sampling_rounds
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
              ~cost_function:Cost.cost ~init_distribution:env)
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
            | "cost" ->
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
                    test_cost env)
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
            let prog =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                (fun () ->
                  Language.Piecewise.layout @@ Parse.parse_piecewise source_file)
            in
            Printf.printf "%s\n" prog))

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
                  Synthesizer.Syn.synthesize_piecewise env max_length
                    num_burn_in num_sampling)
            in
            let () =
              Printf.printf "result:\n%s\n" @@ Language.Piecewise.layout result
            in
            ()))

let sampling =
  Command.basic ~summary:"sampling"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and prog_file = anon ("prog file" %: regular_file) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let env = mk_env_from_files source_file meta_file in
            let prog = Parse.parse prog_file in
            let scache =
              Synthesizer.Sampling.biased_cost_sampling
                (fun _ -> true)
                env.tps [ env.i_err ] prog env.sampling_rounds
            in
            let () =
              Printf.printf "%s\n" @@ Synthesizer.Sampling.cache_layout scache
            in
            ()))

let command =
  Command.group ~summary:"Error Perturbation Learning"
    [
      ("test", test);
      ("batched-test", batched_test);
      ("parse-input", parse_input);
      ("parse-result", parse_result);
      ("parse-result-one", parse_result_one);
      ("sampling", sampling);
      ("synthesize", synthesize);
    ]

let () = Command.run command
