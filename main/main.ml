open Zlog
open Printf
;;
let ppf = Format.err_formatter
;;
open Core

let mk_standard_env () =
  let open Primitive in
  let i_err = [Value.L [1;2]; Value.L [3;4]] in
  let tps = [Tp.IntList; Tp.IntList] in
  let sampling_rounds = 6 in
  let prog_size = 4 in
  let libraries = ["List"] in
  let inspector = Language.Bblib.invocation_inspector_init libraries in
  Synthesizer.Mkenv.mk_env_v2_ Imp.sigma_merge Language.Bblib.merge inspector Imp.phi_merge
    tps i_err Operator.op_pool sampling_rounds prog_size

let test_cost () =
  Synthesizer.Cost.test (mk_standard_env ())

let test_mcmc () =
  let open Synthesizer in
  let env = Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> mk_standard_env ()) in
  let env, cost = Mcmc.metropolis_hastings
      ~burn_in: 300
      ~sampling_steps: 30
      ~proposal_distribution: Mutate.mutate
      ~cost_function: Cost.cost
      ~init_distribution: env in
  let () = Printf.printf "prog(cost: %f):\n%s\n" cost (Language.Oplang.layout env.cur_p.prog) in
  ()

let test_oplang () =
  let open Synthesizer in
  let () = Language.Oplang.test () in
  ()

let test_feature () =
  let qv = [Primitive.Tp.Int, "u"; Primitive.Tp.Int, "v"] in
  let args = [Primitive.Tp.IntList, "l1"; Primitive.Tp.IntList, "l2"] in
  let mps = ["mem"; "hd"; "<"] in
  let inferctx = Classify.Cctx.mk_cctx args qv mps in
  let () = Printf.printf "fset: %s\n" (Classify.Feature.layout_set inferctx.Classify.Cctx.fset) in
  let module V = Primitive.Value in
  let () = Classify.Infer.spec_infer inferctx
      [[V.L [1;2]; V.L [3;4]]; [V.L [3;4]; V.L [1;2]]]
      (fun x -> x)
      (function
        | [V.L [1;2]; V.L [3;4]] -> true
        | _ -> false)
      (* (fun v -> not (Primitive.Imp.phi_merge v)) *)
      (* Primitive.Imp.sigma_merge *)
  in
  ()

let test_pre_infer () =
  let prog = Parse.parse "data/pre.prog" in
  let env = mk_standard_env () in
  let open Synthesizer in
  let scache = Sampling.cost_sampling_ env.tps [env.i_err] prog env.sampling_rounds in
  let qv = [Primitive.Tp.Int, "u"; Primitive.Tp.Int, "v"] in
  let args = prog.fin in
  let mps = ["mem"; "hd"; "<"] in
  let cctx = Classify.Cctx.mk_cctx [Primitive.Tp.IntList, "x1"; Primitive.Tp.IntList, "x2"] qv mps in
  let () = Printf.printf "fset: %s\n" (Classify.Feature.layout_set cctx.Classify.Cctx.fset) in
  let () = Pre.perturbation_pre_infer cctx scache
      env.Synthesizer.Env.sigma
      (fun v -> snd @@ env.client env.library_inspector v)
      env.Synthesizer.Env.phi
  in
  ()

let batched_test num_times num_burn_in num_sampling =
  let open Synthesizer in
  let rec aux n =
    if n >= num_times then () else
      let env = Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
          (fun () -> mk_standard_env ()) in
      let env, cost =
        Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "") (
          fun () ->
            Mcmc.metropolis_hastings
              ~burn_in: num_burn_in
              ~sampling_steps: num_sampling
              ~proposal_distribution: Mutate.mutate
              ~cost_function: Cost.cost
              ~init_distribution: (mk_standard_env ())
        )
      in
      let () = Zlog.log_write @@ Printf.sprintf "prog(cost: %f):\n%s\n" cost (Language.Oplang.layout env.cur_p.prog) in
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

let test = Command.basic
    ~summary:"test."
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and test_name = anon ("test name" %: string)
      in
      fun () -> Config.exec_main configfile (fun () ->
          (* event "test" (fun () -> Printf.printf "test!\n"; Language.Arg_solving.test ()) *)
          match test_name with
          | "cost" -> Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                        (fun () -> Printf.printf "test!\n"; test_cost ())
          | "mcmc" -> Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                        (fun () -> Printf.printf "test!\n"; test_mcmc ())
          | "oplang" -> Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                          (fun () -> Printf.printf "test!\n"; test_oplang ())
          | "feature" -> Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                           (fun () -> Printf.printf "test!\n"; test_feature ())
          | "pre" -> Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                       (fun () -> Printf.printf "test!\n"; test_pre_infer ())
          | _ -> raise @@ failwith "unknown test name"
        )
    )

let batched_test = Command.basic
    ~summary:"batched test."
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and num_times = anon ("num times" %: int)
      and num_burn_in = anon ("num burn-in" %: int)
      and num_sampling = anon ("num sampling" %: int)
      in
      fun () -> Config.exec_main configfile (fun () ->
          batched_test num_times num_burn_in num_sampling
        )
    )

let parse =  Command.basic
    ~summary:"parse"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      in
      fun () -> Config.exec_main configfile (fun () ->
          let prog = Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () -> Language.Oplang.layout @@ Parse.parse source_file) in
          Printf.printf "%s\n" prog
        )
    )

let ocaml_parse =  Command.basic
    ~summary:"ocaml-parse"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      in
      fun () -> Config.exec_main configfile (fun () ->
          let prog = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
          let meta = Ocaml_parser.Frontend.parse ~sourcefile:meta_file in
          let open Basic_dt in
          let preds, sigma, client, libs, phi, tps, op_pool, sampling_rounds, p_size =
            Language.Clientlang_of_ocamlast.of_ocamlast prog meta in
          let () = Printf.printf "preds:%s\nsigma:%s\nclient:%s\nlibs:%s\nphi:%s\ntps:%s\nop_pool:%s\nsampling_rounds:%i\np_size:%i\n"
              (List.split_by_comma (fun x -> x) preds)
              (Specification.Spec.layout sigma)
              (Language.Clientlang.layout client)
              (List.split_by_comma (fun x -> x) libs)
              (Specification.Spec.layout phi)
              (List.split_by_comma Primitive.Tp.layout tps)
              (List.split_by_comma (fun x -> x) op_pool)
              sampling_rounds p_size
          in
          let env = Synthesizer.Mkenv.mk_env_v2 sigma client libs phi tps
                    [Primitive.Value.L [1;2]; Primitive.Value.L [3;4]] op_pool sampling_rounds p_size in
          let stat, result = Synthesizer.Env.(env.client env.library_inspector env.i_err) in
          let () = match result with
            | None -> Printf.printf "execption...\n"
            | Some vs -> Printf.printf "result = [%s]\n" @@ Primitive.Value.layout_l vs in
          let () = Printf.printf "stat: %s\n" @@ List.split_by_comma string_of_int stat in
          ()
        )
    )

let command =
  Command.group ~summary:"Error Perturbation Learning"
    [ "test", test;
      "batched-test", batched_test;
      "parse", parse;
      "ocaml-parse", ocaml_parse;
    ]

let () = Command.run command
;;
