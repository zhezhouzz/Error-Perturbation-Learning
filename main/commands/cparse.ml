open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

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
