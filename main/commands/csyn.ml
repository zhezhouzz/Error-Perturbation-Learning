open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

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
