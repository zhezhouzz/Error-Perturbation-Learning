open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let syn_piecewise source_file meta_file max_length bound =
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
      (fun () -> Synthesizer.Syn.synthesize_piecewise env max_length bound)
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
        Synthesizer.Syn.synthesize_multi_core env max_length bound)
  in
  (env.i_err, result)

let synthesize_piecewise =
  Command.basic ~summary:"synthesize-piecewise"
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
              syn_piecewise source_file meta_file max_length
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
