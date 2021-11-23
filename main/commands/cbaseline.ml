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
