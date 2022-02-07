open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let moti_save_ source_file meta_file max_length num_sampling =
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Mkenv.random_init_prog
        @@ mk_env_from_files source_file meta_file)
  in
  let open Language.Oplang_serialization in
  let pf_graph = init_pf_graph env.op_pool in
  let pf_graph =
    reg_vertices pf_graph @@ enumerate env.tps env.op_pool max_length
  in
  let pf_graph = make_edges pf_graph in
  let count prog =
    let (_, none_num, data), cost_time =
      Zlog.event_time_
        (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "sampling")
        (fun () ->
          Sampling.Scache.eval_sampling [ env.i_err ] [ prog ]
            (Primitive.Measure.mk_measure_cond env.i_err)
            num_sampling)
    in
    let stat =
      Zlog.event_
        (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
           "evaluation") (fun () ->
          Evaluation.Ev.evaluation none_num data env.sigma
            (fun inp -> snd @@ env.client env.library_inspector inp)
            env.phi)
    in
    stat.in_sigma_out_phi_unique_num
  in
  let pf_graph = make_num pf_graph count in
  let () = Printf.printf "%s\n" @@ moti_layout_stat pf_graph in
  let () = Yojson.Basic.to_file ".moti" @@ pf_graph_save pf_graph in
  ()

let moti_load_ () =
  let open Language.Oplang_serialization in
  let pf_graph' =
    Yojson.Basic.from_file ".moti" |> pf_graph_load Parse.parse_string
  in
  let () = Printf.printf "%s\n" @@ moti_layout_stat pf_graph' in
  ()

let moti_save =
  Command.basic ~summary:"moti save"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and max_length = anon ("maximal length of the pieces" %: int)
      and num_sampling = anon ("num_sampling" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let () = moti_save_ source_file meta_file max_length num_sampling in
            ()))

let moti_load =
  Command.basic ~summary:"moti load"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file) in
      fun () -> Config.exec_main configfile moti_load_)
