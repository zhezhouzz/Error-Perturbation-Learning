open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let naive_mcmc_path = ".result/.motipf/"

let naive_mcmc source_file meta_file num_test bound =
  (* set naive cost function *)
  let () =
    Config.conf :=
      { !Config.conf with Config.cost_function_version = Config.VCountErrors }
  in
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Mkenv.random_init_prog
        @@ mk_env_from_files source_file meta_file)
  in
  let stop_steps =
    List.init num_test ~f:(fun i ->
        Zlog.event_
          (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
          (fun () ->
            let res, step = Synthesizer.Syn.synthesize_f_moti bound env in
            match res with
            | None -> raise @@ failwith "no result"
            | Some (_, prog) ->
                let path = sprintf "%s/pf%i.prog" naive_mcmc_path i in
                let () =
                  Core.Out_channel.write_all path
                    ~data:
                      (Language.Piecewise.layout_with_i_err env.i_err ([], prog))
                in
                step))
  in
  let num_over_bound, fc =
    List.fold_left
      ~f:(fun (n, l) x ->
        match x with None -> (n + 1, l) | Some x -> (n, x :: l))
      ~init:(0, []) stop_steps
  in
  (num_over_bound, fc)

let count env num_sampling prog =
  let open Synthesizer.Env in
  let _, none_num, data =
    Sampling.Scache.eval_sampling [ env.i_err ] [ prog ]
      (fun _ -> true)
      num_sampling
  in
  let stat =
    Evaluation.Ev.evaluation none_num data env.sigma
      (fun inp -> snd @@ env.client env.library_inspector inp)
      env.phi
  in
  stat.in_sigma_out_phi_unique_num

let moti_save_ source_file meta_file max_length num_sampling =
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Mkenv.random_init_prog
        @@ mk_env_from_files source_file meta_file)
  in
  let open Language.Oplang_serialization in
  let pf_graph = init_pf_graph env.op_pool max_length in
  let pf_graph =
    reg_vertices pf_graph @@ enumerate env.tps env.op_pool pf_graph.max_length
  in
  let pf_graph = make_edges pf_graph in
  let pf_graph = make_num pf_graph (count env num_sampling) in
  let () = Printf.printf "%s\n" @@ moti_layout_stat pf_graph in
  let () = Yojson.Basic.to_file ".moti" @@ pf_graph_save pf_graph in
  ()

let moti_load_ source_file meta_file num_sampling =
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Mkenv.random_init_prog
        @@ mk_env_from_files source_file meta_file)
  in
  let open Language.Oplang_serialization in
  let pf_graph =
    Yojson.Basic.from_file ".moti" |> pf_graph_load Parse.parse_string
  in
  (* let pf_graph = make_num pf_graph (count env num_sampling) in *)
  let () = Printf.printf "%s\n" @@ moti_layout_stat pf_graph in
  ()

let moti =
  Command.basic ~summary:"moti save/load"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and max_length = anon ("maximal length of the pieces" %: int)
      and num_sampling = anon ("num_sampling" %: int)
      and sl = anon ("save/load" %: string) in
      fun () ->
        Config.exec_main configfile (fun () ->
            Zlog.event_
              (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () ->
                match sl with
                | "save" ->
                    moti_save_ source_file meta_file max_length num_sampling
                | "load" -> moti_load_ source_file meta_file num_sampling
                | _ -> raise @@ failwith "[save | load]")))

let search_ algo start =
  let open Language.Oplang_serialization in
  let open Yojson.Basic in
  let pf_graph = from_file ".moti" |> pf_graph_load Parse.parse_string in
  let algo =
    match algo with
    | "bfs" -> bfs
    | "dfs" -> dfs
    | _ -> raise @@ failwith "unknown searching algo"
  in
  let start =
    match start with
    | "init" -> init_node pf_graph
    | "max" -> max_node pf_graph
    | _ -> raise @@ failwith "bad start"
  in
  let res, blocks = reorder pf_graph algo start in
  let () =
    Yojson.Basic.to_file ".search"
      (`Assoc
        [ ("res", kvlist_save i_save f_save res); ("blocks", il_save blocks) ])
  in
  ()

let search =
  Command.basic ~summary:"moti serch"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and algo = anon ("algo: dfs, bfs" %: string)
      and start = anon ("start: init, max" %: string) in
      fun () ->
        Config.exec_main configfile (fun () ->
            Zlog.event_
              (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () -> search_ algo start)))

let moti_robu =
  Command.basic ~summary:"moti robustness"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and num_test = anon ("number of test" %: int)
      and num_sampling = anon ("number sampling" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            Zlog.event_
              (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () ->
                let n, l =
                  naive_mcmc source_file meta_file num_test
                    (Synthesizer.Syn.IterBound (0, num_sampling))
                in
                Printf.printf "%i\n%s\n" n
                  (Basic_dt.List.split_by_comma string_of_int l))))

let evaluate_result ct env ectx (idx, prog) qc_conf =
  let epre =
    Zlog.event_ "evaluate_result::epre" (fun () ->
        Synthesizer.Syn.synthesize_erroneous_pre_moti env qc_conf prog)
  in
  match epre with
  | None -> 0
  | Some epre ->
      let in_pre =
        Zlog.event_ "evaluate_result::in_pre" (fun () ->
            Synthesizer.Enum.count_in_pre ct ectx (epre, idx))
      in
      in_pre

let evaluate_result_v2 env inp_m ct (run_idx, num_step, epre) =
  match epre with
  | None -> ()
  | Some epre ->
      Primitive.Inpmap.count_tab_add_pre inp_m ct (run_idx, num_step, epre)

let dump_record (total, union, res) filename =
  let open Yojson.Basic in
  let j =
    `Assoc
      [
        ("total", `Int total);
        ( "union",
          `List
            (List.map
               ~f:(fun (a, b) ->
                 `Assoc [ ("u_i", `Int a); ("u_in_pre", `Int b) ])
               union) );
        ( "runs",
          `List
            (List.map
               ~f:(fun res ->
                 `List
                   (List.map
                      ~f:(fun (i, idx, cost, in_pre) ->
                        `Assoc
                          [
                            ("i", `Int i);
                            ("idx", `Int i);
                            ("cost", `Float cost);
                            ("in_pre", `Int in_pre);
                          ])
                      res))
               res) );
      ]
  in
  to_file filename j

let naive_mcmc_record source_file meta_file pos_data_file data_file interval
    bound num_test ct_file_name =
  (* set naive cost function *)
  let ectx = Synthesizer.Enum.load data_file in
  let inpm =
    Primitive.Inpmap.t_of_sexp @@ Sexplib.Sexp.load_sexp pos_data_file
  in
  let pos_data = Primitive.Inpmap.get_inps inpm 1000 in
  let () =
    Config.conf :=
      { !Config.conf with Config.cost_function_version = Config.VCountErrors }
  in
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> mk_env_from_files source_file meta_file)
  in
  let total = Synthesizer.Enum.num_inps ectx in
  let ct = Primitive.Inpmap.mk_count_tab ectx.m in
  let range = ref None in
  let res =
    List.init num_test ~f:(fun run_idx ->
        Zlog.event_
          (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
          (fun () ->
            let rcd =
              Synthesizer.Syn.synthesize_f_moti_record interval bound env
            in
            let () =
              match !range with
              | None -> range := Some (List.map ~f:(fun (i, _) -> i) rcd)
              | Some _ -> ()
            in
            let pre_tab = Hashtbl.create ~size:50 (module Int) in
            let () =
              List.iter
                ~f:(fun (i, (actual_idx, prog, _)) ->
                  if Hashtbl.mem pre_tab actual_idx then ()
                  else
                    let epre =
                      Zlog.event_ "evaluate_result::epre" (fun () ->
                          Synthesizer.Syn.synthesize_erroneous_pre_moti_pos env
                            pos_data ([], prog))
                    in
                    let x = Hashtbl.add pre_tab ~key:actual_idx ~data:epre in
                    ())
                rcd
            in
            let () =
              List.iter
                ~f:(fun (num_step, (actual_idx, _, _)) ->
                  match Hashtbl.find pre_tab actual_idx with
                  | None -> raise @@ failwith "never happen"
                  | Some epre ->
                      evaluate_result_v2 env ectx.m ct (run_idx, num_step, epre))
                rcd
            in
            ct))
  in
  let () =
    Sexplib.Sexp.save ct_file_name
      (Primitive.Inpmap.sexp_of_count_tab (total, ct))
  in
  ()

let count_result_to_json (total, num_runs, num_unions, idxs, tab) =
  let l =
    List.map ~f:(fun ((run_idx, union_idx, num_step), n) ->
        `Assoc
          [
            ("run_idx", `Int run_idx);
            ("num_unoin", `Int (union_idx + 1));
            ("num_step", `Int num_step);
            ("in_pre", `Int n);
          ])
    @@ Primitive.Inpmap.res_to_list tab
  in
  `Assoc
    [
      ("total", `Int total);
      ("num_runs", `Int num_runs);
      ("num_unions", `Int num_unions);
      ("idxs", `List (List.map ~f:(fun x -> `Int x) idxs));
      ("data", `List l);
    ]

let moti_analysis ct_file num_runs num_union interval bound out_file_name =
  let rec mk_idx l i =
    if i > bound then l
    else if
      Int.equal (i mod interval) 0 || (i < interval && Int.equal (i mod 2) 0)
    then mk_idx (l @ [ i ]) (i + 1)
    else mk_idx l (i + 1)
  in
  let idxs = mk_idx [] 0 in
  let total, ct =
    Primitive.Inpmap.count_tab_of_sexp @@ Sexplib.Sexp.load_sexp ct_file
  in
  let res = Primitive.Inpmap.count_tab_analysis ct num_runs num_union idxs in
  Yojson.Basic.to_file out_file_name
  @@ count_result_to_json (total, num_runs, num_union, idxs, res)

let moti_coverage =
  Command.basic ~summary:"moti coverage"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and pos_data_file = anon ("pos data file" %: regular_file)
      and neg_data_file = anon ("neg data file" %: regular_file)
      and interval = anon ("interval" %: int)
      and num_sampling = anon ("number sampling" %: int)
      and num_test = anon ("number of test" %: int)
      and ct_file_name = anon ("output ct file name" %: string) in
      fun () ->
        Config.exec_main configfile (fun () ->
            Zlog.event_
              (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () ->
                let () =
                  naive_mcmc_record source_file meta_file pos_data_file
                    neg_data_file interval
                    (Synthesizer.Syn.IterBound (0, num_sampling))
                    num_test ct_file_name
                in
                ())))

let moti_coverage_analysis =
  Command.basic ~summary:"moti coverage analysis"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and ct_file = anon ("count table file" %: regular_file)
      and interval = anon ("interval" %: int)
      and num_sampling = anon ("number sampling" %: int)
      and num_test = anon ("number of test" %: int)
      and num_union = anon ("num union" %: int)
      and output_file = anon ("output file" %: string) in
      fun () ->
        Config.exec_main configfile (fun () ->
            Zlog.event_
              (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () ->
                let () =
                  moti_analysis ct_file num_test num_union interval num_sampling
                    output_file
                in
                ())))
