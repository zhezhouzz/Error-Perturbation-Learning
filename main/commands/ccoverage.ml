open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux
module V = Primitive.Value

let e_0 inputs =
  let rec inc i = function
    | [] -> true
    | h :: t -> Int.equal h i && inc (i + 1) t
  in
  match inputs with
  | [ V.L s1; V.L s2 ] ->
      List.equal ( = ) [ 3; 4 ] s2 && List.length s1 >= 2 && inc 1 s1
  | _ -> false

let gen_from_target_data source_file meta_file data_file prog_file qc_conf =
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> mk_env_from_files source_file meta_file)
  in
  let _, prog = Parse.parse_piecewise prog_file in
  let epre =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> Synthesizer.Syn.synthesize_erroneous_pre env qc_conf prog)
  in
  let ectx = Synthesizer.Enum.load data_file in
  let total = Synthesizer.Enum.num_inps ectx in
  let in_pre = Synthesizer.Enum.count_in_pre ectx epre in
  let () =
    Printf.printf "pre: %i/%i = %.2f\n" in_pre total
      (float_of_int in_pre /. float_of_int total *. 100.0)
  in
  ()

let gen_from_target_perturbation source_file meta_file target_prog_file
    prog_file qc_conf test_num =
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Mkenv.random_init_prog
        @@ mk_env_from_files source_file meta_file)
  in
  let _, prog = Parse.parse_piecewise prog_file in
  let epre =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> Synthesizer.Syn.synthesize_erroneous_pre env qc_conf prog)
  in
  let i_err, target_prog = Parse.parse_piecewise target_prog_file in
  let env = Synthesizer.Mkenv.update_i_err env i_err in
  let conds =
    Sampling.Scache.mk_conds
      (fun _ -> true)
      env.sigma
      (fun inp -> snd @@ env.client env.library_inspector inp)
      env.phi
      (fun _ -> true)
  in
  let pool, num_none, data =
    Sampling.Scache.target_sampling conds [ i_err ] target_prog test_num
  in
  let n = List.length @@ List.filter ~f:epre data in
  let () = Printf.printf "pre: %i/%i\n" n (List.length data) in
  ()

let multi_dimension_syn source_file meta_file qc_conf ds bound =
  let () =
    if ds <= 0 then raise @@ failwith "wrong argument dimension number" else ()
  in
  let bias _ = true in
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> mk_env_from_files source_file meta_file)
  in
  let prorgs_to_piecewise = function
    | [] -> raise @@ failwith "die"
    | h :: t ->
        (List.map ~f:(fun f -> (Specification.Spec.dummy_pre env.tps, f)) t, h)
  in
  let make_new old_pres old_prog =
    let env = Synthesizer.Mkenv.random_init_prog env in
    let _, prog =
      Zlog.event_
        (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
        (fun () ->
          (* Synthesizer.Syn.synthesize_piecewise env max_length num_burn_in *)
          (*   num_sampling *)
          Synthesizer.Syn.synthesize_multi_core env bias 1 bound)
    in
    let new_prog = prog :: old_prog in
    let pre =
      Synthesizer.Syn.synthesize_pre_multi env qc_conf
        (prorgs_to_piecewise new_prog)
    in
    (pre :: old_pres, new_prog)
  in
  let rec loop i (pres, ps) =
    let pres, ps = make_new pres ps in
    if i >= ds then (pres, (env.i_err, prorgs_to_piecewise ps))
    else loop (i + 1) (pres, ps)
  in
  loop 1 ([], [])

let coverage_against_data =
  Command.basic ~summary:"coverage-against-data"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and data_file = anon ("data file" %: regular_file)
      and prog_file = anon ("prog file" %: regular_file)
      and qc_file = anon ("qc file" %: regular_file) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let qc_conf = Qc_config.load_config qc_file in
            gen_from_target_data source_file meta_file data_file prog_file
              qc_conf))

let coverage =
  Command.basic ~summary:"coverage"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and target_prog_file = anon ("target prog file" %: regular_file)
      and prog_file = anon ("prog file" %: regular_file)
      and qc_file = anon ("qc file" %: regular_file)
      and test_num = anon ("test_num" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let qc_conf = Qc_config.load_config qc_file in
            gen_from_target_perturbation source_file meta_file target_prog_file
              prog_file qc_conf test_num))

let coverage_syn =
  Command.basic ~summary:"coverage-syn"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and qc_file = anon ("qc file" %: regular_file)
      and ds = anon ("number of sub perturbation functions" %: int)
      and num_sampling = anon ("num sampling" %: int)
      and output_file_name = anon ("output file name" %: string) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let qc_conf = Qc_config.load_config qc_file in
            let bound = Synthesizer.Syn.IterBound (0, num_sampling) in
            let pres, (i_err, prog) =
              multi_dimension_syn source_file meta_file qc_conf ds bound
            in
            let fname =
              Printf.sprintf ".result/%s_coverage_syn.prog" output_file_name
            in
            let prename =
              Printf.sprintf ".result/%s_coverage_syn.pre" output_file_name
            in
            let () =
              Core.Out_channel.write_all fname
                ~data:(Language.Piecewise.layout_with_i_err i_err prog)
            in
            let () =
              Core.Out_channel.write_all prename
                ~data:
                  (Basic_dt.List.split_by "\n" Specification.Spec.layout pres)
            in
            ()))

let coverage_all_save =
  Command.basic ~summary:"coverage-all-save"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and arg_assign_bound = anon ("arg assign bound" %: int)
      and num_sampling = anon ("num sampling" %: int)
      and database_name = anon ("database name" %: string) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let env =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                (fun () -> mk_env_from_files source_file meta_file)
            in
            Zlog.event_
              (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () ->
                let open Synthesizer in
                let client =
                  Enum.make_client env.sigma (Mkenv.to_c env) env.phi
                in
                let ectx =
                  Enum.init
                    ~enum_max_argassigns:
                      (if arg_assign_bound <= 0 then None
                      else Some arg_assign_bound)
                    ~iter_bound:num_sampling env.p_size env.op_pool env.tps
                    env.i_err
                in
                let () = Enum.run (Enum.explore_state client) ectx in
                let () =
                  Zlog.event_
                    (Printf.sprintf "save time %s:%i[%s]-%s" __FILE__ __LINE__
                       __FUNCTION__ "") (fun () -> Enum.save ectx database_name)
                in
                ())))
