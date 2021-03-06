open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let cind_num_sampling = 90

let one_pass env selected_op_pool bound =
  let () =
    Zlog.log_write @@ Printf.sprintf "pool: %s"
    @@ List.to_string ~f:(fun x -> x) selected_op_pool
  in
  (* set pool & remove size bound *)
  (* let masure v = Primitive.Measure.measure_size v <= 1000 in *)
  let masure _ = true in
  let env =
    Synthesizer.Env.
      { env with op_pool = selected_op_pool; measure_cond = masure }
  in
  let _, prog =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Syn.synthesize_multi_core env (fun _ -> true) 1 bound)
  in
  let _, none_num, data =
    Sampling.Scache.eval_sampling [ env.i_err ] [ prog ] masure
      cind_num_sampling
  in
  let stat =
    Evaluation.Ev.evaluation none_num data env.sigma
      (fun inp -> snd @@ env.client env.library_inspector inp)
      env.phi
  in
  let acc =
    float Evaluation.Ev.(stat.in_sigma_out_phi_unique_num)
    /. float cind_num_sampling
  in
  let () =
    Zlog.log_write
    @@ Printf.sprintf "%s\nacc:%f\n" (Evaluation.Ev.layout_eval "" stat 0.0) acc
  in
  acc

let random_select_helper _ (s, e) =
  let open Primitive.Operator in
  (* let pool = get_pool_by_name name in *)
  let pool = ind_op_pool in
  let () =
    if e > List.length pool then
      raise
      @@ failwith
           (Printf.sprintf
              "error: the end value (%i) is greater than the number of all \
               operators (%i)"
              e (List.length pool))
    else ()
  in
  pool

let random_v1 (env, pool, (s, e), num_op_pools_per_bound, bound) =
  let open Primitive.Operator in
  let pool_arr = Array.of_list pool in
  let acc_arr = Array.init (Array.length pool_arr + 1) ~f:(fun _ -> 0.0) in
  for i = s to e do
    let pools =
      Primitive.Randomgen.choose_n_from_arr pool_arr i num_op_pools_per_bound
    in
    let pools = List.map pools ~f:(fun x -> x @ basic_op_pool) in
    let accs = List.map pools ~f:(fun pool -> one_pass env pool bound) in
    let acc_mean =
      List.fold accs ~init:0.0 ~f:(fun sum x -> sum +. x)
      /. float num_op_pools_per_bound
    in
    acc_arr.(i) <- acc_mean
  done;
  List.sub (Array.to_list acc_arr) ~pos:s ~len:(e - s + 1)

let arr_mean arr =
  Array.fold arr ~init:0.0 ~f:(fun sum x -> sum +. x)
  /. float (Array.length arr)

let list_mean arr =
  List.fold arr ~init:0.0 ~f:(fun sum x -> sum +. x) /. float (List.length arr)

let random_v2 env pool (s, e) num_op_pools_per_bound bound =
  let open Primitive.Operator in
  let acc_mat =
    Array.make_matrix
      ~dimx:(List.length pool + 1)
      ~dimy:num_op_pools_per_bound 0.0
  in
  for n = 0 to num_op_pools_per_bound - 1 do
    (* let () = Printf.printf "n:%i\n" n in *)
    let pool = QCheck.Gen.generate1 (QCheck.Gen.shuffle_l pool) in
    for i = s to e do
      (* let () = Printf.printf "\ti:%i\n" i in *)
      let pool = basic_op_pool @ List.sub ~pos:0 ~len:i pool in
      (* if fails (mostly cannot find initial perturbation function), return 0.0       *)
      let acc =
        try one_pass env pool bound
        with
        | Synthesizer.Mkenv.InitializationError
        | Synthesizer.Syn.SynthesisHasNoGoodResult
        ->
          0.0
      in
      acc_mat.(i).(n) <- acc
    done
  done;
  acc_mat

let num_run_from_init_set = 3

let inner_repeat_num = 3

let indudctive_run env init_op_set rest_op_set e bound =
  let rs =
    QCheck.Gen.generate ~n:inner_repeat_num (QCheck.Gen.shuffle_l rest_op_set)
  in
  let cal_acc_arr rest_op_set =
    Array.init e ~f:(fun k ->
        let len = k in
        (* let () = *)
        (*   Printf.printf "e:%i len: %i; len(rest_op_set):%i\n" e len *)
        (*   @@ List.length rest_op_set *)
        (* in *)
        (* let () = raise @@ failwith "zz" in *)
        let pool =
          Primitive.Operator.basic_op_pool @ init_op_set
          @ List.sub rest_op_set ~pos:0 ~len
        in
        let accs =
          List.init num_run_from_init_set ~f:(fun _ ->
              let ct = ref 0 in
              let rec get_acc () =
                try one_pass env pool bound with
                | Synthesizer.Syn.SynthesisHasNoGoodResult -> 0.0
                | Synthesizer.Mkenv.InitializationError ->
                    if !ct < len then (
                      ct := !ct + 1;
                      get_acc ())
                    else 0.0
              in

              get_acc ())
        in
        list_mean accs)
  in
  List.map ~f:cal_acc_arr rs

let outer_repeat_num = 3

let dump_data data =
  let open Yojson.Basic.Util in
  let aux (init_op_set, rest_op_set, acc_mat) =
    let opset_encode s = `List (List.map ~f:(fun x -> `String x) s) in
    let mat_decode mat =
      `List
        (List.map
           ~f:(fun a ->
             `List (List.map ~f:(fun x -> `Float x) @@ Array.to_list a))
           mat)
    in
    `Assoc
      [
        ("init_op_set", opset_encode init_op_set);
        ("rest_op_set", opset_encode rest_op_set);
        ("acc_mat", mat_decode acc_mat);
      ]
  in
  `List (List.map ~f:aux data)

let dump_multi_init_data data =
  let open Yojson.Basic.Util in
  let aux mat =
    `List
      (List.map
         ~f:(fun (n, accs) ->
           `Assoc
             [
               ("n_init", `Int n);
               ("acc_mat", `List (List.map ~f:(fun x -> `Float x) accs));
             ])
         mat)
  in
  `List (List.map ~f:aux data)

let random_v3 env pool (s, e) bound =
  let num_init_op_set = s in
  let open Primitive.Operator in
  let ps =
    List.map
      (QCheck.Gen.generate ~n:outer_repeat_num (QCheck.Gen.shuffle_l pool))
      ~f:(fun l ->
        let len = List.length l in
        let l1 = List.sub ~pos:0 ~len:num_init_op_set l in
        let l2 = List.sub ~pos:num_init_op_set ~len:(len - num_init_op_set) l in
        (l1, l2))
  in
  let res =
    List.map
      ~f:(fun (init_op_set, rest_op_set) ->
        ( init_op_set,
          rest_op_set,
          indudctive_run env init_op_set rest_op_set (e - s + 1) bound ))
      ps
  in
  let json = dump_data res in
  (* let () = *)
  (*   List.iter *)
  (*     ~f:(fun (a, b, res) -> *)
  (*       Printf.printf "init: %s; rest: %s\n" *)
  (*         (List.to_string ~f:(fun x -> x) a) *)
  (*         (List.to_string ~f:(fun x -> x) b); *)
  (*       List.iter *)
  (*         ~f:(fun arr -> *)
  (*           Printf.printf "\t[%s]\n" *)
  (*           @@ List.to_string ~f:string_of_float *)
  (*           @@ Array.to_list arr) *)
  (*         res) *)
  (*     res *)
  (* in *)
  (json, List.concat @@ List.map ~f:(fun (_, _, x) -> x) res)

let random_select env name (s, e) bound =
  (* random_v1 @@ random_select_helper env name (s, e) num_op_pools_per_bound bound *)
  let pool = random_select_helper name (s, e) in
  random_v3 env pool (s, e) bound

let robu_random_inits env qc_conf num bound =
  let open Synthesizer in
  let open Env in
  let cond inp =
    match snd @@ env.client env.library_inspector inp with
    | None -> false
    | Some outp -> env.sigma inp && not (env.phi (inp @ outp))
  in
  let alphas =
    Zquickcheck.Qc_baseline.gen_erroneous_inputs qc_conf env.Env.tps cond num
  in
  List.map
    ~f:(fun alpha ->
      let env = Mkenv.update_i_err env alpha in
      one_pass env Primitive.Operator.(ind_op_pool @ basic_op_pool) bound)
    alphas

let parse freq = List.map ~f:int_of_string @@ String.split freq ~on:','

let multi_init_repeat_num = 20

let robu_multi_inits env qc_conf num ns bound =
  let open Synthesizer in
  let open Env in
  let cond inp =
    match snd @@ env.client env.library_inspector inp with
    | None -> false
    | Some outp -> env.sigma inp && not (env.phi (inp @ outp))
  in
  let res =
    List.init
      ~f:(fun _ ->
        match List.rev ns with
        | [] ->
            raise
            @@ failwith
                 "the required numbers of initial erroenous inputs are empty"
        | n :: _ ->
            let alphas =
              Zquickcheck.Qc_baseline.gen_erroneous_inputs qc_conf env.Env.tps
                cond n
            in
            let alpha =
              match alphas with
              | [] ->
                  raise
                  @@ failwith
                       "the required number of initial erroenous input is zero"
              | h :: _ -> h
            in
            let env = Mkenv.update_i_err env alpha in
            List.map
              ~f:(fun n ->
                let env =
                  Mkenv.update_init_sampling_set env
                  @@ Basic_dt.List.sublist alphas (0, n)
                in
                ( n,
                  List.init multi_init_repeat_num ~f:(fun _ ->
                      one_pass env
                        Primitive.Operator.(ind_op_pool @ basic_op_pool)
                        bound) ))
              ns)
      num
  in
  let json = dump_multi_init_data res in
  (json, res)

let ind =
  Command.basic ~summary:"inductively increasing the number of operators we use"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and output_json_file = anon ("output json file" %: regular_file)
      and name = anon ("name of datatype" %: string)
      and s = anon ("number of initial operators" %: int)
      and e = anon ("number of final operators" %: int)
      and time_in_second = anon ("step bound" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            Zlog.event_
              (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () ->
                let env = mk_env_from_files source_file meta_file in
                let ind_len = e - s + 1 in
                let bound =
                  (* Synthesizer.Syn.(TimeBound (float time_in_second)) *)
                  Synthesizer.Syn.IterBound (0, time_in_second)
                in
                let json, acc_mat = random_select env name (s, e) bound in
                let () = Yojson.Basic.to_file output_json_file json in
                let accs = Array.init ~f:(fun _ -> 0.0) ind_len in
                let arr_add_to a b =
                  Array.iteri ~f:(fun idx v -> b.(idx) <- b.(idx) +. v) a
                in
                let () =
                  List.iter ~f:(fun arr -> arr_add_to arr accs) acc_mat
                in
                let accs =
                  Array.to_list
                  @@ Array.map
                       ~f:(fun x ->
                         x /. float (outer_repeat_num * inner_repeat_num))
                       accs
                in
                (* let print_mat mat = *)
                (*   Array.iter mat ~f:(fun arr -> *)
                (*       Printf.printf "%s\n" *)
                (*       @@ List.to_string ~f:string_of_float *)
                (*       @@ Array.to_list arr) *)
                (* in *)
                Printf.printf "client: %s\nname: %s\nacc: %i -> %i\n%s\n"
                  source_file name s e
                  (List.to_string accs ~f:string_of_float))))

let robu_init =
  Command.basic ~summary:"increasing the number of initial erroneous inputs"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and output_json_file = anon ("output json file" %: regular_file)
      and freq =
        anon
          ("numbers of size of initial erroenous input (split by comma)"
         %: string)
      and num = anon ("number of test time" %: int)
      and step_bound = anon ("step bound" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            Zlog.event_
              (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () ->
                let qc_conf = Qc_config.load_config qc_file in
                let env = mk_env_from_files source_file meta_file in
                let bound =
                  (* Synthesizer.Syn.(TimeBound (float time_in_second)) *)
                  Synthesizer.Syn.IterBound (0, step_bound)
                in
                let ns = parse freq in
                let json, acc_mat = robu_multi_inits env qc_conf num ns bound in
                let () = Yojson.Basic.to_file output_json_file json in
                let record = Array.init ~f:(fun _ -> 0.0) (List.length ns) in
                let () =
                  List.iter
                    ~f:(fun accs ->
                      List.iteri
                        ~f:(fun idx (_, accs) ->
                          record.(idx) <-
                            record.(idx)
                            +. List.fold_left
                                 ~f:(fun r x -> x +. r)
                                 ~init:0.0 accs
                               /. (float_of_int @@ List.length accs))
                        accs)
                    acc_mat
                in
                let record =
                  Array.to_list
                  @@ Array.map
                       ~f:(fun x -> x /. float (List.length acc_mat))
                       record
                in
                (* let print_mat mat = *)
                (*   Array.iter mat ~f:(fun arr -> *)
                (*       Printf.printf "%s\n" *)
                (*       @@ List.to_string ~f:string_of_float *)
                (*       @@ Array.to_list arr) *)
                (* in *)
                Printf.printf "client: %s\nacc: [%s]\n%s\n" source_file freq
                  (List.to_string record ~f:string_of_float))))
