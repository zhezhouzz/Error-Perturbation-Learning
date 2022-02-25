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
  let masure v = Primitive.Measure.measure_size v <= 1000 in
  let env =
    Synthesizer.Env.
      { env with op_pool = selected_op_pool; measure_cond = masure }
  in
  let _, prog =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> Synthesizer.Syn.synthesize_multi_core env 1 bound)
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

let random_select_helper name (s, e) =
  let open Primitive.Operator in
  let pool = get_pool_by_name name in
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
    let () = Printf.printf "n:%i\n" n in
    let pool = QCheck.Gen.generate1 (QCheck.Gen.shuffle_l pool) in
    for i = s to e do
      let () = Printf.printf "\ti:%i\n" i in
      let pool = basic_op_pool @ List.sub ~pos:0 ~len:i pool in
      (* if fails (mostly cannot find initial perturbation function), return 0.0       *)
      let acc =
        try one_pass env pool bound
        with Synthesizer.Mutate.InitializationError -> 0.0
      in
      acc_mat.(i).(n) <- acc
    done
  done;
  acc_mat

let num_run_from_init_set = 1

let inner_repeat_num = 2

let indudctive_run env init_op_set rest_op_set e bound =
  let rs =
    QCheck.Gen.generate ~n:inner_repeat_num (QCheck.Gen.shuffle_l rest_op_set)
  in
  let cal_acc_arr rest_op_set =
    Array.init e ~f:(fun k ->
        let len = k + 1 in
        let pool =
          Primitive.Operator.basic_op_pool @ init_op_set
          @ List.sub rest_op_set ~pos:0 ~len
        in
        let accs =
          List.init num_run_from_init_set ~f:(fun _ ->
              let acc =
                try one_pass env pool bound
                with Synthesizer.Mutate.InitializationError -> 0.0
              in
              acc)
        in
        list_mean accs)
  in
  List.map ~f:cal_acc_arr rs

let outer_repeat_num = 2

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
  let () =
    List.iter
      ~f:(fun (a, b, res) ->
        Printf.printf "init: %s; rest: %s\n"
          (List.to_string ~f:(fun x -> x) a)
          (List.to_string ~f:(fun x -> x) b);
        List.iter
          ~f:(fun arr ->
            Printf.printf "\t[%s]\n"
            @@ List.to_string ~f:string_of_float
            @@ Array.to_list arr)
          res)
      res
  in
  (* let res = List.map ~f:(fun (_, _, mat) -> Array.transpose_exn mat) res in *)
  List.concat @@ List.map ~f:(fun (_, _, x) -> x) res

let random_select env name (s, e) bound =
  (* random_v1 @@ random_select_helper env name (s, e) num_op_pools_per_bound bound *)
  let pool = random_select_helper name (s, e) in
  random_v3 env pool (s, e) bound

let ind =
  Command.basic ~summary:"inductively increasing the number of operators we use"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and name = anon ("name of datatype" %: string)
      and s = anon ("number of initial operators" %: int)
      and e = anon ("number of final operators" %: int)
      and time_in_second = anon ("total_time in second" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            Zlog.event_
              (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
              (fun () ->
                let env = mk_env_from_files source_file meta_file in
                let ind_len = e - s + 1 in
                let acc_mat =
                  random_select env name (s, e)
                    Synthesizer.Syn.(TimeBound (float time_in_second))
                in
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
