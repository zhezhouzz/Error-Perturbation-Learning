open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux

let cind_num_sampling = 100

let num_op_pools_per_bound = 3

let one_pass env selected_op_pool bound =
  let () =
    Zlog.log_write @@ Printf.sprintf "pool: %s"
    @@ List.to_string ~f:(fun x -> x) selected_op_pool
  in
  let _, prog =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> Synthesizer.Syn.synthesize_multi_core env 1 bound)
  in
  let env = Synthesizer.Env.{ env with op_pool = selected_op_pool } in
  let _, none_num, data =
    Sampling.Scache.eval_sampling [ env.i_err ] [ prog ]
      (fun _ -> true)
      cind_num_sampling
  in
  let stat =
    Evaluation.Ev.evaluation none_num data env.sigma
      (fun inp -> snd @@ env.client env.library_inspector inp)
      env.phi
  in
  let acc = Evaluation.Ev.get_acc stat in
  let () =
    Zlog.log_write
    @@ Printf.sprintf "%s\nacc:%f\n" (Evaluation.Ev.layout_eval "" stat 0.0) acc
  in
  acc

let random_select env name (s, e) bound =
  let open Primitive.Operator in
  let pool_arr = Array.of_list @@ get_pool_by_name name in
  let acc_arr = Array.init (Array.length pool_arr + 1) ~f:(fun _ -> 0.0) in
  let () =
    if e > Array.length pool_arr then
      raise
      @@ failwith
           (Printf.sprintf
              "error: the end value (%i) is greater than the number of all \
               operators (%i)"
              e (Array.length pool_arr))
    else ()
  in
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
                let accs =
                  random_select env name (s, e)
                    Synthesizer.Syn.(TimeBound (float time_in_second))
                in
                Printf.printf "client: %s\nname: %s\nacc: %i -> %i\n%s\n"
                  source_file name s e
                  (List.to_string accs ~f:string_of_float))))
