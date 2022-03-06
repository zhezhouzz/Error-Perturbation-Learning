(*Figaro style*)

let layout_pf (cur, cur_cost) =
  match cur.Env.cur_p with
  | None -> raise @@ failwith "the env has not prog initialized"
  | Some x ->
      Zlog.log_write
        (Printf.sprintf "[%s:%i] prog(non-det: %b):\n%scost = %f" __FILE__
           __LINE__
           (Language.Oplang.check_non_det x.prog)
           (Language.Oplang.layout x.prog)
           cur_cost)

let update_best_one best_one (counter, cur, cur_cost) =
  match !best_one with
  | None -> false
  | Some (_, prev_cost) ->
      if cur_cost < prev_cost then (
        layout_pf (cur, cur_cost);
        best_one := Some (cur, cur_cost);
        Zlog.log_write
          (Printf.sprintf "\tnow the best one is found in %i step" counter);
        true)
      else false

let get_best_one best_one =
  match !best_one with
  | Some (result, cost) -> (result, cost)
  | None -> raise @@ failwith "never happen in mcmc"

let mcmc_jump mutate (cur, cal_cost) =
  Zlog.event_ (Printf.sprintf "%s:%i[%s]" __FILE__ __LINE__ __FUNCTION__)
    (fun () ->
      let next = mutate cur in
      let next_cost = cal_cost next in
      (next, next_cost))

let mcmc_judge (cur, cur_cost) (next, next_cost) =
  if next_cost < cur_cost then (next, next_cost)
  else if Random.float 1.0 < cur_cost /. next_cost then (next, next_cost)
  else (cur, cur_cost)

let stat_add cur =
  Syn_stat.add
    (match cur.Env.cur_p with
    | None -> raise @@ failwith "the env has not prog initialized"
    | Some x -> x.prog)

let metropolis_hastings_core cond mutate cal_cost init =
  (*Here we only need one "f", some library will return a set of "f" or distribution of "f", we dont need it. *)
  let counter = ref 0 in
  let best_one = ref (Some (init, cal_cost init)) in
  let rec loop (cur, cur_cost) =
    (* if !counter > 40 then raise @@ failwith "too many" else (); *)
    let _ = stat_add cur in
    let _ = update_best_one best_one (!counter, cur, cur_cost) in
    if cond !counter then (cur, cur_cost)
    else
      let next, next_cost = mcmc_jump mutate (cur, cal_cost) in
      (* let () = layout_pf (next, next_cost) in *)
      let _ = counter := !counter + 1 in
      loop @@ mcmc_judge (cur, cur_cost) (next, next_cost)
  in
  let () = Syn_stat.init () in
  let _ = loop (init, cal_cost init) in
  get_best_one best_one

let metropolis_hastings_core_moti cond mutate cal_cost init =
  (*Here we only need one "f", some library will return a set of "f" or distribution of "f", we dont need it. *)
  let counter = ref 0 in
  let best_step = ref 0 in
  let best_one = ref (Some (init, cal_cost init)) in
  let rec loop (cur, cur_cost) =
    (* if !counter > 40 then raise @@ failwith "too many" else (); *)
    let _ = stat_add cur in
    let if_updated = update_best_one best_one (!counter, cur, cur_cost) in
    let () = if if_updated then best_step := !counter else () in
    (* HACK: Early stop *)
    if cur_cost <= 0.001 then Some !counter
    else if cond !counter then None
    else
      let next, next_cost = mcmc_jump mutate (cur, cal_cost) in
      (* let () = layout_pf (next, next_cost) in *)
      let _ = counter := !counter + 1 in
      loop @@ mcmc_judge (cur, cur_cost) (next, next_cost)
  in
  let () = Syn_stat.init () in
  let stop_step = loop (init, cal_cost init) in
  (stop_step, get_best_one best_one)

let metropolis_hastings ~(*the steps before sampling, >= 0*)
                        (burn_in : int)
    ~sampling_steps:(* the steps of sampling phase *)
      (sampling : int)
    ~proposal_distribution:
      (*proposal_distribution maps a "f" to a new "f", which is a non-deterministic function; probably has other types
        e.g 'a -> 'a QCheck.Gen.t which returns a random generator of "f";
        e.g 'a -> ('a -> 'a) QCheck.Gen.t which returns a random generator of mapping of "f" to "f";
      *)
      (mutate : 'a -> 'a)
    ~cost_function:
      (*cost function can be replaced with condition in some library; but they should provide something like"soft condition"...*)
      (cal_cost : 'a -> float) ~init_distribution:(*initial "f" *)
                                 (init : 'a) : 'a * float =
  (*Here we only need one "f", some library will return a set of "f" or distribution of "f", we dont need it. *)
  let bound = burn_in + sampling in
  let cond counter = counter >= bound in
  metropolis_hastings_core cond mutate cal_cost init

let metropolis_hastings_time ~time_bound:(bound : float) (* in second *)
    ~proposal_distribution:
      (*proposal_distribution maps a "f" to a new "f", which is a non-deterministic function; probably has other types
        e.g 'a -> 'a QCheck.Gen.t which returns a random generator of "f";
        e.g 'a -> ('a -> 'a) QCheck.Gen.t which returns a random generator of mapping of "f" to "f";
      *)
      (mutate : 'a -> 'a)
    ~cost_function:
      (*cost function can be replaced with condition in some library; but they should provide something like"soft condition"...*)
      (cal_cost : 'a -> float) ~init_distribution:(*initial "f" *)
                                 (init : 'a) : 'a * float =
  (*Here we only need one "f", some library will return a set of "f" or distribution of "f", we dont need it. *)
  let start_time = Sys.time () in
  let cond _ =
    let t = Sys.time () -. start_time in
    (* let _ = *)
    (*   Zlog.log_write *)
    (*   @@ Printf.sprintf "record exec time: %f (%f) |> %b" t bound (t > bound) *)
    (* in *)
    t > bound
  in
  metropolis_hastings_core cond mutate cal_cost init

let metropolis_hastings_moti ~(*the steps before sampling, >= 0*)
                             (burn_in : int)
    ~sampling_steps:(* the steps of sampling phase *)
      (sampling : int)
    ~proposal_distribution:
      (*proposal_distribution maps a "f" to a new "f", which is a non-deterministic function; probably has other types
        e.g 'a -> 'a QCheck.Gen.t which returns a random generator of "f";
        e.g 'a -> ('a -> 'a) QCheck.Gen.t which returns a random generator of mapping of "f" to "f";
      *)
      (mutate : 'a -> 'a)
    ~cost_function:
      (*cost function can be replaced with condition in some library; but they should provide something like"soft condition"...*)
      (cal_cost : 'a -> float) ~init_distribution:(*initial "f" *)
                                 (init : 'a) : int option * ('a * float) =
  (*Here we only need one "f", some library will return a set of "f" or distribution of "f", we dont need it. *)
  let bound = burn_in + sampling in
  let cond counter = counter >= bound in
  metropolis_hastings_core_moti cond mutate cal_cost init

let metropolis_hastings_time_moti ~time_bound:(bound : float) (* in second *)
    ~proposal_distribution:
      (*proposal_distribution maps a "f" to a new "f", which is a non-deterministic function; probably has other types
        e.g 'a -> 'a QCheck.Gen.t which returns a random generator of "f";
        e.g 'a -> ('a -> 'a) QCheck.Gen.t which returns a random generator of mapping of "f" to "f";
      *)
      (mutate : 'a -> 'a)
    ~cost_function:
      (*cost function can be replaced with condition in some library; but they should provide something like"soft condition"...*)
      (cal_cost : 'a -> float) ~init_distribution:(*initial "f" *)
                                 (init : 'a) : int option * ('a * float) =
  (*Here we only need one "f", some library will return a set of "f" or distribution of "f", we dont need it. *)
  let start_time = Sys.time () in
  let cond _ =
    let t = Sys.time () -. start_time in
    (* let _ = *)
    (*   Zlog.log_write *)
    (*   @@ Printf.sprintf "record exec time: %f (%f) |> %b" t bound (t > bound) *)
    (* in *)
    t > bound
  in
  metropolis_hastings_core_moti cond mutate cal_cost init
