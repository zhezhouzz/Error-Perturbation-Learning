(*Figaro style*)
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
  let counter = ref 0 in
  let bound = ref (burn_in + sampling) in
  let best_one = ref (Some (init, cal_cost init)) in
  let update_best_one (cur_id, cur, cur_cost) =
    match !best_one with
    | None -> ()
    | Some (_, prev_cost) ->
        if cur_cost < prev_cost then (
          best_one := Some (cur, cur_cost);
          match cur_id with
          | None -> ()
          | Some id ->
              Zlog.log_write (Printf.sprintf "\tnow the best one is %i" id))
        else ()
  in
  let rec loop (cur, cur_cost) =
    (* (if !counter mod 20 == 0 then Printf.printf "MCMC: %i\n" !counter else ()); *)
    let cur_id =
      Syn_stat.add
        (match cur.Env.cur_p with
        | None -> raise @@ failwith "the env has not prog initialized"
        | Some x -> x.prog)
    in
    let _ = update_best_one (cur_id, cur, cur_cost) in
    if !counter >= !bound then (cur, cur_cost)
    else
      let next, next_cost =
        Zlog.event_
          (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__
             (string_of_int !counter))
          (fun () ->
            let next = mutate cur in
            let next_cost = cal_cost next in
            (next, next_cost))
      in
      let _ = counter := !counter + 1 in
      if next_cost < cur_cost then loop (next, next_cost)
      else if Random.float 1.0 < cur_cost /. next_cost then
        loop (next, next_cost)
      else loop (cur, cur_cost)
  in
  let () = Syn_stat.init () in
  let _ = loop (init, cal_cost init) in
  let result, cost =
    match !best_one with
    | Some (result, cost) -> (result, cost)
    | None -> raise @@ failwith "never happen in mcmc"
  in
  (result, cost)
