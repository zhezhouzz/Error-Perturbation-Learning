(*Figaro style*)
let metropolis_hastings
    (*the steps before sampling, >= 0*)
    ~burn_in:(burn_in: int)
    (* the steps of sampling phase *)
    ~sampling_steps: (sampling: int)
    (*proposal_distribution maps a "f" to a new "f", which is a non-deterministic function; probably has other types
      e.g 'a -> 'a QCheck.Gen.t which returns a random generator of "f";
      e.g 'a -> ('a -> 'a) QCheck.Gen.t which returns a random generator of mapping of "f" to "f";
    *)
    ~proposal_distribution:(mutate: 'a -> 'a)
    (*cost function can be replaced with condition in some library; but they should provide something like"soft condition"...*)
    ~cost_function: (cal_cost: 'a -> float)
    (*initial "f" *)
    ~init_distribution:(init: 'a)
  (*Here we only need one "f", some library will return a set of "f" or distribution of "f", we dont need it. *)
  : 'a =
  let counter = ref 0 in
  let best_one = ref None in
  let update_best_one (cur, cur_cost) =
    match !best_one with
    | None -> ()
    | Some (_, prev_cost) ->
      if cur_cost < prev_cost then best_one := Some (cur, cur_cost) else ()
  in
  let rec loop (cur, cur_cost) =
    update_best_one (cur, cur_cost);
    if !counter == 0 then cur, cur_cost else
      let _ = Printf.printf "------MCMC %i------\n" (!counter) in
      let _ = counter := !counter - 1 in
      let next = mutate cur in
      let next_cost = cal_cost next in
      if next_cost < cur_cost
      then loop (next, next_cost)
      else if Random.float 1.0 < (cur_cost /. next_cost)
      then loop (next, next_cost)
      else loop (cur, cur_cost)
  in
  (* warm up *)
  let _ = counter := burn_in in
  let result, result_cost = loop (init, cal_cost init) in
  (* sampling *)
  let _ = best_one := Some (result, result_cost) in
  let _ = counter := sampling in
  let result = match !best_one with
    | Some (result, _) -> result
    | None -> raise @@ failwith "never happen in mcmc"
  in
  let _ = Printf.printf "------MCMC End------\n" in
  result
