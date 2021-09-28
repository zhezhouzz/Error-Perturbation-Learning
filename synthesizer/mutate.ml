open QCheck;;
open Language;;
open Basic_dt;;
let arg_reassign cache = Gen.oneofl @@
  List.map (fun m -> (fun (tps, ops, prog, cache) ->
      Some (tps, ops, Oplang.subst m prog, cache))) cache.Arg_solving.solutions

let op_swap prog_len =
  Gen.map (fun (i, j) ->
      (fun (tps, ops, _, _) ->
         (* let _ = Printf.printf "len(ops) = %i; i = %i; j = %i\n" (List.length ops) i j in *)
         let ops = List.swap_exn ops i j in
         Arg_solving.arg_assign tps ops
      )
    ) @@ Gen.pair (Gen.int_bound (prog_len - 1)) (Gen.int_bound (prog_len - 1))

let op_replace prog_len op_pool =
  Gen.map (fun (i, op) ->
      (fun (tps, ops, _, _) ->
         let ops = List.replace_exn ops i op in
         Arg_solving.arg_assign tps ops)
    ) @@ Gen.pair (Gen.int_bound (prog_len - 1)) (Gen.oneofl op_pool)

let op_deny prog_len =
  Gen.map (fun i ->
      (fun (tps, ops, _, _) ->
         let ops = List.replace_exn ops i Primitive.Operator.unused in
         Arg_solving.arg_assign tps ops)
    ) @@ Gen.int_bound (prog_len - 1)

let mutate op_pool (tps, ops, prog, cache) =
  let open Config in
  let md = !conf.mutation_distribution in
  let prog_len = List.length ops in
  let gen = Gen.frequency
      [md.arg_reassign, arg_reassign cache;
       md.op_swap, op_swap prog_len;
       md.op_replace, op_replace prog_len op_pool;
       md.op_deny, op_deny prog_len;
      ] in
  let counter = ref 0 in
  let rec loop () =
    if !counter > 10 then raise @@ failwith "mutate too many times" else
      match (Gen.generate1 gen) (tps, ops, prog, cache) with
      | Some r -> r
      | None -> loop ()
  in
  loop ()

let test () =
  let tps, ops = Oplang.test_tps_ops in
  match Arg_solving.arg_assign tps ops with
  | None -> raise @@ failwith "never happen"
  | Some (tps, ops, prog, cache) ->
    let _ = Printf.printf "prog:\n%s\n" (Oplang.layout prog) in
    let (_, _, prog, _) = mutate Primitive.Operator.op_pool (tps, ops, prog, cache) in
    let _ = Printf.printf "prog:\n%s\n" (Oplang.layout prog) in
    ();;
