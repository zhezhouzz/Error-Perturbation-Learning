open QCheck;;
open Language;;
open Basic_dt;;
(* TODO: readable mutation operations *)
type mutation_operation =
  | ArgReassign of int
  | Swap of int * int
  | Replace of int * string
  | Deny of int

let layout_mutation = function
  | ArgReassign _ -> "re assign arguments"
  | Swap (i, j) -> Printf.sprintf "swap %ith op with %ith op" i j
  | Replace (i, op) -> Printf.sprintf "replace %ith op with %s" i op
  | Deny i -> Printf.sprintf "delete %ith op" i

let arg_reassign cache =
  match Arg_solving.cached_varaint_set cache with
  | [] -> Gen.pure None
  | variants -> Gen.opt @@ Gen.map (fun idx -> ArgReassign idx) @@ Gen.oneofl variants

let op_swap prog_len =
  if prog_len < 2 then Gen.pure None
  else
    Gen.opt @@ Gen.map (fun (i, j) -> Swap (i, j)) @@ Gen.pair (Gen.int_bound (prog_len - 1)) (Gen.int_bound (prog_len - 1))

let op_replace prog_len op_pool =
  if prog_len < 1 then Gen.pure None
  else
  Gen.opt @@ Gen.map (fun (i, op) -> Replace (i, op)) @@ Gen.pair (Gen.int_bound (prog_len - 1)) (Gen.oneofl op_pool)

let op_deny prog_len =
  if prog_len < 2 then Gen.pure None
  else
      Gen.opt @@ Gen.map (fun i -> Deny i) @@ Gen.int_bound (prog_len - 1)

let apply_mutation muation_op cache =
  match muation_op with
  | None -> None
  | Some muation_op ->
    match muation_op with
    | ArgReassign idx -> Some (Arg_solving.shift_within_in_cache cache idx)
    | Swap (i, j) ->
      let ops = List.swap_exn cache.ops i j in
      Arg_solving.arg_assign cache.tps ops
    | Replace (i, op) ->
      let ops = List.replace_exn cache.ops i op in
      Arg_solving.arg_assign cache.tps ops
    | Deny i ->
      let ops = List.replace_exn cache.ops i Primitive.Operator.unused in
      Arg_solving.arg_assign cache.tps ops

let mutate_ op_pool cache = (
  let open Config in
  let open Arg_solving in
  let md = !conf.mutation_distribution in
  let prog_len = List.length cache.ops in
  let gen = Gen.frequency
      [md.arg_reassign, arg_reassign cache;
       md.op_swap, op_swap prog_len;
       md.op_replace, op_replace prog_len op_pool;
       md.op_deny, op_deny prog_len;
      ] in
  let counter = ref 0 in
  let rec loop () =
    if !counter > 10 then raise @@ failwith "mutate too many times" else
      let mutation = Gen.generate1 gen in
      match apply_mutation mutation cache with
      | Some r -> Printf.printf "mutation:\n%s\n" (match mutation with
          | None -> raise @@ failwith "die in mutate_"
          | Some mutation -> layout_mutation mutation); r
      | None -> loop ()
  in
  loop ()
)

let mutate (env: Env.t) =
  let open Env in
  let (prog, acache) =
    mutate_ env.op_pool env.cur_p.acache in
  {env with cur_p = {prog = prog; acache = acache}}

let test () =
  let tps, ops = Oplang.test_tps_ops in
  match Arg_solving.arg_assign tps ops with
  | None -> raise @@ failwith "never happen"
  | Some (prog, cache) ->
    let _ = Printf.printf "prog:\n%s\n" (Oplang.layout prog) in
    let (prog, _) = mutate_ Primitive.Operator.op_pool cache in
    let _ = Printf.printf "prog:\n%s\n" (Oplang.layout prog) in
    ();;
