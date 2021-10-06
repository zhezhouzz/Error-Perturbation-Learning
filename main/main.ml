open Log
open Printf
;;
let ppf = Format.err_formatter
;;
open Core

let mk_standard_env () =
  let open Primitive in
  let i_err = [Value.L [1;2]; Value.L [3;4]] in
  let tps = [Tp.IntList; Tp.IntList] in
  let sampling_rounds = 6 in
  let prog_size = 4 in
  let library = ["is_empty", Imp.is_empty;
                 "cons", Imp.cons;
                 "tail", Imp.tail;
                 "top", Imp.top;
                ] in
  Synthesizer.Mkenv.mk_env Imp.sigma_merge Invocation.merge library Imp.phi_merge
    tps i_err Operator.op_pool sampling_rounds prog_size

let test_cost () =
  Synthesizer.Cost.test (mk_standard_env ())

let test_mcmc () =
  let open Synthesizer in
  let env, cost = Mcmc.metropolis_hastings
      ~burn_in: 300
      ~sampling_steps: 30
      ~proposal_distribution: Mutate.mutate
      ~cost_function: Cost.cost
      ~init_distribution: (mk_standard_env ()) in
  let () = Printf.printf "prog(cost: %f):\n%s\n" cost (Language.Oplang.layout env.cur_p.prog) in
  ()

let test_oplang () =
  let open Synthesizer in
  let () = Language.Oplang.test () in
  ()

let test_feature () =
  let qv = [Primitive.Tp.Int, "u"; Primitive.Tp.Int, "v"] in
  let args = [Primitive.Tp.IntList, "l1"; Primitive.Tp.IntList, "l2"] in
  let mps = ["mem"; "hd"; "=="; "<"] in
  let inferctx = Classify.Infer.mk_infer_ctx args qv mps in
  let () = Printf.printf "fset: %s\n" (Classify.Feature.layout_set inferctx.Classify.Feature_vector.fset) in
  let module V = Primitive.Value in
  let () = Classify.Infer.pre_infer inferctx [[V.L [1;2]; V.L [3;4]]] (fun _ -> true) in
  ()

let batched_test num_times num_burn_in num_sampling =
  let open Synthesizer in
  let rec aux n =
    if n >= num_times then () else
      let env, cost =
        event (Printf.sprintf "test(%i)" n) (
          fun () -> Printf.printf "test!\n";
            Mcmc.metropolis_hastings
              ~burn_in: num_burn_in
              ~sampling_steps: num_sampling
              ~proposal_distribution: Mutate.mutate
              ~cost_function: Cost.cost
              ~init_distribution: (mk_standard_env ())
        )
      in
      let () = Log.log_write @@ Printf.sprintf "prog(cost: %f):\n%s\n" cost (Language.Oplang.layout env.cur_p.prog) in
      let () = Config.refresh_logfile (string_of_int n) in
      aux (n + 1)
  in
  aux 0

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")

let test = Command.basic
    ~summary:"test."
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and test_name = anon ("test name" %: string)
      in
      fun () -> Config.exec_main configfile (fun () ->
          (* event "test" (fun () -> Printf.printf "test!\n"; Language.Arg_solving.test ()) *)
          match test_name with
          | "cost" -> event "test" (fun () -> Printf.printf "test!\n"; test_cost ())
          | "mcmc" -> event "test" (fun () -> Printf.printf "test!\n"; test_mcmc ())
          | "oplang" -> event "test" (fun () -> Printf.printf "test!\n"; test_oplang ())
          | "feature" -> event "test" (fun () -> Printf.printf "test!\n"; test_feature ())
          | _ -> raise @@ failwith "unknown test name"
        )
    )

let batched_test = Command.basic
    ~summary:"batched test."
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and num_times = anon ("num times" %: int)
      and num_burn_in = anon ("num burn-in" %: int)
      and num_sampling = anon ("num sampling" %: int)
      in
      fun () -> Config.exec_main configfile (fun () ->
          batched_test num_times num_burn_in num_sampling
        )
    )

let parse =  Command.basic
    ~summary:"parse"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      in
      fun () -> Config.exec_main configfile (fun () ->
          Printf.printf "%s\n" @@
          Language.Oplang.layout @@ Parse.parse source_file
        )
    )

let command =
  Command.group ~summary:"Error Perturbation Learning"
    [ "test", test;
      "batched-test", batched_test;
      "parse", parse;
    ]

let () = Command.run command
;;
