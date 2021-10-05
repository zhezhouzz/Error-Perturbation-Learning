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
          event "test" (fun () -> Printf.printf "test!\n";
                         match test_name with
                         | "cost" -> test_cost ()
                         | "mcmc" -> test_mcmc ()
                         | _ -> raise @@ failwith "unknown test name"
                       )
        )
    )

let command =
  Command.group ~summary:"Error Perturbation Learning"
    [ "test", test;
    ]

let () = Command.run command
;;
