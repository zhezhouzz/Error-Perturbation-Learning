open Log
open Printf
;;
let ppf = Format.err_formatter
;;
open Core

let test () =
  let open Primitive in
  let i_err = [Value.L [1;2]; Value.L [3;4]] in
  let tps = [Tp.IntList; Tp.IntList] in
  let sampling_rounds = 3 in
  let prog_size = 4 in
  let env = Synthesizer.Mkenv.mk_env Imp.sigma_merge Imp.prog_merge Imp.phi_merge
      tps i_err Operator.op_pool sampling_rounds prog_size in
  let open Synthesizer in
  let env = Mcmc.metropolis_hastings
      ~burn_in:10
      ~proposal_distribution:Mutate.mutate
      ~cost_function:Cost.cost
      ~init_distribution: env in
  let () = Printf.printf "prog:\n%s\n" (Language.Oplang.layout env.cur_p.prog) in
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
      in
      fun () -> Config.exec_main configfile (fun () ->
          (* event "test" (fun () -> Printf.printf "test!\n"; Language.Arg_solving.test ()) *)
          event "test" (fun () -> Printf.printf "test!\n";
                         (* Synthesizer.Mutate.test (); *)
                         (* Synthesizer.Sampling.test (); *)
                         test ()
                       )
        )
    )

let command =
  Command.group ~summary:"Error Perturbation Learning"
    [ "test", test;
    ]

let () = Command.run command
;;
