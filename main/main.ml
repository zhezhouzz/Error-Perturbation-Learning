open Log
open Printf
;;
let ppf = Format.err_formatter
;;
open Core

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
          event "test" (fun () -> Printf.printf "test!\n"; Synthesizer.Mutate.test ())
        )
    )

let command =
  Command.group ~summary:"Error Perturbation Learning"
    [ "test", test;
    ]

let () = Command.run command
;;
