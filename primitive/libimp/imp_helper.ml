(* open Value *)
(* open Imp *)

let exn file line =
  failwith
    (Printf.sprintf "runtime operator(defined at file %s line %i) error" file
       line)
