open Language
open Primitive
module T = Tp
module L = Oplang
open Basic_dt;;

let to_tp l =
  match l with
  | ["int"] -> T.Int
  | ["int"; "list"] -> T.IntList
  | _ -> raise @@ failwith (Printf.sprintf "parsing error: unknonw tp(%s)" (List.split_by_comma (fun x -> x) l))

let to_arg (_, name, tp) =
  let hd = String.get name 0 in
  let tl = String.sub name 1 (String.length name - 1) in
  match hd with
  | 'x' -> to_tp tp, int_of_string tl
  | _ -> raise @@ failwith "parsing error: invalid argument name(should initial with x)"
let to_statement (_, res, op, args) =
  let res, args = Sugar.map2 (List.map to_arg) (res, args) in
  L.({op = op; args = args; res = res})

let to_prog (inputs, body, outputs) =
  let inputs, outputs = Sugar.map2 (List.map to_arg) (inputs, outputs) in
  L.({fin = inputs; body = List.map to_statement body; fout = outputs})

let parse filename =
  to_prog @@ Parser.prog_eof Lexer.next_token (Lexing.from_channel (open_in filename))
