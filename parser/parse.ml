open Language
open Primitive
module T = Tp
module Lang = Oplang
open Basic_dt;;

let to_tp l =
  match l with
  | ["int"] -> T.Int
  | ["int"; "list"] -> T.IntList
  | ["int"; "tree"] -> T.IntTree
  | _ -> raise @@ failwith (Printf.sprintf "parsing error: unknonw tp(%s)" (List.split_by_comma (fun x -> x) l))

let to_arg (_, name, tp) =
  let hd = String.get name 0 in
  let tl = String.sub name 1 (String.length name - 1) in
  match hd with
  | 'x' -> to_tp tp, int_of_string tl
  | _ -> raise @@ failwith "parsing error: invalid argument name(should initial with x)"
let to_statement (_, res, op, args) =
  let res, args = Sugar.map2 (List.map to_arg) (res, args) in
  Lang.({op = op; args = args; res = res})

let to_prog (inputs, body, outputs) =
  let inputs, outputs = Sugar.map2 (List.map to_arg) (inputs, outputs) in
  Lang.({fin = inputs; body = List.map to_statement body; fout = outputs})

let layout_position (p: Lexing.position) =
  let open Lexing in
  spf "At line %i, offset %i: syntax error" p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_ linebuf =
  to_prog
  (* @@ parse_ Parser.prog_eof (Lexing.from_channel (open_in filename)) *)
  @@
  (try
     Parser.prog_eof Lexer.next_token linebuf
   with
   | Lexer.LexError msg ->
     raise @@ failwith (Printf.sprintf "%s%!" msg)
   | Parser.Error ->
     raise @@ failwith (layout_position @@ Lexing.lexeme_end_p linebuf)
  )

let parse filename =
  let linebuf = Lexing.from_channel (open_in filename) in
  parse_ linebuf
let parse_string str =
  parse_ @@ Lexing.from_string str

type parsing_res =
  | ParsePre of string
  | ParseF of string

let parse_piecewise filename =
  let open Stdlib in
  let ic = open_in filename in
  let rec loop tmp current =
    try
      let line = input_line ic in
      match line with
      | "Pre" -> loop (tmp @ [current]) (ParsePre "")
      | "Perturbation" | "Default" -> loop (tmp @ [current]) (ParseF "")
      | line ->
        let current =
          match current with
          | ParsePre x -> ParsePre (spf "%s\n%s" x line)
          | ParseF x -> ParseF (spf "%s\n%s" x line)
        in
        loop tmp current
    with _ ->
      close_in ic;
      tmp @ [current]
  in
  let res = loop [] (ParseF "") in
  res

let solve_piecewise (c: parsing_res list) =
  match List.rev c with
  | [] | (ParsePre _) :: _ -> raise @@ failwith "wrong format of piecewise perturbation function"
  | (ParseF x) :: c ->
    let c = List.rev c in
    let default_f = parse_string x in
    let get_pre x = Language.Of_ocamlast.load_precondition default_f.Lang.fin @@ Ocaml_parser.Frontend.parse_string x in
    let rec aux (previous, pre_opt) c =
      match c, pre_opt with
      | [], None -> previous
      | [], Some _ -> raise @@ failwith "wrong format of piecewise perturbation function"
      | (ParsePre _) :: _, Some _ -> raise @@ failwith "perturbation function expected"
      | (ParsePre x) :: t, None -> aux (previous, Some (get_pre x)) t
      | (ParseF x) :: t, Some pre ->
        let f = parse_string x in
        aux (previous @ [pre, f], None) t
      | (ParseF _) :: _, None -> raise @@ failwith "wrong format of piecewise perturbation function"
    in
    aux ([], None) c
