open Language
open Primitive
module T = Tp
module Lang = Oplang
open Basic_dt

let to_basic_tp = function
  | "bool" -> T.Bool
  | "int" -> T.Int
  | "instr" -> T.IfcInstr
  | _ -> raise @@ failwith "parse: wrong tp"

let to_tp (elem, l) =
  let elem = List.map to_basic_tp elem in
  match (elem, l) with
  | [ x ], [] -> x
  | _, [] -> raise @@ failwith "parse: wrong tp"
  | [ T.Int ], [ "list" ] -> T.IntList
  | [ T.Int; T.Bool ], [ "list" ] -> T.IntBoolList
  | [ T.Bool; T.Int; T.Bool ], [ "list" ] -> T.BoolIntBoolList
  | [ T.IfcInstr ], [ "list" ] -> T.IfcInstrList
  | [ T.Int ], [ "tree" ] -> T.IntTree
  | _ ->
      raise
      @@ failwith
           (Printf.sprintf "parsing error: unknonw tp(%s)"
              (List.split_by_comma (fun x -> x) l))

let to_arg (_, name, tp) =
  let hd = String.get name 0 in
  let tl = String.sub name 1 (String.length name - 1) in
  match hd with
  | 'x' -> (to_tp tp, int_of_string tl)
  | _ ->
      raise
      @@ failwith "parsing error: invalid argument name(should initial with x)"

let to_statement (_, res, op, args) =
  let res, args = Sugar.map2 (List.map to_arg) (res, args) in
  Lang.{ op; args; res }

let to_prog (inputs, body, outputs) =
  let inputs, outputs = Sugar.map2 (List.map to_arg) (inputs, outputs) in
  Lang.{ fin = inputs; body = List.map to_statement body; fout = outputs }

let layout_position (p : Lexing.position) =
  let open Lexing in
  spf "At line %i, offset %i: syntax error" p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_ linebuf =
  to_prog
  (* @@ parse_ Parser.prog_eof (Lexing.from_channel (open_in filename)) *)
  @@
  try Parser.prog_eof Lexer.next_token linebuf with
  | Lexer.LexError msg -> raise @@ failwith (Printf.sprintf "%s%!" msg)
  | Parser.Error ->
      raise @@ failwith (layout_position @@ Lexing.lexeme_end_p linebuf)

let parse filename =
  let linebuf = Lexing.from_channel (open_in filename) in
  parse_ linebuf

let parse_string str = parse_ @@ Lexing.from_string str

type parsing_res = ParseDefault | ParsePre of string | ParseF of string

(* TODO: multiple line i_err *)
let i_err_name = "i_err"

let parse_raw_piecewise filename =
  let open Stdlib in
  let ic = open_in filename in
  let close tmp current =
    match current with ParseF "" -> tmp | _ -> tmp @ [ current ]
  in
  let i_err =
    try
      let line = input_line ic in
      let name, lit =
        Language.Of_ocamlast.load_literal
        @@ Ocaml_parser.Frontend.parse_string line
      in
      if String.equal name i_err_name then lit
      else
        raise @@ failwith
        @@ spf
             "wrong format of piecewise perturbation function, initial error \
              should have name \"%s\""
             i_err_name
    with
    | Failure x -> raise @@ failwith x
    | _ ->
        raise
        @@ failwith
             "wrong format of piecewise perturbation function, expect a \
              initial error"
  in
  let rec loop tmp current =
    try
      let line = input_line ic in
      match (line, current) with
      | "Pre", ParseDefault -> loop tmp (ParsePre "")
      | "Pre", _ -> loop (tmp @ [ current ]) (ParsePre "")
      | "Perturbation", ParseDefault -> loop tmp (ParseF "")
      | "Perturbation", _ -> loop (close tmp current) (ParseF "")
      | "Default", ParseDefault -> loop tmp (ParseF "")
      | "Default", _ -> loop (close tmp current) (ParseF "")
      | line, ParsePre x -> loop tmp @@ ParsePre (spf "%s\n%s" x line)
      | line, ParseF x -> loop tmp @@ ParseF (spf "%s\n%s" x line)
      | line, ParseDefault -> loop tmp @@ ParseF line
    with _ ->
      close_in ic;
      tmp @ [ current ]
  in
  let res = loop [] ParseDefault in
  (i_err, res)

let solve_piecewise (c : parsing_res list) =
  (* let () = Printf.printf "len(c) = %i\n" @@ List.length c in *)
  match List.rev c with
  | [] | ParsePre _ :: _ | ParseDefault :: _ ->
      raise
      @@ failwith
           "wrong format of piecewise perturbation function, expect a case"
  | ParseF x :: c ->
      let c = List.rev c in
      (* Printf.printf "parsing f(%s)\n" x; *)
      let default_f = parse_string x in
      let get_pre x =
        Language.Of_ocamlast.load_precondition
          (List.map
             (fun (tp, idx) -> (tp, Lang.layout_var (tp, idx)))
             default_f.Lang.fin)
        @@ Ocaml_parser.Frontend.parse_string x
      in
      let rec aux (previous, pre_opt) c =
        match (c, pre_opt) with
        | [], None -> previous
        | [], Some _ ->
            raise
            @@ failwith
                 "wrong format of piecewise perturbation function, expect a \
                  case, but get nothing"
        | ParseDefault :: _, _ ->
            raise @@ failwith "parse piecewise should never happen"
        | ParsePre _ :: _, Some _ ->
            raise
            @@ failwith
                 "perturbation function expected, expect a case, but get \
                  another pre"
        | ParsePre x :: t, None ->
            (* Printf.printf "parsing pre(%s)\n" x; *)
            aux (previous, Some (get_pre x)) t
        | ParseF x :: t, Some pre ->
            (* Printf.printf "parsing f(%s)\n" x; *)
            let f = parse_string x in
            aux (previous @ [ (pre, f) ], None) t
        | ParseF _ :: _, None ->
            raise
            @@ failwith
                 "wrong format of piecewise perturbation function, expect a pre"
      in
      (aux ([], None) c, default_f)

let parse_piecewise filename =
  let i_err, c = parse_raw_piecewise filename in
  (i_err, solve_piecewise c)

let parse_verified_sigma args filename =
  Language.Of_ocamlast.load_precondition args
  @@ Ocaml_parser.Frontend.parse ~sourcefile:filename
