open Basic_dt

let encode_field_ treetp_name field value : Yojson.Basic.t =
  `Assoc [ ("t", `String treetp_name); ("f", `String field); ("v", value) ]

let decode_field_ treetp_name (json : Yojson.Basic.t) =
  let open Yojson.Basic in
  let open Util in
  let treetp = json |> member "t" |> to_string in
  if String.equal treetp_name treetp then
    let field = json |> member "f" |> to_string in
    let value = json |> member "v" in
    (field, value)
  else raise @@ failwith (Printf.sprintf "%s::decode wrong type" treetp_name)

let decode_list err decoder json =
  match json with
  | `List l -> List.map decoder l
  | _ -> raise @@ failwith (Printf.sprintf "decode_list:%s" err)

module T = struct
  type t = Bool | Int | IntList | IntTree | IntTreeI | IntTreeB

  type tpedvar = t * string

  let encode = function
    | Bool -> `String "B"
    | Int -> `String "I"
    | IntList -> `String "IL"
    | IntTree -> `String "IT"
    | IntTreeI -> `String "ITI"
    | IntTreeB -> `String "ITB"

  open Yojson.Basic

  let decode json =
    let open Util in
    let tp = to_string json in
    if String.equal "B" tp then Bool
    else if String.equal "I" tp then Int
    else if String.equal "IL" tp then IntList
    else if String.equal "IT" tp then IntTree
    else if String.equal "ITI" tp then IntTreeI
    else if String.equal "ITB" tp then IntTreeB
    else raise @@ failwith "Lit.Tree::decode wrong type"

  let tpedvar_encode (tp, name) =
    `Assoc [ ("t", `String "tpv"); ("tp", encode tp); ("n", `String name) ]

  let tpedvar_decode json =
    let open Util in
    let treetp = json |> member "t" |> to_string in
    if String.equal "tpv" treetp then
      let tp = json |> member "tp" |> decode in
      let name = json |> member "n" |> to_string in
      (tp, name)
    else raise @@ failwith (Printf.sprintf "%s::decode wrong type" "tpedvar")

  module Tp = Primitive.Tp

  let to_tp = function
    | Bool -> Tp.Bool
    | Int -> Tp.Int
    | IntList -> Tp.IntList
    | IntTree -> Tp.IntTree
    | IntTreeI -> Tp.IntTreeI
    | IntTreeB -> Tp.IntTreeB
end

module L = struct
  type t =
    | Int of int
    | Bool of bool
    | IntList of int list
    | IntTree of int Tree.t

  let treetp_name = "Lit"

  let encode_field = encode_field_ treetp_name

  let decode_field = decode_field_ treetp_name

  let encode = function
    | Int i -> encode_field "Int" (`Int i)
    | Bool b -> encode_field "Bool" (`Bool b)
    | _ -> raise @@ failwith "Lit::encode"

  let decode (json : Yojson.Basic.t) : t =
    let e = failwith (Printf.sprintf "%s::decode wrong field" treetp_name) in
    let open Yojson.Basic in
    let open Util in
    let field, value = decode_field json in
    if String.equal "Int" field then Int (to_int value)
    else if String.equal "Bool" field then Bool (to_bool value)
    else raise e
end

type op = string

module SE = struct
  type t =
    | Literal of T.t * L.t
    | Var of T.t * string
    | Op of T.t * op * t list

  let treetp_name = "S"

  let encode_field = encode_field_ treetp_name

  let decode_field = decode_field_ treetp_name

  let rec encode = function
    | Literal (tp, lit) ->
        encode_field "Lit" (`List [ T.encode tp; L.encode lit ])
    | Var (tp, name) -> encode_field "V" (`List [ T.encode tp; `String name ])
    | Op (tp, op, args) ->
        encode_field "Op"
          (`List [ T.encode tp; `String op; `List (List.map encode args) ])

  let rec decode json =
    let e = failwith (Printf.sprintf "%s::decode wrong field" treetp_name) in
    let open Yojson.Basic in
    let open Util in
    let field, value = decode_field json in
    match (field, value) with
    | field, `List [ tp; lit ] when String.equal "Lit" field ->
        Literal (T.decode tp, L.decode lit)
    | field, `List [ tp; name ] when String.equal "V" field ->
        Var (T.decode tp, to_string name)
    | field, `List [ tp; op; `List args ] when String.equal "Op" field ->
        Op (T.decode tp, to_string op, List.map decode args)
    | _ -> raise e
end

module E = struct
  type t =
    | True
    | Atom of SE.t
    | Implies of t * t
    | Ite of t * t * t
    | Not of t
    | And of t list
    | Or of t list
    | Iff of t * t

  type free_variable = T.tpedvar

  type forallformula = free_variable list * t

  let treetp_name = "E"

  let encode_field = encode_field_ treetp_name

  let decode_field = decode_field_ treetp_name

  let rec encode = function
    | True -> encode_field "ETrue" (`List [])
    | Atom bexpr -> encode_field "EAtom" (`List [ SE.encode bexpr ])
    | Implies (p1, p2) ->
        encode_field "EImplies" (`List [ encode p1; encode p2 ])
    | And ps -> encode_field "EAnd" (`List (List.map encode ps))
    | Or ps -> encode_field "EOr" (`List (List.map encode ps))
    | Not p -> encode_field "ENot" (`List [ encode p ])
    | Iff (p1, p2) -> encode_field "EIff" (`List [ encode p1; encode p2 ])
    | Ite (p1, p2, p3) ->
        encode_field "EIte" (`List [ encode p1; encode p2; encode p3 ])

  let rec decode json =
    let e = failwith (Printf.sprintf "%s::decode wrong field" treetp_name) in
    let field, value = decode_field json in
    match (field, value) with
    | field, `List [] when String.equal "ETrue" field -> True
    | field, `List [ bexpr ] when String.equal "EAtom" field ->
        Atom (SE.decode bexpr)
    | field, `List [ p1; p2 ] when String.equal "EImplies" field ->
        Implies (decode p1, decode p2)
    | field, `List ps when String.equal "EAnd" field -> And (List.map decode ps)
    | field, `List ps when String.equal "EOr" field -> Or (List.map decode ps)
    | field, `List [ p ] when String.equal "ENot" field -> Not (decode p)
    | field, `List [ p1; p2 ] when String.equal "EIff" field ->
        Iff (decode p1, decode p2)
    | field, `List [ p1; p2; p3 ] when String.equal "EIte" field ->
        Ite (decode p1, decode p2, decode p3)
    | _ -> raise e

  let forallformula_tpname = "ff"

  let forallformula_encode (qv, body) =
    `Assoc
      [
        ("treetp", `String forallformula_tpname);
        ("qv", `List (List.map T.tpedvar_encode qv));
        ("body", encode body);
      ]

  let forallformula_decode json =
    let e =
      failwith (Printf.sprintf "%s::decode wrong type" forallformula_tpname)
    in
    let open Yojson.Basic in
    let open Util in
    let treetp = json |> member "treetp" |> to_string in
    if String.equal forallformula_tpname treetp then
      let qv =
        match json |> member "qv" with
        | `List qv -> List.map T.tpedvar_decode qv
        | _ -> raise e
      in
      let body = json |> member "body" |> decode in
      (qv, body)
    else raise e
end

type t =
  | ForAll of E.forallformula
  | Implies of t * t
  | Ite of t * t * t
  | Not of t
  | And of t list
  | Or of t list
  | Iff of t * t
  | SpecApply of string * SE.t list

type spec = T.tpedvar list * E.forallformula

type murphy_inp = { mname : string; mspec : spec; mss : Evalue.t list list }

let treetp_name = "A"

let encode_field = encode_field_ treetp_name

let decode_field = decode_field_ treetp_name

let rec encode = function
  | ForAll _ -> raise @@ failwith "never happen ast encode"
  | Implies (p1, p2) -> encode_field "AImplies" (`List [ encode p1; encode p2 ])
  | And ps -> encode_field "AAnd" (`List (List.map encode ps))
  | Or ps -> encode_field "AOr" (`List (List.map encode ps))
  | Not p -> encode_field "ANot" (`List [ encode p ])
  | Iff (p1, p2) -> encode_field "AIff" (`List [ encode p1; encode p2 ])
  | Ite (p1, p2, p3) ->
      encode_field "AIte" (`List [ encode p1; encode p2; encode p3 ])
  | SpecApply (specname, args) ->
      encode_field "ASpecApply"
        (`List [ `String specname; `List (List.map SE.encode args) ])

let rec decode json =
  let e = failwith (Printf.sprintf "%s::decode wrong field" treetp_name) in
  let field, value = decode_field json in
  match (field, value) with
  (* | field, _ when String.equal "Forall" field -> True *)
  | field, `List [ p1; p2 ] when String.equal "AImplies" field ->
      Implies (decode p1, decode p2)
  | field, `List ps when String.equal "AAnd" field -> And (List.map decode ps)
  | field, `List ps when String.equal "AOr" field -> Or (List.map decode ps)
  | field, `List [ p ] when String.equal "ANot" field -> Not (decode p)
  | field, `List [ p1; p2 ] when String.equal "AIff" field ->
      Iff (decode p1, decode p2)
  | field, `List [ p1; p2; p3 ] when String.equal "AIte" field ->
      Ite (decode p1, decode p2, decode p3)
  | field, `List [ `String specname; `List args ]
    when String.equal "ASpecApply" field ->
      SpecApply (specname, List.map SE.decode args)
  | _ -> raise e

let spec_tpname = "spec"

let spec_encode (args, specbody) =
  `Assoc
    [
      ("treetp", `String spec_tpname);
      ("args", `List (List.map T.tpedvar_encode args));
      ("specbody", E.forallformula_encode specbody);
    ]

let spec_decode json =
  let e = failwith (Printf.sprintf "%s::decode wrong type" spec_tpname) in
  let open Yojson.Basic in
  let open Util in
  let treetp = json |> member "treetp" |> to_string in
  if String.equal spec_tpname treetp then
    let qv =
      match json |> member "args" with
      | `List qv -> List.map T.tpedvar_decode qv
      | _ -> raise e
    in
    let body = json |> member "specbody" |> E.forallformula_decode in
    (qv, body)
  else raise e

let spectable_encode tab =
  let j =
    StrMap.fold
      (fun name spec r ->
        (* let _ = printf "spectable_encode find name:%s\n" name in *)
        `Assoc [ ("name", `String name); ("spec", spec_encode spec) ] :: r)
      tab []
  in
  `List j

let spectable_decode = function
  | `List l ->
      List.fold_left
        (fun r json ->
          let open Yojson.Basic in
          let name = json |> Util.member "name" |> Util.to_string in
          (* let _ = printf "spectable_decode find name:%s\n" name in *)
          let spec = json |> Util.member "spec" |> spec_decode in
          StrMap.add name spec r)
        StrMap.empty l
  | _ -> raise @@ failwith (Printf.sprintf "%s::decode wrong type" "spec table")

module S = Specification.Specast

let elrond_tv_to_tv (tp, name) = (T.to_tp tp, name)

let elrond_epr_to_body x =
  let open E in
  let rec aux = function
    | True -> S.True
    | Atom (SE.Op (_, op, args)) ->
        let se_to_v = function
          | SE.Var (t, name) -> (T.to_tp t, name)
          | _ -> raise @@ failwith "die"
        in
        S.MethodPredicate (op, List.map se_to_v args)
    | Atom _ -> raise @@ failwith "die"
    | Implies (e1, e2) -> S.Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> S.Ite (aux e1, aux e2, aux e3)
    | Not e -> S.Not (aux e)
    | And l -> S.And (List.map aux l)
    | Or l -> S.Or (List.map aux l)
    | Iff (e1, e2) -> S.Iff (aux e1, aux e2)
  in
  aux x

let elrond_spec_to_spec (args, (qv, x)) =
  Specification.Spec.
    {
      args = List.map elrond_tv_to_tv args;
      qv = List.map elrond_tv_to_tv qv;
      body = elrond_epr_to_body x;
    }

let elrond_spectab_to_spectab m = StrMap.map elrond_spec_to_spec m

let load_spectab filename =
  elrond_spectab_to_spectab @@ spectable_decode
  @@ Yojson.Basic.from_file filename

let load_all spectabfile alphafile =
  let spectab = load_spectab spectabfile in
  let alphas = Evalue.load_alpha alphafile in
  StrMap.mapi
    (fun k v ->
      match List.find_opt (fun (name, _) -> String.equal k name) alphas with
      | None -> raise @@ failwith "die"
      | Some (_, alphas) -> (v, alphas))
    spectab

let show_all x =
  StrMap.iter
    (fun name (spec, alphas) ->
      Zlog.log_write
      @@ spf "%s:\n%s\n%s\n" name
           (Specification.Spec.layout spec)
           (List.split_by "\n" Primitive.Value.layout_l alphas))
    x
