open Specast;;
open Basic_dt;;
open Primitive;;
module MP = Method_predicate;;
let desugar (prop: t) : t =
  let rec aux = function
    | True -> True
    | Bvar (t, b) -> Bvar (t, b)
    | MethodPredicate (mp, args) ->
      MethodPredicate (MP.instantization mp @@ List.map fst args, args)
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not e -> Not (aux e)
    | And l -> And (List.map aux l)
    | Or l -> Or (List.map aux l)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
  in
  aux prop

let eval e env =
  let get_val name = StrMap.find "prop::eval" env name in
  let rec aux = function
    | True -> true
    | Bvar (_, b) ->
      (match get_val b with
       | Value.B true -> true
       | Value.B false -> false
       | _ -> raise @@ failwith "wrong type in prop::eval")
    | MethodPredicate (mp, args) ->
      MP.apply mp @@
      List.map (fun (_, name) -> get_val name) args
    | Implies (e1, e2) -> if aux e1 then aux e2 else true
    | Ite (e1, e2, e3) -> if aux e1 then aux e2 else aux e3
    | Not e -> not (aux e)
    | And l -> List.for_all (fun x -> x) @@ List.map aux l
    | Or l -> List.exists (fun x -> x) @@ List.map aux l
    | Iff (e1, e2) -> (aux e1) == (aux e2)
  in
  aux e

let to_z3 ctx prop =
  let open Z3 in
  let rec aux = function
    | True -> Boolean.mk_true ctx
    | Bvar (t, b) -> Prover.Z3aux.tpedvar_to_z3 ctx (t, b)
    | MethodPredicate ("==", [a; b]) ->
      let (a, b) = Sugar.map2 (Prover.Z3aux.tpedvar_to_z3 ctx) (a, b) in
      Z3.Boolean.mk_eq ctx a b
    | MethodPredicate ("==", _) -> raise @@ failwith "prop to z3: bad =="
    | MethodPredicate ("<", [a; b]) ->
      let (a, b) = Sugar.map2 (Prover.Z3aux.tpedvar_to_z3 ctx) (a, b) in
      Z3.Arithmetic.mk_lt ctx a b
    | MethodPredicate ("<", _) -> raise @@ failwith "prop to z3: bad <"
    | MethodPredicate (mp, args) ->
      let args_z3 = List.map (Prover.Z3aux.tpedvar_to_z3 ctx) args in
      let func = Prover.Z3aux.z3func ctx mp (List.map fst args) Tp.Bool in
      Z3.FuncDecl.apply func args_z3
    | Implies (p1, p2) -> Boolean.mk_implies ctx (aux p1) (aux p2)
    | Ite (p1, p2, p3) -> Boolean.mk_ite ctx (aux p1) (aux p2) (aux p3)
    | Not p -> Boolean.mk_not ctx (aux p)
    | And ps -> Boolean.mk_and ctx (List.map aux ps)
    | Or ps -> Boolean.mk_or ctx (List.map aux ps)
    | Iff (p1, p2) -> Boolean.mk_iff ctx (aux p1) (aux p2) in
  aux prop

let sym_and = "&&"
let sym_or = "||"
let sym_not = "!"
let sym_implies = "==>"
let sym_iff = "<==>"

open Printf;;
let rec layout = function
  | True -> "true"
  | Bvar (_, b) -> b
  | MethodPredicate (mp, args) -> sprintf "%s(%s)" mp (List.split_by_comma snd args)
  | Implies (p1, p2) -> sprintf "(%s %s %s)" (layout p1) sym_implies (layout p2)
  | And ps -> sprintf "(%s)" (List.inner_layout (List.map layout ps) sym_and "true")
  | Or ps -> sprintf "(%s)" (List.inner_layout (List.map layout ps) sym_or "false")
  | Not p -> sprintf "(%s)" (sym_not^(layout p))
  | Iff (p1, p2) -> sprintf "(%s %s %s)" (layout p1) sym_iff (layout p2)
  | Ite (p1, p2, p3) ->
    sprintf "(ite %s %s %s)" (layout p1) (layout p2) (layout p3)

  let pretty_layout indent e =
    let mk_indent indent = String.init indent (fun _ -> ' ') in
    let rec is_display_level_lit = function
      | True | Bvar (_, _) | MethodPredicate (_, _) -> true
      | Not x -> is_display_level_lit x
      | _ -> false in
    let rec aux indent = function
      | True -> "true"
      | Bvar (_, b) -> b
      | MethodPredicate ("==", [a;b]) -> sprintf "(%s == %s)" (snd a) (snd b)
      | MethodPredicate ("<", [a;b]) -> sprintf "(%s < %s)" (snd a) (snd b)
      | MethodPredicate (mp, args) -> sprintf "%s(%s)" (MP.poly_name mp) (List.split_by_comma snd args)
      | Implies (p1, p2) ->
        if is_display_level_lit p2
        then
          sprintf "%s(%s %s %s)"
            (mk_indent indent) (aux 0 p1) sym_implies (aux 0 p2)
        else
      (* | Implies (MethodPredicate (mp1, args1), MethodPredicate (mp2, args2)) -> *)
      (*   sprintf "%s(%s %s %s)" *)
      (*     (mk_indent indent) (aux 0 (MethodPredicate (mp1, args1))) sym_implies (aux 0 (MethodPredicate (mp2, args2))) *)
      (* | Implies (p1, p2) -> *)
        sprintf "%s(\n%s %s \n%s %s\n%s)"
          (mk_indent indent) (aux (indent + 1) p1) sym_implies
          (mk_indent indent) (aux (indent + 1) p2) (mk_indent indent)
      | And [] -> raise @@ failwith "epr does not involve void conj"
      | And [p] -> aux indent p
      | And ps ->
        if List.for_all is_display_level_lit ps
        then
          sprintf "%s(%s)" (mk_indent indent) @@ List.split_by (sprintf " %s " sym_and) (aux 0) ps
        else
          sprintf "%s(\n%s\n%s)" (mk_indent indent)
            (List.inner_layout (List.map (aux (indent + 1)) ps) (" "^sym_and^"\n") "true")
            (mk_indent indent)
      | Or [] -> raise @@ failwith "epr does not involve void disconj"
      | Or [p] -> aux indent p
      | Or ps ->
        if List.for_all is_display_level_lit ps
        then
          sprintf "%s(%s)" (mk_indent indent) @@ List.split_by (sprintf " %s " sym_or) (aux 0) ps
        else
          sprintf "%s(\n%s\n%s)" (mk_indent indent)
            (List.inner_layout (List.map (aux (indent + 1)) ps) (" "^sym_or^"\n") "true")
            (mk_indent indent)
      | Not (MethodPredicate ("==", [a;b])) -> sprintf "(%s != %s)" (snd a) (snd b)
      | Not (MethodPredicate ("<", [a;b])) -> sprintf "(%s > %s)" (snd a) (snd b)
      | Not p -> sprintf "%s%s%s" (mk_indent indent) sym_not (aux 0 p)
      | Iff (p1, p2) ->
        sprintf "%s(%s %s \n%s)"
          (mk_indent indent) (aux 0 p1) sym_iff (aux (indent + 1) p2)
      | Ite (p1, p2, p3) ->
        sprintf "%s(ite%s\n%s\n%s)"
          (mk_indent indent) (aux 1 p1) (aux (indent + 4) p2) (aux (indent + 4) p3)
    in
    aux indent e

let pretty_layout_prop t = pretty_layout 0 t
