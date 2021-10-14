module Spec = Specification.Spec
module SpecAst = Specification.Specast
type asst =
  | NoPre of string * Spec.t
  | HasPre of string * Spec.t * string * Spec.t
open Primitive;;
open Basic_dt;;
module T = Tp;;
let client_pre client_name = spf "%s_pre" client_name
let client_post client_name = spf "%s_post" client_name

open Ocaml_parser;;
open Parsetree;;
open Printf;;
module L = Clientlang;;
let longident_to_string ld =
  let x =
    List.fold_left (fun res x ->
        match res with
        | None -> Some x
        | Some res -> Some (spf "%s.%s" res x)
      ) None (Longident.flatten ld)
  in
  match x with
  | None -> raise @@ failwith "longident_to_string"
  | Some x -> x

let pattern_to_string pattern =
  match pattern.ppat_desc with
  | Ppat_var ident -> ident.txt
  | _ ->
    printf "pattern_to_string\n";
    Pprintast.pattern Format.std_formatter pattern;
    raise @@ failwith "wrong pattern name, maybe untyped"

let type_reduction = function
  | "List.t" -> T.IntList
  | "Customstk.t" -> T.IntList
  | "Bankersq.t" -> T.IntList
  | "Batchedq.t" -> T.IntList
  | "Leftisthp.t" -> T.IntTreeI
  | "Rbset.t" -> T.IntTreeB
  | "Splayhp.t" -> T.IntTree
  | "Stream.t" -> T.IntList
  | "Trie.t" -> T.IntTree
  | "Trie.tp" -> T.IntList
  | "Unbset.t" -> T.IntTree
  | "Uniquel.t" -> T.IntList
  | "bool" -> T.Bool
  | "int" -> T.Int
  | tp -> failwith (spf "unknown type(%s)" tp)
let rawtp_to_tp (signame, sigtps) rawtp =
  match List.find_opt (fun sigtp -> String.equal rawtp sigtp) sigtps with
  | Some _ -> type_reduction @@ spf "%s.%s" signame rawtp
  | None -> type_reduction rawtp
(* let get_type tp = *)
(*   let aux = Pprintast.core_type *)
(*   match tp.ptyp_desc with *)
(*   | Ptyp_var tp -> type_reduction tp *)
(*   | Ptyp_package _ -> raise @@ failwith "package pattern type" *)
(*   | Ptyp_poly (ld, rest) -> raise @@ failwith "poly pattern type" *)
(*   | Ptyp_extension _ -> raise @@ failwith "extension pattern type" *)
(*   | Ptyp_class (_, _) -> raise @@ failwith "class pattern type" *)
(*   | Ptyp_any -> raise @@ failwith "any pattern type" *)
(*   | Ptyp_constr (const, tps) -> *)
(*     if List.length tps != 0 then raise @@ failwith "not imp get_type" else *)
(*       type_reduction (longident_to_string const.txt) *)
(*   | _ -> raise @@ failwith "wrong pattern type" *)

let get_type tp =
  let tp_str =
  ignore (Format.flush_str_formatter ()) ;
  let f = Format.str_formatter in
  Pprintast.core_type f tp;
  Format.flush_str_formatter ()
  in
  type_reduction tp_str
let pattern_to_typedvar pattern =
  match pattern.ppat_desc with
  | Ppat_var name -> None, name.txt
  | Ppat_constraint (ident, tp) ->
    Some (get_type tp), pattern_to_string ident
  | _ ->
    printf "pattern_to_typedvar\n";
    Pprintast.pattern Format.std_formatter pattern;
    raise @@ failwith "wrong pattern name"
let pattern_to_topt_tuple pattern =
  match pattern.ppat_desc with
  | Ppat_tuple ps -> List.map pattern_to_typedvar ps
  | _ -> [pattern_to_typedvar pattern]

let pattern_to pattern =
  match pattern.ppat_desc with
  | Ppat_tuple ps -> List.map pattern_to_typedvar ps
  | _ -> [pattern_to_typedvar pattern]

let parse_func_args expr =
  let rec aux args expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, arg, expr) ->
      aux (args @ [arg]) expr
    | _ -> (args, expr)
  in
  let rawargs, body = aux [] expr in
  let args = List.map (fun pattern ->
      let tp, name = pattern_to_typedvar pattern in
      match tp with
      | None -> raise @@ failwith "args in function definition need type"
      | Some tp -> tp, name
    ) rawargs in
  args, body


let expr_to_name expr =
  match expr.pexp_desc with
  | Pexp_ident ld -> longident_to_string ld.txt
  | _ -> raise @@ failwith (sprintf "expr_to_name(%s)" (Pprintast.string_of_expression expr))

(* TODO: do not infer type here *)
let expr_to_vname expr =
  match expr.pexp_desc with
  | Pexp_ident ld -> longident_to_string ld.txt
  | Pexp_constraint (expr, _) ->
    (match expr.pexp_desc with
     | Pexp_ident ld -> longident_to_string ld.txt
     | _ -> raise @@ failwith "only add type denotation to variable"
    )
  | Pexp_constant _ -> raise @@ failwith "do not support complicate literal"
  | _ ->
    let _ = Pprintast.expression Format.std_formatter expr in
    raise @@ failwith "expr_to_vname"

let body_of_ocamlast expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_tuple es -> L.VarTuple (
        List.map (fun e -> try expr_to_name e with _ -> raise @@ failwith "parsing: tuple can only contains variables") es
      )
    | Pexp_constraint (expr, _) -> aux expr
      (* let _ = Printf.printf "constr: %s\n" (Pprintast.string_of_expression expr) in *)
      (* L.VarTuple ([expr_to_name expr]) *)
    | Pexp_ident _ ->
      (* let _ = Printf.printf "ident: %s\n" (Pprintast.string_of_expression expr) in *)
      L.VarTuple ([expr_to_name expr])
    | Pexp_construct (id, None) ->
      let lit =
        match longident_to_string id.txt with
        | "true" -> L.LitB true
        | "false" -> L.LitB false
        | _ -> raise @@ failwith "do not support complicate literal"
      in
      lit
    | Pexp_constant (Pconst_integer (istr, None)) -> L.LitI (int_of_string istr)
    | Pexp_let (_, vbs, e) ->
      List.fold_right (fun vb body ->
          let leftvars = List.map (fun (topt, name) ->
              match topt with
              | None -> raise @@ failwith "let binding must have type"
              | Some tp -> (tp, name)
            ) @@ pattern_to_topt_tuple vb.pvb_pat in
          L.Let (leftvars, aux vb.pvb_expr, body)
        ) vbs (aux e)
    | Pexp_apply (func, args) ->
      (* let _ = Printf.printf "func: %s\n" (Pprintast.string_of_expression func) in *)
      let funcname = expr_to_name func in
      let args = List.map (fun (_, e) -> try expr_to_name e with _ ->
          raise @@ failwith "parsing: application can only contains variables") args in
      if Clientlang_op.known_op funcname
      then L.Op (funcname, args)
      else L.App(funcname, args)
    | Pexp_ifthenelse (e1, e2, Some e3) -> L.Ift (aux e1, aux e2, aux e3)
    | Pexp_ifthenelse (_, _, None) -> raise @@ failwith "no else branch in ite"
    | Pexp_match (case_target, cases) ->
      let handle_match_args match_arg =
        let aux e =
          match e.pexp_desc with
          | Pexp_ident ld -> longident_to_string ld.txt
          | _ -> failwith "parser: wrong format in match"
        in
        aux match_arg
      in
      let case_target = handle_match_args case_target in
      let handle_case case =
        match case.pc_guard with
        | None ->
          printf "%s\n" @@ Pprintast.string_of_expression case.pc_rhs;
          failwith "handle_case"
        | Some guard -> guard, case.pc_rhs
      in
      let cs = List.map (fun case ->
          let case_e, body = handle_case case in
          match aux case_e with
          | L.VarTuple [funcname] -> (funcname, [], aux body)
          | L.App (funcname, args) -> (funcname, args, aux body)
          | _ -> failwith "parser: wrong format in match"
        ) cases in
      L.Match (case_target, cs)
    | _ -> raise @@ failwith (spf "not imp client parsing:\n%s" @@ Pprintast.string_of_expression expr )
  in
  aux expr

let structure_to_signagture struc =
  match struc.pstr_desc with
  | Pstr_modtype m ->
    let mname = m.pmtd_name.txt in
    (match m.pmtd_type with
     | Some mt ->
       (match mt.pmty_desc with
        | Pmty_signature signature ->
          mname, signature
        | _ -> raise @@ failwith "not a struct")
     | _ -> raise @@ failwith "not a struct")
  | _ -> raise @@ failwith "not a struct"

let solve_functype ct =
  let rec parse_currying previous ct =
    match ct.ptyp_desc with
    | Ptyp_arrow (_, t1, t2) ->
      parse_currying (previous @ [t1]) t2
    | _ -> previous, ct
  in
  let argtps, rettp = parse_currying [] ct in
  let rec aux ct =
    match ct.ptyp_desc with
    | Ptyp_constr (tpc, cts) ->
      if not (List.length cts == 0)
      then
        raise @@ failwith "solve_functype constr"
      else
        [longident_to_string tpc.txt]
    | Ptyp_tuple cts -> List.flatten @@ List.map aux cts
    | Ptyp_var tp -> [tp]
    | Ptyp_arrow (_, _, _) -> raise @@ failwith "solve_functype: function type"
    | _ -> raise @@ failwith "solve_functype"
  in
  List.flatten @@ List.map aux argtps, aux rettp

let vd_to_tpedvars vd =
  let funcname = vd.pval_name.txt in
  let tp = solve_functype vd.pval_type in
  (* let () = printf "%s -> %s\n" (List.to_string T.layout (fst tp)) (List.to_string T.layout (snd tp)) in *)
  funcname, tp

let structure_to_vd struc =
  match struc.pstr_desc with
  | Pstr_primitive vd -> vd
  | _ -> raise @@ failwith "structure_to_vd"

let layout_funcm funcm =
  StrMap.iter (fun name (argtps, rettp) ->
      printf "val %s(%s) => (%s)\n" name (List.to_string T.layout argtps) (List.to_string T.layout rettp)
    ) funcm

let parse_propositional_term tenv expr =
  let vars_to_tvars tenv args =
    List.map (fun e ->
        let name = expr_to_name e in
        match StrMap.find_opt tenv name with
        | Some tp -> (tp, name)
        | None -> raise @@ failwith (spf "parsing assertion: unbounded variable %s" name))
      args
  in
  let handle_logic func args =
    let targs = vars_to_tvars tenv args in
    SpecAst.MethodPredicate (func, targs)
  in
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_constant (Pconst_string ("true", _ , None)) -> SpecAst.True
    | Pexp_constant (Pconst_string ("false", _ , None)) -> SpecAst.Not SpecAst.True
    | Pexp_constant _ -> raise @@ failwith "do not support complicate literal"
    | Pexp_apply (func, args) ->
      (* let _ = Printf.printf "prop-func: %s\n" (Pprintast.string_of_expression func) in *)
      let funcname = expr_to_name func in
      let args = List.map snd args in
      (match funcname, args with
       | "&&", [a; b] -> SpecAst.And [aux a; aux b]
       | "||", [a; b] -> SpecAst.Or [aux a; aux b]
       | "implies", [a; b] -> SpecAst.Implies (aux a, aux b)
       | "iff", [a; b] -> SpecAst.Iff (aux a, aux b)
       | _, _ -> handle_logic funcname args
      )
    | Pexp_construct (id, None) ->
      (match longident_to_string id.txt with
       | "true" -> SpecAst.True
       | "false" -> SpecAst.Not SpecAst.True
       | _ -> raise @@ failwith "do not support complicate literal")
    | Pexp_construct (_, Some _) ->  raise @@ failwith "Pexp_construct"
    | _ -> raise @@ failwith "parse_propositional_term"
  in
  aux expr

let parse_assertion client_name inputargs restps asts =
  (* let ppf = Format.std_formatter in *)
  let get_meta p =
    match p.pstr_desc with
    | Pstr_value (_, [value_binding]) -> value_binding.pvb_expr
    | _ -> raise @@ failwith "parse_assertion::get_meta"
  in
  let get_strings expr =
    (match expr.pexp_desc with
     | Pexp_array es ->
       List.map (fun e ->
           match e.pexp_desc with
           | Pexp_constant (Pconst_string (c, _, None)) -> c
           | _ -> raise @@ failwith "parse_assertion::strings"
         ) es
     | _ -> raise @@ failwith "parse_assertion::strings")
  in
  let get_int expr =
    (match expr.pexp_desc with
     | Pexp_constant (Pconst_integer (istr, None)) -> int_of_string istr
     | _ -> raise @@ failwith "parse_assertion::get_int")
  in
  let get_assertion argtps p =
    match p.pstr_desc with
    | Pstr_value (_, [value_binding]) ->
      let expr = value_binding.pvb_expr in
      let args_, body = parse_func_args expr in
      (* let _ = Printf.printf "args: %s\n" (List.split_by_comma Tp.layouttvar args_) in *)
      let tenv = List.fold_left (fun tenv (tp, name) ->
          StrMap.add name tp tenv
        ) StrMap.empty args_ in
      let args = List.sublist args_ (0, List.length argtps) in
      let qv = List.sublist args_ (List.length argtps, List.length args_) in
      let _ = if List.exists (fun ((tp, _), tp') -> not (Tp.eq tp tp'))
          @@ List.combine args argtps
        then
          raise @@ failwith "unmatched assertion variables"
        else ()
      in
      let body = Specification.Prop.desugar @@ parse_propositional_term tenv body in
      let spec = Spec.({args; qv; body}) in
      spec
    | _ -> raise @@ failwith "translate not an assertion"
  in
  let preds = get_strings @@ get_meta @@ List.nth asts 0 in
  let op_pool = get_strings @@ get_meta @@ List.nth asts 1 in
  let libs = get_strings @@ get_meta @@ List.nth asts 2 in
  let sampling_rounds = get_int @@ get_meta @@ List.nth asts 3 in
  let p_size  = get_int @@ get_meta @@ List.nth asts 4 in
  let asst =
    if List.length asts == 7 then
      let pre = get_assertion (List.map fst inputargs) @@ List.nth asts 5 in
      let post = get_assertion restps @@ List.nth asts 6 in
       HasPre (client_pre client_name, pre, client_post client_name, post)
    else if List.length asts == 6 then
      let post = get_assertion restps @@ List.nth asts 5 in
      NoPre (client_post client_name, post)
    else
      raise @@ failwith "assertions wrong format"
  in
  preds, op_pool, libs, sampling_rounds, p_size, asst

let parse_client client =
  match client.pstr_desc with
  | Pstr_value (_, [value_binding]) ->
    let expr = value_binding.pvb_expr in
    (* let _ = Pprintast.expression ppf expr in *)
    let args, body = parse_func_args expr in
    args, body_of_ocamlast body
  | _ -> raise @@ failwith "translate not a function value"

let of_ocamlast source_client source_assertions =
  if List.length source_client != 2 then
    raise @@ failwith "source wrong format"
  else
    let clienttp = List.nth source_client 0 in
    let client = List.nth source_client 1 in
    let client_name, (_, outt) = vd_to_tpedvars @@ structure_to_vd clienttp in
    let outt = List.map type_reduction outt in
    let args, client = parse_client client in
    let preds, op_pool, libs, sampling_rounds, p_size, asst = parse_assertion client_name args outt source_assertions in
    let sigma, phi = match asst with
      | NoPre (_, phi) ->
        let sigma = Spec.({args; qv = []; body = SpecAst.True}) in
        sigma, phi
      | HasPre (_, sigma, _, phi) -> sigma, phi
    in
    let tps = List.map fst args in
    let client_func = Clientlang.({fname = client_name; args = args; body = client; res = outt}) in
    preds, sigma, client_func, libs, phi, tps, op_pool, sampling_rounds, p_size

