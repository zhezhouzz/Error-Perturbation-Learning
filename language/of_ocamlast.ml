open Primitive
open Basic_dt
module T = Tp
module V = Value
open Ocaml_parser
open Parsetree
open Printf

(* aux *)
let longident_to_string ld =
  let x =
    List.fold_left
      (fun res x ->
        match res with None -> Some x | Some res -> Some (spf "%s.%s" res x))
      None (Longident.flatten ld)
  in
  match x with None -> raise @@ failwith "longident_to_string" | Some x -> x

let pattern_to_string pattern =
  match pattern.ppat_desc with
  | Ppat_var ident -> ident.txt
  | _ ->
      printf "pattern_to_string\n";
      Pprintast.pattern Format.std_formatter pattern;
      raise @@ failwith "wrong pattern name, maybe untyped"

(*Type*)
let type_reduction tpname =
  let tpname' = try Bblib.underline_type_reduction tpname with _ -> tpname in
  Tp.of_string tpname'

let type_of_ocamltype tp =
  let tp_str =
    ignore (Format.flush_str_formatter ());
    let f = Format.str_formatter in
    Pprintast.core_type f tp;
    Format.flush_str_formatter ()
  in
  type_reduction tp_str

(* Literal *)
let rec lit_of_ocamlexpr e =
  let mk_exn constructor args =
    failwith
      (spf "do not support complicate literal %s --> [%s] --"
         (Pprintast.string_of_expression constructor)
         (List.split_by_comma V.layout args))
  in
  match e.pexp_desc with
  | Pexp_tuple es -> List.flatten @@ List.map lit_of_ocamlexpr es
  | Pexp_construct (id, e) -> (
      match (longident_to_string id.txt, e) with
      | "true", None -> [ V.B true ]
      | "false", None -> [ V.B false ]
      | "Leaf", None -> [ V.T Tree.Leaf ]
      | "LILeaf", None -> [ V.TI LabeledTree.Leaf ]
      | "LBLeaf", None -> [ V.TB LabeledTree.Leaf ]
      | "()", None -> [ V.U ]
      | "Empty", None -> [ V.U ]
      | "[]", None -> [ V.L [] ]
      | "NodeS", Some e -> (
          match lit_of_ocamlexpr e with
          | [ V.I x ] -> [ V.T (Tree.Node (x, Leaf, Leaf)) ]
          | k -> raise @@ mk_exn e k)
      | "Node", Some e -> (
          match lit_of_ocamlexpr e with
          | [ V.I x; V.T a; V.T b ] -> [ V.T (Tree.Node (x, a, b)) ]
          | k -> raise @@ mk_exn e k)
      | "LNode", Some e -> (
          match lit_of_ocamlexpr e with
          | [ V.B label; V.I x; V.TB a; V.TB b ] ->
              [ V.TB (LabeledTree.Node (label, x, a, b)) ]
          | [ V.I label; V.I x; V.TI a; V.TI b ] ->
              [ V.TI (LabeledTree.Node (label, x, a, b)) ]
          | k -> raise @@ mk_exn e k)
      | "::", Some e -> (
          match lit_of_ocamlexpr e with
          | [ V.I hd; V.L tl ] -> [ V.L (hd :: tl) ]
          | k -> raise @@ mk_exn e k)
      | x, None ->
          raise @@ failwith (spf "do not support complicate literal(%s) --" x)
      | x, Some es ->
          raise
          @@ failwith
               (spf "do not support complicate literal %s with %s --" x
               @@ Pprintast.string_of_expression es))
  | Pexp_constant (Pconst_integer (istr, None)) -> [ V.I (int_of_string istr) ]
  | Pexp_constant _ ->
      raise
      @@ failwith
           (spf "do not support complicate constant(%s) --"
           @@ Pprintast.string_of_expression e)
  | _ ->
      raise
      @@ failwith
           (spf "do not support impure literal(%s) --"
              (Pprintast.string_of_expression e))

(* Args *)
let pattern_to_typedvar pattern =
  match pattern.ppat_desc with
  | Ppat_var name -> (None, name.txt)
  | Ppat_constraint (ident, tp) ->
      (Some (type_of_ocamltype tp), pattern_to_string ident)
  | _ ->
      printf "pattern_to_typedvar\n";
      Pprintast.pattern Format.std_formatter pattern;
      raise @@ failwith "wrong pattern name"

let pattern_to_topt_tuple pattern =
  match pattern.ppat_desc with
  | Ppat_tuple ps -> List.map pattern_to_typedvar ps
  | _ -> [ pattern_to_typedvar pattern ]

(* Expr *)
module L = Tinyocaml

let expr_of_ocamlexpr expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_tuple es -> L.Tuple (List.map aux es)
    | Pexp_constraint (expr, _) -> aux expr
    | Pexp_ident id -> L.Var (longident_to_string id.txt)
    | Pexp_construct (_, _) | Pexp_constant _ -> Lit (lit_of_ocamlexpr expr)
    | Pexp_let (_, vbs, e) ->
        List.fold_right
          (fun vb body ->
            let leftvars =
              List.map (fun (topt, name) ->
                  match topt with
                  | None -> raise @@ failwith "let binding must have type"
                  | Some tp -> (tp, name))
              @@ pattern_to_topt_tuple vb.pvb_pat
            in
            L.Let (leftvars, aux vb.pvb_expr, body))
          vbs (aux e)
    | Pexp_apply (func, args) ->
        let funcname =
          match aux func with
          | L.Var x -> x
          | _ -> raise @@ failwith "Can only allow function name as function"
        in
        let args = List.map (fun x -> aux @@ snd x) args in
        if Clientlang_op.known_op funcname then L.Op (funcname, args)
        else L.App (funcname, args)
    | Pexp_ifthenelse (e1, e2, Some e3) -> L.Ift (aux e1, aux e2, aux e3)
    | Pexp_ifthenelse (_, _, None) -> raise @@ failwith "no else branch in ite"
    | Pexp_match (case_target, cases) ->
        let handle_match_args match_arg =
          let e = aux match_arg in
          let rec aux e =
            match e with
            | L.Var var -> [ var ]
            | L.Tuple vars -> List.flatten @@ List.map aux vars
            | _ -> failwith "parser: wrong format in match"
          in
          aux e
        in
        let case_target = handle_match_args case_target in
        let handle_case case =
          match case.pc_guard with
          | None ->
              printf "%s\n" @@ Pprintast.string_of_expression case.pc_rhs;
              failwith "handle_case"
          | Some guard -> (guard, case.pc_rhs)
        in
        let cs =
          List.map
            (fun case ->
              let case_e, body = handle_case case in
              (aux case_e, aux body))
            cases
        in
        L.Match (case_target, cs)
    | _ ->
        raise
        @@ failwith
             (spf "not imp client parsing:%s"
             @@ Pprintast.string_of_expression expr)
  in
  aux expr

(* Prop *)
module Spec = Specification.Spec
module SpecAst = Specification.Specast

let prop_of_ocamlexpr tenv expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_constant (Pconst_string ("true", _, None)) -> SpecAst.True
    | Pexp_constant (Pconst_string ("false", _, None)) ->
        SpecAst.Not SpecAst.True
    | Pexp_constant _ -> raise @@ failwith "do not support complicate literal"
    | Pexp_construct (id, None) -> (
        match longident_to_string id.txt with
        | "true" -> SpecAst.True
        | "false" -> SpecAst.Not SpecAst.True
        | _ -> raise @@ failwith "do not support complicate literal")
    | Pexp_construct (_, Some _) -> raise @@ failwith "Pexp_construct"
    | Pexp_apply (func, args) -> (
        let funcname =
          match expr_of_ocamlexpr func with
          | L.Var x -> x
          | _ -> raise @@ failwith "Can only allow function name as function"
        in
        let args = List.map snd args in
        match (funcname, args) with
        | "&&", [ a; b ] -> SpecAst.And [ aux a; aux b ]
        | "||", [ a; b ] -> SpecAst.Or [ aux a; aux b ]
        | "implies", [ a; b ] -> SpecAst.Implies (aux a, aux b)
        | "iff", [ a; b ] -> SpecAst.Iff (aux a, aux b)
        | "not", [ a ] -> SpecAst.Not (aux a)
        | _ ->
            let targs =
              List.map
                (fun e ->
                  let name =
                    match expr_of_ocamlexpr e with
                    | L.Var x -> x
                    | x ->
                        raise
                        @@ failwith
                             (spf
                                "Method predicate does not allow nested \
                                 predicates %s(%s)"
                                funcname
                             @@ Clientlang.layout_body x)
                  in
                  match StrMap.find_opt tenv name with
                  | Some tp -> (tp, name)
                  | None ->
                      raise
                      @@ failwith
                           (spf "parsing assertion: unbounded variable %s" name))
                args
            in
            SpecAst.MethodPredicate (funcname, targs))
    | _ -> raise @@ failwith "parse_propositional_term"
  in
  aux expr

(* Function *)
let args_body_of_ocamlexpr expr =
  let rec aux args expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, arg, expr) -> aux (args @ [ arg ]) expr
    | _ -> (args, expr)
  in
  let rawargs, body = aux [] expr in
  let args =
    List.map
      (fun pattern ->
        let tp, name = pattern_to_typedvar pattern in
        match tp with
        | None -> raise @@ failwith "args in function definition need type"
        | Some tp -> (tp, name))
      rawargs
  in
  (args, body)

(* Client Code *)
type client = {
  clientname : string;
  clientargs : T.tvar list;
  clientbody : Tinyocaml.t;
}

let client_of_ocamlstruct structure =
  match structure.pstr_desc with
  | Pstr_value (_, [ value_binding ]) ->
      let clientname = pattern_to_string value_binding.pvb_pat in
      let clientargs, clientbody =
        args_body_of_ocamlexpr value_binding.pvb_expr
      in
      let clientbody = expr_of_ocamlexpr clientbody in
      { clientname; clientargs; clientbody }
  | _ -> raise @@ failwith "translate not a function value"

(* Assertion *)
type assertion = {
  assertionname : string;
  assertionargs : T.tvar list;
  assertionbody : SpecAst.t;
}

let assertion_of_ocamlstruct structure =
  match structure.pstr_desc with
  | Pstr_value (_, [ value_binding ]) ->
      let assertionname = pattern_to_string value_binding.pvb_pat in
      let assertionargs, assertionbody =
        args_body_of_ocamlexpr value_binding.pvb_expr
      in
      let tenv =
        List.fold_left
          (fun tenv (tp, name) -> StrMap.add name tp tenv)
          StrMap.empty assertionargs
      in
      let assertionbody =
        Specification.Prop.desugar @@ prop_of_ocamlexpr tenv assertionbody
      in
      { assertionname; assertionargs; assertionbody }
  | _ -> raise @@ failwith "translate not a function value"

(* Meta *)
let get_p asts idx =
  try List.nth asts idx
  with _ -> raise @@ failwith (spf "missing %ith structure" idx)

let get_meta name asts idx =
  let p = get_p asts idx in
  let name', expr =
    match p.pstr_desc with
    | Pstr_value (_, [ value_binding ]) ->
        (pattern_to_string value_binding.pvb_pat, value_binding.pvb_expr)
    | _ -> raise @@ failwith "parse_meta: wrong format"
  in
  if not (String.equal name name') then
    raise @@ failwith (spf "parse_meta: %s is expected" name)
  else expr

let load_meta asts =
  let get_strings expr =
    match expr.pexp_desc with
    | Pexp_array es ->
        List.map
          (fun e ->
            match e.pexp_desc with
            | Pexp_constant (Pconst_string (c, _, None)) -> c
            | _ -> raise @@ failwith "parse_assertion::strings")
          es
    | _ -> raise @@ failwith "parse_assertion::strings"
  in
  let get_int expr =
    match expr_of_ocamlexpr expr with
    | Lit [ V.I i ] -> i
    | _ -> raise @@ failwith "parse_meta: int value is expected"
  in
  let get_lit expr =
    let rec aux = function
      | L.Lit lit -> lit
      | Tuple es -> List.flatten @@ List.map aux es
      | x ->
          raise
          @@ failwith
               (spf "parse_meta: literal is expected, but get %s"
                  (Clientlang.layout_body x))
    in
    aux @@ expr_of_ocamlexpr expr
  in
  let preds = get_strings @@ get_meta "preds" asts 0 in
  let op_pool = get_strings @@ get_meta "op_pool" asts 1 in
  let libs = get_strings @@ get_meta "libs" asts 2 in
  let i_err = get_lit @@ get_meta "i_err" asts 3 in
  let sampling_rounds = get_int @@ get_meta "sampling_rounds" asts 4 in
  let p_size = get_int @@ get_meta "p_size" asts 5 in
  let assertion1 = assertion_of_ocamlstruct @@ get_p asts 6 in
  match assertion1.assertionname with
  | "pre" ->
      let assertion2 = assertion_of_ocamlstruct @@ get_p asts 7 in
      ( preds,
        op_pool,
        libs,
        i_err,
        sampling_rounds,
        p_size,
        assertion1,
        assertion2 )
  | "post" ->
      ( preds,
        op_pool,
        libs,
        i_err,
        sampling_rounds,
        p_size,
        {
          assertionname = "pre";
          assertionargs = [];
          assertionbody = SpecAst.True;
        },
        assertion1 )
  | _ -> raise @@ failwith "parse_meta: pre/post are expected"

let slipt_args argtps assertionargs =
  if List.length assertionargs < List.length argtps then
    raise
    @@ failwith
         (spf
            "the length of args of client(%i) does not match the args of the \
             assertions(%i)"
            (List.length assertionargs)
            (List.length argtps))
  else
    let args' = List.sublist assertionargs (0, List.length argtps) in
    let rest =
      List.sublist assertionargs (List.length argtps, List.length assertionargs)
    in
    let _ =
      if
        List.exists (fun ((tp, _), tp') -> not (Tp.eq tp tp'))
        @@ List.combine args' argtps
      then
        raise
        @@ failwith
             (sprintf "unmatched assertion variables [%s] vs. [%s]"
                (List.split_by_comma Tp.layouttvar args')
                (List.split_by_comma Tp.layout argtps))
      else ()
    in
    (args', rest)

let pre_to_spec args { assertionargs; assertionbody; _ } =
  match assertionbody with
  | SpecAst.True ->
      (* When the preconditino is omitted *)
      Spec.{ args; qv = []; body = SpecAst.True }
  | _ ->
      let specargs, qv = slipt_args (List.map fst args) assertionargs in
      Spec.{ args = specargs; qv; body = assertionbody }

let post_to_spec args rettps { assertionargs; assertionbody; _ } =
  (* let () = Printf.printf "assertion name = %s\n" assertionname in *)
  let specargs, rest = slipt_args (List.map fst args) assertionargs in
  let specargs', qv = slipt_args rettps rest in
  Spec.{ args = specargs @ specargs'; qv; body = assertionbody }

let load_rettps struc =
  match struc.pstr_desc with
  | Pstr_primitive vd ->
      let rec parse_currying previous ct =
        match ct.ptyp_desc with
        | Ptyp_arrow (_, t1, t2) -> parse_currying (previous @ [ t1 ]) t2
        | _ -> (previous, ct)
      in
      let _, rettp = parse_currying [] vd.pval_type in
      let rec aux ct =
        match ct.ptyp_desc with
        | Ptyp_constr (tpc, cts) ->
            if not (List.length cts == 0) then
              raise @@ failwith "solve_functype constr"
            else [ longident_to_string tpc.txt ]
        | Ptyp_tuple cts -> List.flatten @@ List.map aux cts
        | Ptyp_var tp -> [ tp ]
        | Ptyp_arrow (_, _, _) ->
            raise @@ failwith "solve_functype: function type"
        | _ -> raise @@ failwith "solve_functype"
      in
      aux rettp
  | _ -> raise @@ failwith "wrong client types"

(* Env *)

let load_client_and_meta source_client source_assertions =
  (* to get the return type *)
  let rettps =
    List.map type_reduction @@ load_rettps @@ get_p source_client 0
  in
  let { clientname; clientargs; clientbody } =
    client_of_ocamlstruct @@ get_p source_client 1
  in
  let preds, op_pool, libs, i_err, sampling_rounds, p_size, sigma, phi =
    load_meta source_assertions
  in
  let sigma = pre_to_spec clientargs sigma in
  let phi = post_to_spec clientargs rettps phi in
  let tps = List.map fst clientargs in
  (* let () = Printf.printf "args: %s\n" @@ List.split_by_comma Tp.layouttvar args in *)
  let client_func =
    L.{ fname = clientname; args = clientargs; body = clientbody; res = rettps }
  in
  ( sigma,
    client_func,
    libs,
    i_err,
    phi,
    tps,
    op_pool,
    preds,
    sampling_rounds,
    p_size )

(* Perturbation Function Precondition *)
let load_precondition args source_assertion =
  let assertion = assertion_of_ocamlstruct @@ get_p source_assertion 0 in
  pre_to_spec args assertion

(* Perturbation Function Precondition *)
let load_literal source =
  let { clientname; clientbody; _ } = client_of_ocamlstruct @@ get_p source 0 in
  (clientname, Clientlang.to_lits clientbody)
