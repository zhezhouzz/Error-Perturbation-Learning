open Primitive;;
module E = Specification.Specast
module SE = Specification.Prop
module P = Method_predicate
module T = Tp
module V = Value

open Basic_dt
open Printf
type value = V.t
  type variable = string
  type t =
    | Pr of string * T.tvar list
    | Base of string *  T.tvar list
    | Bo of T.tvar
  type set = t list
let eval feature m =
  let find = StrMap.find "eval_feature" m in
  match feature with
  | Pr (pred, args) -> SE.eval (E.MethodPredicate (pred, args)) m
  | Base (op, args) -> SE.eval (E.MethodPredicate (op, args)) m
  | Bo x ->
    (match find (snd x) with
     | V.B b -> b
     | _ -> raise @@ failwith "eval_feature")

let eq a b =
  match a, b with
  | Pr (pred, args), Pr (pred', args') ->
    (String.equal pred pred') && (List.equal T.tvar_eq args args')
  | Base (op, args), Base (op', args') ->
    (String.equal op op') && (List.equal T.tvar_eq args args')
  | Bo a, Bo a' -> T.tvar_eq a a'
  | _ -> false

let layout = function
  | Pr (pred, args) ->
    sprintf "%s(%s)" pred (List.split_by_comma (fun (_, x) -> x) args)
  | Base (op, args) ->
    sprintf "%s(%s)" op (List.split_by_comma (fun (_, x) -> x) args)
  | Bo x -> snd x

let to_prop feature =
  match feature with
  | Pr (pred, args) -> E.MethodPredicate (pred, args)
  | Base (op, args) -> E.MethodPredicate (op, args)
  | Bo (t, b) -> E.Bvar (t, b)
