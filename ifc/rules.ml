type label = L | H

let layout_label = function L -> "L" | H -> "H"

let label_eq (l1 : label) (l2 : label) : bool =
  match (l1, l2) with L, L -> true | H, H -> true | _, _ -> false

let label_join (l1 : label) (l2 : label) : label =
  match (l1, l2) with _, H -> H | H, _ -> H | _, _ -> L

let flows_to (l1 : label) (l2 : label) : bool =
  match (l1, l2) with L, _ -> true | _, H -> true | _, _ -> false

type lab = int

(* 1 ~ 3 represent variable name(index) *)
(* 0 represent PC *)

type rule_expr = L_Bot | L_Var of int | L_Join of rule_expr * rule_expr

(** Side conditions for rules: the Allow part *)
type rule_scond =
  | A_True
  | A_LE of rule_expr * rule_expr
  | A_And of rule_scond * rule_scond
  | A_Or of rule_scond * rule_scond

type allow_modify = {
  allow : rule_scond;
  labRes : rule_expr option;
  labResPC : rule_expr;
}

(** * Rule evaluation *)

(** * Rules Evaluation *)

(*(* eval_var is a mapping from abstract label names to concrete label
  values (a context).  The function [apply_rule] below uses this context
  to evaluate the rule ([AllowModify]).  *)
  let mk_eval_var (n:nat) (v1 v2 v3: option T) (pc: T) : lab n -> T =
    fun lv ->
      match lv with
          | lab1 -> v1
          | lab2 -> v2
          | lab3 -> v3
          | labpc -> Some pc
  ********)

let mk_eval_var (vs : label list) (pc : label) : lab -> label =
 fun lv -> if lv <= 1 then List.nth vs lv else pc

let rec eval_expr (eval_var : lab -> label) (e : rule_expr) : label =
  match e with
  | L_Bot -> L
  | L_Var labv -> eval_var labv
  | L_Join (e1, e2) ->
      label_join (eval_expr eval_var e1) (eval_expr eval_var e2)

(** eval_cond : evaluates a side_condition with given values for the argument *)
let rec eval_cond (eval_var : lab -> label) (c : rule_scond) : bool =
  match c with
  | A_True -> true
  | A_And (c1, c2) -> eval_cond eval_var c1 && eval_cond eval_var c2
  | A_Or (c1, c2) -> eval_cond eval_var c1 || eval_cond eval_var c2
  | A_LE (e1, e2) -> flows_to (eval_expr eval_var e1) (eval_expr eval_var e2)

(** apply_rule applies the allow-modify r to the given parameters.=
    Returns the (optional) result value label and result PC label,
    or nothing when the side condition fails. *)
let apply_rule (r : allow_modify) (vlabs : label list) (pclab : label) :
    (label option * label) option =
  let eval_var = mk_eval_var vlabs pclab in
  match eval_cond eval_var r.allow with
  | false -> None
  | true ->
      let rpc = eval_expr eval_var r.labResPC in
      let rres =
        match r.labRes with
        | Some lres -> Some (eval_expr eval_var lres)
        | None -> None
      in
      Some (rres, rpc)
