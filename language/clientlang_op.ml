module V = Primitive.Value;;
module Tp = Primitive.Tp;;
open Basic_dt;;
let ops = ["=="; "<"; ">"; "+"; "-"; "<="]

let known_op name =
  match List.find_opt (String.equal name) ops with
  | Some _ -> true
  | None -> false

let tp op =
  match op with
  | "==" | "<" | ">" | "<=" -> Some ([Tp.Int; Tp.Int;], [Tp.Bool])
  | "+" | "-" -> Some ([Tp.Int; Tp.Int;], [Tp.Int])
  | _ -> None

let eval op values =
  match op, values with
  | "==", [V.I i1; V.I i2] -> [V.B (i1 == i2)]
  | "<=", [V.I i1; V.I i2] -> [V.B (i1 <= i2)]
  | "<",  [V.I i1; V.I i2] -> [V.B (i1 < i2)]
  | ">",  [V.I i1; V.I i2] -> [V.B (i1 > i2)]
  | "+",  [V.I i1; V.I i2] -> [V.I (i1 + i2)]
  | "-",  [V.I i1; V.I i2] -> [V.I (i1 - i2)]
  | _ , _ ->
    raise @@ failwith (spf "unknown %s op %s(%s)" (Primitive.Tp.layout @@ V.get_tp @@ List.nth values 1) op @@ List.split_by_comma V.layout values)
