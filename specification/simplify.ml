open Specast;;
open Primitive;;
let eq a b =
  let rec aux a b =
    match (a, b) with
    | True, True -> true
    | Bvar (t1, b1), Bvar (t2, b2) -> Tp.tvar_eq (t1, b1) (t2, b2)
    | MethodPredicate (mp1, args1), MethodPredicate (mp2, args2) ->
      String.equal mp1 mp2 &&
      List.equal Tp.tvar_eq args1 args2
    | Implies (p1, p2), Implies (p1', p2') -> (aux p1 p1') && (aux p2 p2')
    | And ps1, And ps2 -> List.for_all2 aux ps1 ps2
    | Or ps1, Or ps2 -> List.for_all2 aux ps1 ps2
    | Not p1, Not p2 -> aux p1 p2
    | Iff (p1, p2), Iff (p1', p2') -> (aux p1 p1') && (aux p2 p2')
    | Ite (p1, p2, p3), Ite (p1', p2', p3') -> (aux p1 p1') && (aux p2 p2') && (aux p3 p3')
    | _ -> false
  in
  aux a b


let simplify_ite a =
  let desugar_ite (p1, p2, p3) =
    And [Implies (p1, p2); Implies (Not p1, p3)] in
  let simp_ite p1 p2 p3 =
    match p2, p3 with
    | True, True -> True
    | True, Not True -> p1
    | True, p3 -> Implies (Not p1, p3)
    | Not True, True -> Not p1
    | Not True, Not True -> Not True
    | Not True, p3 -> And [Not p1; p3]
    | p2, True -> Implies (p1, p2)
    | p2, Not True -> And [p1; p2]
    | x1, Not x2 ->
      if eq x1 x2
      then Iff (p1, x1)
      else desugar_ite (p1, p2, p3)
    | _ -> desugar_ite (p1, p2, p3)
  in
  let rec aux a =
    match a with
    | MethodPredicate (_, _) | True | Bvar (_, _) -> a
    | Implies (p1, p2) -> aux (Or [Not p1; p2])
    | Ite (p1, p2, p3) -> simp_ite (aux p1) (aux p2) (aux p3)
    | Not p -> Not (aux p)
    | And ps ->
      And (List.filter_map (function
          | True -> None
          | p -> Some p) (List.map aux ps))
    | Or ps ->
      let ps = List.map aux ps in
      if List.exists (function True -> true | _ -> false) ps
      then True
      else Or ps
    | Iff (p1, p2) ->
      let p1, p2 = Sugar.map2 aux (p1, p2) in
      if eq p1 p2 then True else Iff (p1, p2)
  in
  let rec simplify_same = function
    | Implies(p1, Implies(p2, p3)) -> simplify_same (Implies(And[p1;p2], p3))
    | Implies(p1, p2) -> Implies(simplify_same p1, simplify_same p2)
    | Ite (_, _, _) -> raise @@ failwith "simplify_same"
    | MethodPredicate (_, _) as x -> x
    | Not(Not(p)) -> simplify_same p
    | Not(p) -> Not(simplify_same p)
    | And ((And hs) :: tl) -> And (hs @ tl)
    | Or ((And hs) :: tl) -> Or (hs @ tl)
    | And ps ->
      And (List.filter_map (function
          | True -> None
          | p -> Some p) (List.map simplify_same ps))
    | Or ps -> Or (List.map simplify_same ps)
    | Iff (p1, p2) ->
      let p1, p2 = simplify_same p1, simplify_same p2 in
      if eq p1 p2 then True else Iff (p1, p2)
    | True -> True
    | Bvar (_, _) as a -> a
  in
  (simplify_same (aux a))
let forallformula_simplify_ite (fv, e) = fv, simplify_ite e
