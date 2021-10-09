type label = Pos | Neg | MayNeg

let layout_label = function
  | Pos -> "+"
  | Neg -> "-"
  | MayNeg -> "o"

let is_pos = function
  | Pos -> true
  | _ -> false

let eq_label a b=
  let aux = function
  | Pos, Pos -> true
  | Neg, Neg -> true
  | MayNeg, MayNeg -> true
  | _, _ -> false
  in
  aux (a, b)
