module V = Primitive.Value;;
open Basic_dt;;
let ops = ["=="; "<"; ">"; "+"; "-"]

let known_op name =
  match List.find_opt (String.equal name) ops with
  | Some _ -> true
  | None -> false

let eval op values =
  match op, values with
  | "==", [V.I i1; V.I i2] -> [V.B (i1 == i2)]
  | "<",  [V.I i1; V.I i2] -> [V.B (i1 < i2)]
  | ">",  [V.I i1; V.I i2] -> [V.B (i1 > i2)]
  | _ , _ -> raise @@ failwith (spf "unknown op %s" op)
