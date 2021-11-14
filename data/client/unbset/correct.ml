val insert : int -> Unbset.t -> Unbset.t

let rec insert (x : int) (s : Unbset.t) =
  match s with
  | _ when Unbset.leaf -> Unbset.node x Unbset.leaf Unbset.leaf
  | _ when Unbset.node y a b ->
      if x < y then Unbset.node y (insert x a) b
      else if y < x then Unbset.node y a (insert x b)
      else s
