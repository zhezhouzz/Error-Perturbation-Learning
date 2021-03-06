val insert: int -> Unbset.t -> Unbset.t

let rec insert (x: int) (s: Unbset.t) =
  match s with
  |_ when Unbset.leaf -> Unbset.node x Unbset.leaf Unbset.leaf
  |_ when Unbset.node y a b ->
    if x < y then
      let tmp0: Unbset.t = insert y b in
      Unbset.node x a tmp0
    else if y < x then
      let tmp0: Unbset.t = insert x b in
      Unbset.node y a tmp0
    else s

