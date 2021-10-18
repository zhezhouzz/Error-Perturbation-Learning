val insert: int -> Unbset.t -> Unbset.t

let rec insert (x: int) (s: Unbset.t) =
  match s with
  |_ when Unbset.leaf -> Unbset.node Unbset.leaf x Unbset.leaf
  |_ when Unbset.node a y b ->
    if x < y then
      let tmp0: Unbset.t = insert y a in
      Unbset.node tmp0 x b
    else if y < x then
      let tmp0: Unbset.t = insert x b in
      Unbset.node a y tmp0
    else s

