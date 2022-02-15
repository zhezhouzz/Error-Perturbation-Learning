val balance : bool -> int -> Rbset.t -> Rbset.t -> Rbset.t

let rec balance (r : bool) (x : int) (tree1 : Rbset.t) (tree2 : Rbset.t) =
  match (r, x, tree1, tree2) with
  | _
    when ( false,
           (z : int),
           Rbset.node true
             (Rbset.node true (a : Rbset.t) (x : int) (b : Rbset.t))
             (y : int)
             (c : Rbset.t),
           (d : Rbset.t) ) ->
      Rbset.node true y (Rbset.node false x a b) (Rbset.node false z c d)
  | _
    when ( false,
           (z : int),
           Rbset.node true
             (a : Rbset.t)
             (x : int)
             (Rbset.node true (b : Rbset.t) (y : int) (c : Rbset.t)),
           (d : Rbset.t) ) ->
      Rbset.node true y (Rbset.node false x a b) (Rbset.node false z c d)
  | _
    when ( false,
           (x : int),
           (a : Rbset.t),
           Rbset.node true
             (Rbset.node true (b : Rbset.t) (y : int) (c : Rbset.t))
             (z : int)
             (d : Rbset.t) ) ->
      Rbset.node true y (Rbset.node false x a b) (Rbset.node false z c d)
  | _
    when ( false,
           (x : int),
           (a : Rbset.t),
           Rbset.node true
             (b : Rbset.t)
             (y : int)
             (Rbset.node true (c : Rbset.t) (z : int) (d : Rbset.t)) ) ->
      Rbset.node true y (Rbset.node false x a b) (Rbset.node false z c d)
  | _ when ((r : bool), (x : int), (tree1 : Rbset.t), (tree2 : Rbset.t)) ->
      Rbset.node r x tree1 tree2
