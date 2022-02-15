val merge : Leftisthp.t -> Leftisthp.t -> Leftisthp.t

let rec merge (h1 : Leftisthp.t) (h2 : Leftisthp.t) =
  match (h1, h2) with
  | _ when (a, Leftisthp.leaf) -> a
  | _ when (Leftisthp.leaf, a) -> a
  | _ when (Leftisthp.node p1 x a1 b1, Leftisthp.node p2 y a2 b2) ->
      if x <= y then Leftisthp.makeT x a1 (merge b1 h2)
      else Leftisthp.makeT y a2 (merge h1 b2)
