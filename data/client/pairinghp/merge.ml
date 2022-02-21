val merge : Pairinghp.t -> Pairinghp.t -> Pairinghp.t

let rec merge (ts1 : Pairinghp.t) (ts2 : Pairinghp.t) =
  match (ts1, ts2) with
  | _ when (p, Pairinghp.leaf) -> p
  | _ when (Pairinghp.leaf, q) -> q
  | _ when (Pairinghp.node x ts1', Pairinghp.node y ts2') ->
      if x <= y then
        Pairinghp.node y (Pairinghp.cons (Pairinghp.node x ts2') ts1')
      else Pairinghp.node y (Pairinghp.cons (Pairinghp.node x ts1') ts2')
