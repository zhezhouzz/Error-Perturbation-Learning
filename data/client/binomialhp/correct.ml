val merge : Binomialhp.t -> Binomialhp.t -> Binomialhp.t

let rec merge (ts1 : Binomialhp.t) (ts2 : Binomialhp.t) =
  match (ts1, ts2) with
  | _ when (ts1', Binomialhp.nil) -> ts1'
  | _ when (Binomialhp.nil, ts2') -> ts2'
  | _ when (Binomialhp.cons t1 ts1', Binomialhp.cons t2 ts2') ->
      if Binomialhp.rank t1 < Binomialhp.rank t2 then
        Binomialhp.cons t1 (merge ts1' ts2)
      else if Binomialhp.rank t2 < Binomialhp.rank t1 then
        Binomialhp.cons t2 (merge ts1 ts2')
      else Binomialhp.ins_tree (Binomialhp.link t1 t2) (merge ts1' ts2')
