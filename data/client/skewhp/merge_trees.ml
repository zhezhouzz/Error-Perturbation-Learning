val merge : Skewhp.t -> Skewhp.t -> Skewhp.t

let rec merge_trees (ts1 : Skewhp.t) (ts2 : Skewhp.t) =
  match (ts1, ts2) with
  | _ when (ts1', Skewhp.nil) -> ts1'
  | _ when (Skewhp.nil, ts2') -> ts2'
  | _ when (Skewhp.cons t1 ts1', Skewhp.cons t2 ts2') ->
      if Skewhp.rank t1 < Skewhp.rank t2 then
        Skewhp.cons t1 (merge_trees ts1' ts2)
      else if Skewhp.rank t2 < Skewhp.rank t1 then
        Skewhp.cons t2 (merge_trees ts1 ts2')
      else Skewhp.ins_tree (Skewhp.link t1 t1) (merge_trees ts1' ts2')
