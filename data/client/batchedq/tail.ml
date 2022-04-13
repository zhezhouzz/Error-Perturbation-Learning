val tail : Batchedq.t -> Batchedq.t -> Batchedq.t * Batchedq.t

let tail (f : Batchedq.t) (r : Batchedq.t) =
  match (f, r) with
  | _ when (Batchedq.nil, (r0 : Batchedq.t)) -> raise Empty
  | _ when (Batchedq.cons (x : int) (f1 : Batchedq.t), (r1 : Batchedq.t)) ->
      if Batchedq.is_empty f1 then (Batchedq.rev r1, f1)
      else (Batchedq.cons x Batchedq.nil, r1)
