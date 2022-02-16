val rotate : Realtimeq.t -> Realtimeq.t -> Realtimeq.t -> Realtimeq.t

let rec rotate (q1 : Realtimeq.t) (q2 : Realtimeq.t) (q3 : Realtimeq.t) =
  match (q1, q2, q3) with
  | _ when (Realtimeq.nil, Realtimeq.cons y b, a) -> Realtimeq.cons y a
  | _ when (Realtimeq.cons x xs, Realtimeq.cons y ys, a) ->
      Realtimeq.cons x (rotate ys xs (Realtimeq.cons y a))
  | _ when (c, b, a) -> raise Empty
