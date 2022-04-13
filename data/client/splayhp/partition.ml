val partition : int -> Splayhp.t -> Splayhp.t * Splayhp.t

let rec partition (pivot : int) (tr : Splayhp.t) =
  match tr with
  | _ when Splayhp.leaf -> (tr, tr)
  | _ when Splayhp.node x a b -> (
      if x <= pivot then
        match b with
        | _ when Splayhp.leaf -> (tr, b)
        | _ when Splayhp.node y b1 b2 ->
            if y <= pivot then
              let (small : Splayhp.t), (big : Splayhp.t) = partition pivot b2 in
              (Splayhp.node y (Splayhp.node x a b1) big, small)
            else
              let (small : Splayhp.t), (big : Splayhp.t) = partition pivot b1 in
              (Splayhp.node x a small, Splayhp.node y big b2)
      else
        match a with
        | _ when Splayhp.leaf -> (a, tr)
        | _ when Splayhp.node y a1 a2 ->
            if y <= pivot then
              let (small : Splayhp.t), (big : Splayhp.t) = partition pivot a2 in
              (Splayhp.node y a1 small, Splayhp.node x big b)
            else
              let (small : Splayhp.t), (big : Splayhp.t) = partition pivot a1 in
              (small, Splayhp.node y big (Splayhp.node x a2 b)))
