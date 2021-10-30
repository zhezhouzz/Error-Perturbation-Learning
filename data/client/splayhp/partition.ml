val partition: int -> Splayhp.t -> (Splayhp.t * Splayhp.t)

let rec partition (pivot: int) (tr: Splayhp.t) =
  match tr with
  |_ when Splayhp.leaf -> tr, tr
  |_ when Splayhp.node x a b ->
    if x <= pivot then
      match b with
      |_ when Splayhp.leaf -> tr, b
      |_ when Splayhp.node yb b1 b2->
        if yb <= pivot then
          let (small1: Splayhp.t), (big1: Splayhp.t) = partition pivot b2 in
          Splayhp.node yb (Splayhp.node x a b1) small1, big1
        else
          let (small2: Splayhp.t), (big2: Splayhp.t) = partition pivot b1 in
          Splayhp.node yb a small2, Splayhp.node yb big2 b2
    else
      match a with
      |_ when Splayhp.leaf -> a, tr
      |_ when Splayhp.node ya a1 a2 ->
        if ya <= pivot then
          let (small3: Splayhp.t), (big3: Splayhp.t) = partition pivot a2 in
          Splayhp.node ya a1 small3, Splayhp.node x big3 b
        else
          let (small4: Splayhp.t), (big4: Splayhp.t) = partition pivot a1 in
          small4, Splayhp.node ya big4 (Splayhp.node x a2 b)
