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
          Splayhp.node (Splayhp.node a x b1) yb small1, big1
        else
          let (small2: Splayhp.t), (big2: Splayhp.t) = partition pivot b1 in
          Splayhp.node a x small2, Splayhp.node big2 yb b2
    else
      match a with
      |_ when Splayhp.leaf -> a, tr
      |_ when Splayhp.node ya a1 a2 ->
        if ya <= pivot then
          let (small3: Splayhp.t), (big3: Splayhp.t) = partition pivot a2 in
          Splayhp.node a1 ya small3, Splayhp.node big3 x b
        else
          let (small4: Splayhp.t), (big4: Splayhp.t) = partition pivot a1 in
          small4, Splayhp.node big4 ya (Splayhp.node a2 x b)
