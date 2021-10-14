val merge: List.t -> List.t -> List.t

let rec merge (l1: List.t) (l2: List.t) =
  match l1 with
  |_ when List.nil -> l2
  |_ when List.cons hd1 tl1 ->
    match l2 with
    |_ when List.nil -> l1
    |_ when List.cons hd2 tl2 ->
      if hd1 < hd2
      then
        let tmp0: List.t = merge tl1 tl2 in
        let tmp1: List.t = List.cons hd2 tmp0 in
        List.cons hd1 tmp1
      else if hd2 < hd1
      then
        let tmp0: List.t = merge tl1 tl2 in
        let tmp1: List.t = List.cons hd1 tmp0 in
        List.cons hd2 tmp1
      else
        let tmp0: List.t = merge tl1 tl2 in
        List.cons hd1 tmp0
