module List = Zlist.List
module Tree = Ztree.Tree

module LabeledTree = struct
  type ('a, 'b) t = Leaf | Node of ('b * 'a * ('a, 'b) t * ('a, 'b) t)

  let spf = Printf.sprintf

  let rec map f = function
    | Leaf -> Leaf
    | Node (label, x, a, b) -> Node (label, f x, map f a, map f b)

  let deep t =
    let rec aux = function
      | Leaf -> 0
      | Node (_, _, l, r) ->
          let ln = aux l in
          let rn = aux r in
          if ln > rn then ln + 1 else rn + 1
    in
    aux t

  let rec size = function Leaf -> 0 | Node (_, _, a, b) -> 1 + size a + size b

  let flip tr =
    match tr with Leaf -> Leaf | Node (label, a, b, c) -> Node (label, a, c, b)

  let rec rec_flip tr =
    match tr with
    | Leaf -> Leaf
    | Node (label, a, b, c) -> Node (label, a, rec_flip c, rec_flip b)

  let rotation_right_opt tr =
    match tr with
    | Leaf -> Some Leaf
    | Node (labelx, x, Node (labely, y, a, b), c) ->
        Some (Node (labely, y, a, Node (labelx, x, b, c)))
    | _ -> None

  let rotation_left_opt tr =
    match tr with
    | Leaf -> Some Leaf
    | Node (labelx, x, a, Node (labely, y, b, c)) ->
        Some (Node (labely, y, Node (labelx, x, a, b), c))
    | _ -> None

  let rec append_to_left_most_label label x tr =
    match tr with
    | Leaf -> Node (label, x, Leaf, Leaf)
    | Node (labely, y, a, b) ->
        Node (labely, y, append_to_left_most_label label x a, b)

  let rec append_to_right_most_label label x tr =
    match tr with
    | Leaf -> Node (label, x, Leaf, Leaf)
    | Node (labely, y, a, b) ->
        Node (labely, y, a, append_to_right_most_label label x b)

  let max_opt (e_compare : 'a -> 'a -> int) t1 =
    let rec aux max_e = function
      | Leaf -> max_e
      | Node (_, a, b, c) ->
          let max_e =
            match max_e with
            | None -> a
            | Some max_e -> if e_compare a max_e > 0 then max_e else a
          in
          aux (aux (Some max_e) b) c
    in
    aux None t1

  let min_opt e_compare t1 = max_opt (fun x y -> ~-(e_compare x y)) t1

  let rec from_tree label t =
    match t with
    | Tree.Leaf -> Leaf
    | Tree.Node (a, l, r) ->
        Node (label, a, from_tree label l, from_tree label r)

  let exists f t =
    let rec aux before t =
      if before then true
      else
        match t with
        | Leaf -> false
        | Node (_, e, l, r) -> if f e then true else aux (aux before l) r
    in
    aux false t

  let formal_layout flabel f tr =
    let rec aux = function
      | Leaf -> "Leaf"
      | Node (label, a, Leaf, Leaf) ->
          spf "SLNode (%s, %s)" (flabel label) (f a)
      | Node (label, a, l, r) ->
          Printf.sprintf "LNode (%s, %s, %s, %s)" (flabel label) (f a) (aux l)
            (aux r)
    in
    aux tr

  let layout f tr =
    let rec aux = function
      | Leaf -> "."
      | Node (_, a, Leaf, Leaf) -> f a
      | Node (_, a, l, r) -> Printf.sprintf "{%s, %s, %s}" (aux l) (f a) (aux r)
    in
    aux tr

  let rec leaf eq t u =
    let nochild l r = match (l, r) with Leaf, Leaf -> true | _, _ -> false in
    match t with
    | Leaf -> false
    | Node (_, a, l, r) -> (eq a u && nochild l r) || leaf eq l u || leaf eq r u

  let rec node eq t u =
    let haschild l r =
      match (l, r) with
      | Node (_, _, _, _), _ | _, Node (_, _, _, _) -> true
      | _, _ -> false
    in
    match t with
    | Leaf -> false
    | Node (_, a, l, r) ->
        (eq a u && haschild l r) || node eq l u || node eq r u

  let left_child eq t u v =
    let rec aux before t =
      if before then true
      else
        match t with
        | Leaf -> false
        | Node (_, a, l, r) ->
            if eq a u && exists (fun x -> eq x v) l then true
            else aux (aux false l) r
    in
    aux false t

  let right_child eq t u v =
    let rec aux before t =
      if before then true
      else
        match t with
        | Leaf -> false
        | Node (_, a, l, r) ->
            if eq a u && exists (fun x -> eq x v) r then true
            else aux (aux false l) r
    in
    aux false t

  let parallel_child eq t u v =
    let rec aux = function
      | Leaf -> false
      | Node (_, _, l, r) ->
          (exists (fun x -> eq x u) l && exists (fun x -> eq x v) r)
          || aux l || aux r
    in
    aux t

  let left_adj_child eq t u v =
    let rec aux = function
      | Leaf -> false
      | Node (_, x, Node (_, y, _, _), _) when eq x u && eq y v -> true
      | Node (_, _, l, r) -> aux l || aux r
    in
    aux t

  let right_adj_child eq t u v =
    let rec aux = function
      | Leaf -> false
      | Node (_, x, _, Node (_, y, _, _)) when eq x u && eq y v -> true
      | Node (_, _, l, r) -> aux l || aux r
    in
    aux t

  let parallel_adj_child eq t u v =
    let rec aux = function
      | Leaf -> false
      | Node (_, _, Node (_, x, _, _), Node (_, y, _, _)) when eq x u && eq y v
        ->
          true
      | Node (_, _, l, r) -> aux l || aux r
    in
    aux t

  let eq lcompare compare t1 t2 =
    let rec aux = function
      | Leaf, Leaf -> true
      | Node (lab1, a1, l1, r1), Node (lab2, a2, l2, r2) ->
          if compare a1 a2 && lcompare lab1 lab2 then
            if aux (l1, l2) then aux (r1, r2) else false
          else false
      | _, _ -> false
    in
    aux (t1, t2)

  let rec flatten = function
    | Leaf -> []
    | Node (_, a, l, r) -> a :: (flatten l @ flatten r)

  let flatten_forall t = List.remove_duplicates (flatten t)

  (* let union l0 l1 = List.union (fun x y -> x == y) l0 l1 *)

  let once f tr e =
    let l = flatten tr in
    List.once f l e

  let compare e_compare t1 t2 =
    let rec aux t1 t2 =
      match (t1, t2) with
      | Leaf, Leaf -> 0
      | Leaf, Node _ -> -1
      | Node _, Leaf -> 1
      | Node (_, a1, l1, r1), Node (_, a2, l2, r2) ->
          let c = e_compare a1 a2 in
          if c != 0 then c
          else
            let c = aux l1 l2 in
            if c != 0 then c else aux r1 r2
    in
    aux t1 t2
end
