type tree = Node of int * int * tree list

type t = tree list

exception Empty

let empty = []

open Zlist

let deep = List.length

let of_list l =
  let num = 1 in
  let rec make_one l =
    match l with
    | [] -> raise @@ failwith "cannot make empty tree in binomail"
    | (r, x) :: t -> Node (r, x, make [] 1 t)
  and make tmp num l =
    if List.length l == 0 then tmp
    else if List.length l < num then raise @@ failwith "wrong binomial farmat"
    else
      let a = List.sublist l (0, num) in
      let l' = List.sublist l (num, List.length l) in
      make (tmp @ [ make_one a ]) (num * 2) l'
  in
  make [] num l

let rec to_string t =
  match t with
  | [] -> "()"
  | _ ->
      let a, b =
        List.fold_left
          (fun (layer, rest) tr ->
            match tr with Node (r, x, t) -> (layer @ [ (r, x) ], rest @ [ t ]))
          ([], []) t
      in
      Printf.sprintf "%s\n%s"
        (List.split_by " " (fun (r, x) -> Printf.sprintf "%i:%i " r x) a)
        (List.split_by " " (fun t -> Printf.sprintf "{%s}" @@ to_string t) b)

let rec flatten t =
  List.fold_left
    (fun res tr -> match tr with Node (r, x, t) -> [ r; x ] @ res @ flatten t)
    [] t

let compare t1 t2 =
  let rec aux t1 t2 =
    match (t1, t2) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | Node (r1, x1, y1) :: t1, Node (r2, x2, y2) :: t2 ->
        let tmp = compare r1 r2 in
        if tmp != 0 then tmp
        else
          let tmp = compare x1 x2 in
          if tmp != 0 then tmp
          else
            let tmp = aux y1 y2 in
            if tmp != 0 then tmp else aux t1 t2
  in
  aux t1 t2

let eq t1 t2 = compare t1 t2 == 0

let is_empty ts = ts = []

let rank (Node (r, _, _)) = r

let root (Node (_, x, _)) = x

let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
  if x1 <= x2 then Node (r + 1, x1, t2 :: c1) else Node (r + 1, x2, t1 :: c2)

let rec ins_tree t = function
  | [] -> [ t ]
  | t' :: ts' as ts ->
      if rank t < rank t' then t :: ts else ins_tree (link t t') ts'

let insert x ts = ins_tree (Node (0, x, [])) ts

let rec merge ts1 ts2 =
  match (ts1, ts2) with
  | _, [] -> ts1
  | [], _ -> ts2
  | t1 :: ts1', t2 :: ts2' ->
      if rank t1 < rank t2 then t1 :: merge ts1' ts2
      else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
      else ins_tree (link t1 t2) (merge ts1' ts2')

let rec remove_min_tree = function
  | [] -> raise Empty
  | [ t ] -> (t, [])
  | t :: ts ->
      let t', ts' = remove_min_tree ts in
      if root t <= root t' then (t, ts) else (t', t :: ts')

let find_min ts = root (fst (remove_min_tree ts))

let delete_min ts =
  let Node (_, _, ts1), ts2 = remove_min_tree ts in
  merge (List.rev ts1) ts2
