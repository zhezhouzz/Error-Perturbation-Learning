type elem = int

type tree = Node of int * elem * elem list * tree list

type t = tree list

exception Empty

open Zlist

let deep = List.length

let rec to_string t =
  match t with
  | [] -> "()"
  | _ ->
      let a, b =
        List.fold_left
          (fun (layer, rest) tr ->
            match tr with
            | Node (r, x, l, t) -> (layer @ [ (r, x, l) ], rest @ [ t ]))
          ([], []) t
      in
      Printf.sprintf "%s\n%s"
        (List.split_by " "
           (fun (r, x, l) ->
             Printf.sprintf "%i:%i~[%s] " r x @@ IntList.to_string l)
           a)
        (List.split_by " " (fun t -> Printf.sprintf "{%s}" @@ to_string t) b)

let rec flatten t =
  List.fold_left
    (fun res tr ->
      match tr with Node (r, x, l, t) -> [ r; x ] @ l @ res @ flatten t)
    [] t

let compare t1 t2 =
  let rec aux t1 t2 =
    match (t1, t2) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | Node (r1, x1, l1, y1) :: t1, Node (r2, x2, l2, y2) :: t2 ->
        let tmp = compare r1 r2 in
        if tmp != 0 then tmp
        else
          let tmp = compare x1 x2 in
          if tmp != 0 then tmp
          else
            let tmp = List.compare compare l1 l2 in
            if tmp != 0 then tmp
            else
              let tmp = aux y1 y2 in
              if tmp != 0 then tmp else aux t1 t2
  in
  aux t1 t2

let eq t1 t2 = compare t1 t2 == 0

let empty = []

let is_empty ts = ts = []

let rank (Node (r, _, _, _)) = r

let root (Node (_, x, _, _)) = x

let link (Node (r, x1, xs1, c1) as t1) (Node (_, x2, xs2, c2) as t2) =
  if x1 <= x2 then Node (r + 1, x1, xs1, t2 :: c1)
  else Node (r + 1, x2, xs2, t1 :: c2)

let skew_link x t1 t2 =
  let (Node (r, y, ys, c)) = link t1 t2 in
  if x <= y then Node (r, x, y :: ys, c) else Node (r, y, x :: ys, c)

let rec ins_tree t = function
  | [] -> [ t ]
  | t' :: ts ->
      if rank t < rank t' then t :: t' :: ts else ins_tree (link t t') ts

let rec merge_trees ts1 ts2 =
  match (ts1, ts2) with
  | _, [] -> ts1
  | [], _ -> ts2
  | t1 :: ts1', t2 :: ts2' ->
      if rank t1 < rank t2 then t1 :: merge_trees ts1' ts2
      else if rank t2 < rank t1 then t2 :: merge_trees ts1 ts2'
      else ins_tree (link t1 t2) (merge_trees ts1' ts2')

let normalize = function [] -> [] | t :: ts -> ins_tree t ts

let insert x = function
  | t1 :: t2 :: rest as ts ->
      if rank t1 = rank t2 then skew_link x t1 t2 :: rest
      else Node (0, x, [], []) :: ts
  | ts -> Node (0, x, [], []) :: ts

let merge ts1 ts2 = merge_trees (normalize ts1) (normalize ts2)

let rec remove_min_tree = function
  | [] -> raise Empty
  | [ t ] -> (t, [])
  | t :: ts ->
      let t', ts' = remove_min_tree ts in
      if root t <= root t' then (t, ts) else (t', t :: ts')

let find_min ts = root (fst (remove_min_tree ts))

let delete_min ts =
  let Node (_, _, xs, ts1), ts2 = remove_min_tree ts in
  let rec insert_all ts = function
    | [] -> ts
    | x :: xs' -> insert_all (insert x ts) xs'
  in
  insert_all (merge (List.rev ts1) ts2) xs
