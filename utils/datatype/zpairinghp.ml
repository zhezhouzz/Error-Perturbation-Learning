type t = E | T of int * t list

exception Empty

let empty = E

open Zlist

let rec deep = function
  | E -> 0
  | T (_, l) -> (
      match IntList.max_opt @@ List.map deep l with
      | None -> 1
      | Some x -> 1 + x)

let to_string t =
  let rec aux l =
    match l with
    | [] -> "()"
    | _ ->
        let a, b =
          List.fold_left
            (fun (layer, rest) tr ->
              match tr with
              | E -> (layer @ [ " " ], rest @ [])
              | T (x, l) -> (layer @ [ string_of_int x ], rest @ [ l ]))
            ([], []) l
        in
        Printf.sprintf "%s\n%s"
          (List.split_by " " (fun x -> Printf.sprintf "%s " x) a)
          (List.split_by " " (fun t -> Printf.sprintf "{%s}" @@ aux t) b)
  in
  aux [ t ]

let rec flatten t =
  match t with E -> [] | T (x, l) -> x :: (List.concat @@ List.map flatten l)

let compare t1 t2 =
  let rec aux t1 t2 =
    match (t1, t2) with
    | E, E -> 0
    | E, _ -> -1
    | _, E -> 1
    | T (x1, l1), T (x2, l2) ->
        let tmp = compare x1 x2 in
        if tmp != 0 then tmp else List.compare aux l1 l2
  in
  aux t1 t2

let eq t1 t2 = compare t1 t2 == 0

let is_empty h = h = E

let merge h1 h2 =
  match (h1, h2) with
  | _, E -> h1
  | E, _ -> h2
  | T (x, hs1), T (y, hs2) ->
      if x <= y then T (x, h2 :: hs1) else T (y, h1 :: hs2)

let insert x h = merge (T (x, [])) h

let rec merge_pairs = function
  | [] -> E
  | [ h ] -> h
  | h1 :: h2 :: hs -> merge (merge h1 h2) (merge_pairs hs)

let find_min = function E -> raise Empty | T (x, _) -> x

let delete_min = function E -> raise Empty | T (_, hs) -> merge_pairs hs
