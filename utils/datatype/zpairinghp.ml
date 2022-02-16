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

let rec flatten t =
  match t with E -> [] | T (x, l) -> x :: (List.concat @@ List.map flatten l)

let to_string l =
  let len = 1 + deep l in
  let arr = Array.init len (fun _ -> "") in
  let update idx str = arr.(idx) <- arr.(idx) ^ str in
  let update_below idx str =
    List.iter (fun i -> update i str)
    @@ List.init (len - idx) (fun x -> idx + x)
  in
  let rec aux idx t =
    match t with
    | E -> update idx "()"
    | T (x, l) ->
        update_below idx "{";
        update idx (Printf.sprintf "%i, " x);
        List.iter (fun t -> aux (idx + 1) t) l;
        update_below idx "}"
  in
  aux 0 l;
  Array.fold_left
    (fun res str -> Printf.sprintf "%s\n%s" res str)
    (Printf.sprintf "flatten:%s\n" @@ IntList.to_string @@ flatten l)
    arr

(* let to_string t = *)
(*   let rec aux l = *)
(*     match l with *)
(*     | [] -> "()" *)
(*     | _ -> *)
(*         let a, b = *)
(*           List.fold_left *)
(*             (fun (layer, rest) tr -> *)
(*               match tr with *)
(*               | E -> (layer @ [ " " ], rest @ []) *)
(*               | T (x, l) -> (layer @ [ string_of_int x ], rest @ [ l ])) *)
(*             ([], []) l *)
(*         in *)
(*         Printf.sprintf "%s\n%s" *)
(*           (List.split_by " " (fun x -> Printf.sprintf "%s " x) a) *)
(*           (List.split_by " " (fun t -> Printf.sprintf "{%s}" @@ aux t) b) *)
(*   in *)
(*   aux [ t ] *)

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