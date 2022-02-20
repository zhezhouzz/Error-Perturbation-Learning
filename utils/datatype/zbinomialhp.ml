type tree = Node of int * int * tree list

type t = tree list

exception Empty

let empty = []

open Zlist

let deep = List.length

let rec max_deep l =
  let aux = function Node (_, _, l) -> 1 + max_deep l in
  match IntList.max_opt @@ List.map aux l with None -> 0 | Some x -> x

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

let rec formal_layout l =
  match l with
  | [] -> "BiNil"
  | Node (r, x, l') :: t ->
      Printf.sprintf "BiCons (BiNode (%i, %i, %s), %s)" r x (formal_layout l')
        (formal_layout t)

let flatten_ input =
  let rec aux res t =
    match t with
    | [] -> res
    | _ ->
        let res', ts =
          List.fold_left
            (fun (res, ts) tr ->
              match tr with Node (r, x, t) -> ((r, x) :: res, t :: ts))
            ([], []) t
        in
        aux (res' :: res) (List.concat ts)
  in
  List.concat @@ aux [] input

let flatten t =
  (* let _ = Printf.printf "flatten\n" in *)
  let x, y = List.split @@ flatten_ t in
  (* Printf.printf "flatten end\n"; *)
  x @ y

let to_string l =
  match l with
  | [] -> "_"
  | _ ->
      let len = max_deep l in
      let arr = Array.init len (fun _ -> "") in
      let update idx str = arr.(idx) <- arr.(idx) ^ str in
      let update_below idx str =
        List.iter (fun i -> update i str)
        @@ List.init (len - idx) (fun x -> idx + x)
      in
      let rec aux idx t =
        match t with
        | Node (r, x, l) ->
            update_below idx "{";
            update idx (Printf.sprintf "%i:%i, " r x);
            List.iter (fun t -> aux (idx + 1) t) l;
            update_below idx "}"
      in
      List.iter (fun t -> aux 0 t) l;
      Array.fold_left
        (fun res str -> Printf.sprintf "%s\n%s" res str)
        (Printf.sprintf "flatten:%s\n"
        @@ List.split_by_comma (fun (a, b) -> Printf.sprintf "%i:%i" a b)
        @@ flatten_ l)
        arr

(* let rec aux t = *)
(*   let a, b = *)
(*     List.fold_left *)
(*       (fun (layer, rest) tr -> *)
(*          match tr with Node (r, x, t) -> (layer @ [ (r, x) ], rest @ [ t ])) *)
(*       ([], []) t *)
(*   in *)
(*   let b, c = List.split @@ List.map aux b in *)
(*   aLS b *)
(*   let b = LS b, *)
(* match t with *)
(* | [] -> "_" *)
(* | _ -> *)
(*     let a, b = *)
(*       List.fold_left *)
(*         (fun (layer, rest) tr -> *)
(*           match tr with Node (r, x, t) -> (layer @ [ (r, x) ], rest @ [ t ])) *)
(*         ([], []) t *)
(*     in *)
(*     let b = List.map        *)
(*     Printf.sprintf "%s\n%s" (nested_list_to_string (LO a)) *)
(*       (nested_list_to_string ())       *)
(*       (List.split_by ", " (fun (r, x) -> Printf.sprintf "%i:%i" r x) a) *)
(*       (List.split_by " " (fun t -> Printf.sprintf "{%s}" @@ to_string t) b) *)

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

let rec num_node = function
  | [] -> 0
  | Node (_, _, ts') :: ts -> 1 + num_node ts' + num_node ts

let if_complete_list l =
  (* let () = Printf.printf "_complete_list? %s\n" @@ IntList.to_string l in *)
  let len = List.length l in
  let arr = Array.init len (fun _ -> false) in
  let b =
    List.fold_left
      (fun b i ->
        (* let i = i - 1 in *)
        if not b then false
        else if i < 0 || i >= len then false
        else if arr.(i) then false
        else (
          arr.(i) <- true;
          true))
      true l
  in
  if not b then false else Array.for_all (fun x -> x) arr

let rec binomial_complete_tree = function
  | Node (0, _, []) -> true
  | Node (num_nodes, _, ts) ->
      if List.length ts != num_nodes then false
      else if
        let if_comp = if_complete_list @@ List.map rank ts in
        (* let () = Printf.printf "_complete_list? %b\n" if_comp in *)
        if_comp
      then List.for_all binomial_complete_tree ts
      else false

let flatten_node t = snd @@ List.split @@ flatten_ t

let mem t x = List.mem x @@ flatten_node t

let max_opt t = IntList.max_opt @@ flatten_node t

let min_opt t = IntList.min_opt @@ flatten_node t

let t_head = function Node (r, x, _) -> (r, x)

let t_head_update t x = match t with Node (r, _, l) -> Node (r, x, l)

let binomialhp ts =
  let rec to_binary res = function
    | 0 -> res
    | 1 -> true :: res
    | 2 -> true :: false :: res
    | 3 -> true :: true :: res
    | n ->
        let n' = n / 2 in
        let r = n - (n' * 2) in
        let b =
          if r == 1 then true
          else if r == 0 then false
          else raise @@ failwith "bad divide"
        in
        to_binary (b :: res) n'
  in
  let bl = List.rev @@ to_binary [] @@ num_node ts in
  (* let () = Printf.printf "bl: %s\n" @@ List.split_by_comma string_of_bool bl in *)
  let bl = List.filter_mapi (fun idx b -> if b then Some idx else None) bl in
  (* let () = Printf.printf "bl: %s\n" @@ IntList.to_string bl in *)
  if List.length ts != List.length bl then false
  else
    let rs = List.map rank ts in
    if List.eq ( == ) bl rs then List.for_all binomial_complete_tree ts
    else false
