open Value;;
open Basic_dt;;

let unused x = Some x
let insert = function
  | [L l; I idx; I elem] ->
    let rec aux l n =
      if n == 0 then elem :: l else
        match l with
        | [] -> [elem]
        | h :: t -> h :: (aux t (n - 1))
    in
    Some [L (aux l idx)]
  | _ -> raise @@ failwith "runtime operator error"

let replace = function
  | [L l; I idx; I elem] -> Sugar.opt_fmap (fun x -> [L x]) @@ List.replace_opt l idx elem
  | _ -> raise @@ failwith "runtime operator error"

let swap = function
  | [L l; I idx; I idx'] -> Sugar.opt_fmap (fun x -> [L x]) @@ List.swap_opt l idx idx'
  | _ -> raise @@ failwith "runtime operator error"

let cons = function
  | [L l; I elem] -> Some [L (elem :: l)]
  | _ -> raise @@ failwith "runtime operator error"

let append = function
  | [L l; I elem] -> Some [L (l @ [elem])]
  | _ -> raise @@ failwith "runtime operator error"

let plus1 = function
  | [I elem] -> Some [I (elem + 1)]
  | _ -> raise @@ failwith "runtime operator error"

let minus1 = function
  | [I elem] -> Some [I (elem - 1)]
  | _ -> raise @@ failwith "runtime operator error"

let top = function
  | [L []] -> None
  | [L (h :: _)] -> Some [I h]
  | _ -> raise @@ failwith "runtime operator error"

let bottom = function
  | [L []] -> None
  | [L l] -> Some [I (List.last l)]
  | _ -> raise @@ failwith "runtime operator error"

let max = function
  | [L l] -> Sugar.opt_fmap (fun x -> [I x]) @@ IntList.max_opt l
  | _ -> raise @@ failwith "runtime operator error"

let min = function
  | [L l] -> Sugar.opt_fmap (fun x -> [I x]) @@ IntList.min_opt l
  | _ -> raise @@ failwith "runtime operator error"

let const_value i = function
  | [] -> Some [I i]
  | _ -> raise @@ failwith "runtime operator error"

let rec merge_raw l1 l2 =
  match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | h1 :: t1, h2 :: t2 ->
    if h1 < h2
    then h1 :: h2 :: (merge_raw t1 t2)
    else if h1 > h2
    then h2 :: h1 :: (merge_raw t1 t2)
    else h1 :: (merge_raw t1 t2)

let prog_merge = function
  | [L l1; L l2] -> Some [L (merge_raw l1 l2)]
  | _ -> raise @@ failwith "runtime client program error"

let sigma_merge = function
  | [L l1; L l2] ->
    let check = List.check_sorted (fun a b -> a < b) in
    check l1 && check l2
  | _ -> raise @@ failwith "runtime client sigma error"

let phi_merge = function
  | [L l1] ->
    let check = List.check_sorted (fun a b -> a < b) in
    check l1
  | _ -> raise @@ failwith "runtime client phi error"
