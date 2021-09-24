open Value;;
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
  | [L l; I idx; I elem] ->
    let rec aux l n =
      if n == 0 then elem :: l else
        match l with
        | [] -> [elem]
        | h :: t -> h :: (aux t (n - 1))
    in
    Some [L (aux l idx)]
  | _ -> raise @@ failwith "runtime operator error"

let top = function
  | [L []] -> None
  | [L (h :: _)] -> Some [I h]
  | _ -> raise @@ failwith "runtime operator error"
