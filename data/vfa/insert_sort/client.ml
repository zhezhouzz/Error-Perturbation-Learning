val insert : int -> List.t -> List.t

let rec insert (i : int) (l : List.t) =
  match l with
  | _ when List.nil -> List.cons i List.nil
  | _ when List.cons h t ->
      if i < h then List.cons i (List.cons h t) else List.cons h (insert i t)
