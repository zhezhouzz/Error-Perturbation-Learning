val set_add : int -> Uniquel.t -> Uniquel.t

let rec set_add (a : int) (x : Uniquel.t) =
  match x with
  | _ when Uniquel.nil -> Uniquel.cons a x
  | _ when Uniquel.cons (a1 : int) (x1 : Uniquel.t) ->
      Uniquel.cons a (set_add a1 x1)
