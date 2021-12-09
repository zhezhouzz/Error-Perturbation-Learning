val concat : Customstk.t -> Customstk.t -> Customstk.t

let rec concat (s1 : Customstk.t) (s2 : Customstk.t) =
  if Customstk.is_empty s1 then s2
  else Customstk.push (Customstk.top s1) (concat (Customstk.tail s1) s2)
