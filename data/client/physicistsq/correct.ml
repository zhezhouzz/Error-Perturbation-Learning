val check : Physicisthp.t -> Physicisthp.t

let check (w, lenf, f, lenr, r) =
  if lenr <= lenf then
    if List.is_empty w then (f, lenf, f, lenr, r) else (w, lenf, f, lenr, r)
  else
    let f' = f in
    if List.is_empty f' then
      ( List.append f' @@ List.rev r,
        lenf + lenr,
        List.append f' @@ List.rev r,
        0,
        [] )
    else (f', lenf + lenr, List.append f' @@ List.rev r, 0, [])
