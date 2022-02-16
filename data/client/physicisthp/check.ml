val check :
  Physicistsq.t ->
  int ->
  Physicistsq.t ->
  int ->
  Physicistsq.t ->
  Physicistsq.t * int * Physicistsq.t * int * Physicistsq.t

let check (w : Physicistsq.t) (lenf : int) (f : Physicistsq.t) (lenr : int)
    (r : Physicistsq.t) =
  if lenr <= lenf then
    if Physicistsq.is_empty w then (f, lenf, f, lenr, r)
    else (w, lenf, f, lenr, r)
  else if Physicistsq.is_empty f then
    ( Physicistsq.concat f (Physicistsq.rev r),
      lenf + lenr,
      Physicistsq.concat f (Physicistsq.rev r),
      0,
      [] )
  else (f, lenf + lenr, Physicistsq.concat f r, 0, [])
