val check :
  Physicistsq.t ->
  nat ->
  Physicistsq.t ->
  nat ->
  Physicistsq.t ->
  Physicistsq.t * nat * Physicistsq.t * nat * Physicistsq.t

let check (w : Physicistsq.t) (lenf : nat) (f : Physicistsq.t) (lenr : nat)
    (r : Physicistsq.t) =
  if Nat.leq lenr lenf then
    if Physicistsq.is_empty w then (f, lenf, f, lenr, r)
    else (w, lenf, f, lenr, r)
  else if Physicistsq.is_empty f then
    ( Physicistsq.concat f (Physicistsq.rev r),
      Nat.plus lenf lenr,
      Physicistsq.concat f (Physicistsq.rev r),
      Nat.zero,
      [] )
  else (f, Nat.plus lenf lenr, Physicistsq.concat f r, Nat.zero, [])
