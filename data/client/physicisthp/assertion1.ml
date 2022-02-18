let preds = [| "hd"; "last"; "ord"; "mem"; "size"; "<" |]

let op_pool =
  [|
    (* "replace"; *)
    (* "insert"; *)
    "cons";
    "append";
    "list_destruct";
    "list_last_destruct";
    "list_upper_bound";
    "list_lower_bound";
    "list_single"
    (* "plus1"; *)
    (* "minus1"; *);
  |]

let libs = [| "Physicistsq"; "Nat" |]

let i_err = ([ 2 ], 2, [ 2; 3 ], 3, [ 6; 5; 4 ])

let sampling_rounds = 5

let p_size = 4

let pre (w : Physicistsq.t) (lenf : nat) (f : Physicistsq.t) (lenr : nat)
    (r : Physicistsq.t) (u : int) (v : int) =
  implies (ord w u v) (u < v)
  && implies (ord f u v) (u < v)
  && implies (ord r u v) (v < u)
  && implies (last f v && hd r u) (v < u)
  && size f lenf && size r lenr

let post (w : Physicistsq.t) (lenf : nat) (f : Physicistsq.t) (lenr : nat)
    (r : Physicistsq.t) (w' : Physicistsq.t) (lenf' : nat) (f' : Physicistsq.t)
    (lenr' : nat) (r' : Physicistsq.t) (u : int) (v : int) =
  implies (ord w' u v) (u < v)
  && implies (ord f' u v) (u < v)
  && implies (ord r' u v) (v < u)
  && implies (last f' v && hd r' u) (v < u)
  && size f' lenf' && size r' lenr'
  && (not (lenf' < lenr'))
  && implies (mem w' u) (mem f' u)
  && iff (mem f u || mem r u) (mem f' u || mem r' u)
