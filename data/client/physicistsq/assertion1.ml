let preds = [| "hd"; "ord"; "mem"; "<"; "==" |]

let op_pool = [| "theta_stream" |]

let libs = [| "Physicistsq"; "Nat" |]

(* let i_err = ([ 2 ], 2, [ 2; 3 ], 3, [ 6; 5; 4 ]) *)
let i_err = ([ 2 ], 1, [ 2 ], 2, [ 6; 5 ])

let sampling_rounds = 5

let p_size = 4

let pre (w : Physicistsq.t) (lenf : nat) (f : Physicistsq.t) (lenr : nat)
    (r : Physicistsq.t) =
  strict_sort w && strict_sort f && strict_sort_rev r
  && physicistsq_last_head f r && size f lenf && size r lenr

let post (w : Physicistsq.t) (lenf : nat) (f : Physicistsq.t) (lenr : nat)
    (r : Physicistsq.t) (w' : Physicistsq.t) (lenf' : nat) (f' : Physicistsq.t)
    (lenr' : nat) (r' : Physicistsq.t) (u : int) =
  strict_sort w' && strict_sort f' && strict_sort_rev r'
  && physicistsq_last_head f' r'
  && size f' lenf' && size r' lenr'
  && (not (lenf' < lenr'))
  && implies (mem w' u) (mem f' u)
  && iff (mem f u || mem r u) (mem f' u || mem r' u)
