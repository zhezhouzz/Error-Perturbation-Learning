let preds = [| "hd"; "mem"; "ord"; "<"; "empty"; "size1"; "size2"; "size3" |]

let op_pool = [| "theta_list"; "theta_int" |]

let libs = [| "Batchedq" |]

let i_err = ([ -7 ], [ -5; 6; 1; 0; 8 ])

let sampling_rounds = 6

let p_size = 4

let pre (f : Batchedq.t) (r : Batchedq.t) =
  uniq f && implies (empty f) (empty r)

let post (f : Batchedq.t) (r : Batchedq.t) (f' : Batchedq.t) (r' : Batchedq.t)
    (u : int) =
  iff (mem f' u || mem r' u || hd f u) (mem f u || mem r u)
  && implies (last r u) (hd f' u)
  (* && implies *)
  (*      (not (hd f u)) *)
  (*      (iff *)
  (*         (ord f' u v || ord r' v u || (mem f' u && mem r' v)) *)
  (*         (ord f u v || ord r v u || (mem f u && mem r v))) *)
  && uniq f'
  && implies (empty f') (empty r')
