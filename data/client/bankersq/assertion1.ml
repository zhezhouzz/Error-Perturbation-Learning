let preds = [| "hd"; "ord"; "mem"; "<"; "empty"; "size1"; "size2"; "size3" |]

let op_pool = [| "theta_list"; "theta_int" |]

let libs = [| "Bankersq" |]

let i_err = (2, [ 1; 2 ], 1, [ 3 ], 4)

let sampling_rounds = 6

let p_size = 4

let pre (lenf : int) (f : Bankersq.t) (lenr : int) (r : Bankersq.t) (x : int) =
  lenr < lenf && size f lenf && size r lenr

let post (lenf : int) (f : Bankersq.t) (lenr : int) (r : Bankersq.t) (x : int)
    (lenf' : int) (f' : Bankersq.t) (lenr' : int) (r' : Bankersq.t) (u : int) =
  lenr' < lenf' && size f' lenf' && size r' lenr'
  && iff (mem f u || mem r u || u == x) (mem f' u && mem r' u)
  && iff
       (mem f u || mem r u)
       ((mem f' u && mem r' x) || ord r' x u || ord f' u x)
