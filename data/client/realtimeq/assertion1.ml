let preds = [| "hd"; "ord"; "mem"; "<"; "==" |]

let op_pool = [| "theta_stream"; "theta_int" |]

let libs = [| "Realtimeq" |]

let i_err = ([ 1; 2 ], [ 6; 5; 4 ], [ 9 ])

let sampling_rounds = 6

let p_size = 4

let pre (q1 : Realtimeq.t) (q2 : Realtimeq.t) (q3 : Realtimeq.t) =
  strict_sort q1 && strict_sort_rev q2 && strict_sort q3 && less_last_last q1 q2
  && less_hd_hd q2 q3 && size_plus1 q1 q2

let post (q1 : Realtimeq.t) (q2 : Realtimeq.t) (q3 : Realtimeq.t)
    (nu : Realtimeq.t) =
  strict_sort_rev nu
