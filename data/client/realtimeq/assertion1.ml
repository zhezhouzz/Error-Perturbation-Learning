let preds = [| "hd"; "last"; "ord"; "mem"; "size_plus1"; "<" |]

let op_pool =
  [|
    (* "replace"; *)
    (* "insert"; *)
    "cons";
    "append";
    "list_destruct";
    "list_last_destruct";
    "list_mid_partition";
    "list_alter_partition";
    "list_upper_bound";
    "list_lower_bound";
    "list_single";
    "max";
    "min";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Realtimeq" |]

let i_err = ([ 1; 2 ], [ 6; 4; 3 ], [])

let sampling_rounds = 6

let p_size = 4

let pre (q1 : Realtimeq.t) (q2 : Realtimeq.t) (q3 : Realtimeq.t) (u : int)
    (v : int) =
  implies (ord q1 u v) (u < v)
  && implies (ord q2 u v) (v < u)
  && implies (ord q3 u v) (u < v)
  && implies (last q1 u && last q2 v) (u < v)
  && implies (hd q2 u && hd q3 v) (u < v)
  && size_plus1 q1 q2

let post (q1 : Realtimeq.t) (q2 : Realtimeq.t) (q3 : Realtimeq.t)
    (nu : Realtimeq.t) (u : int) (v : int) =
  implies (ord nu u v) (u < v)
