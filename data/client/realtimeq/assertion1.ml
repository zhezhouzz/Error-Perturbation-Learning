let preds = [| "hd"; "last"; "ord"; "mem"; "size"; "<" |]

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

let pre (w : Realtimeq.t) (lenf : int) (f : Realtimeq.t) (lenr : int)
    (r : Realtimeq.t) (u : int) (v : int) =
  implies (ord w u v) (u < v)
  && implies (ord f u v) (u < v)
  && implies (ord r u v) (v < u)
  && size f lenf && size r lenr

let post (w : Realtimeq.t) (lenf : int) (f : Realtimeq.t) (lenr : int)
    (r : Realtimeq.t) (w' : Realtimeq.t) (lenf' : int) (f' : Realtimeq.t)
    (lenr' : int) (r' : Realtimeq.t) (u : int) (v : int) =
  implies (ord w' u v) (u < v)
  && implies (ord f' u v) (u < v)
  && implies (ord r' u v) (v < u)
  && size f' lenf' && size r' lenr'
  && (not (lenf' < lenr'))
  && implies (mem w' u) (mem f' u)
  && iff (mem f u || mem r u) (mem f' u || mem r' u)
