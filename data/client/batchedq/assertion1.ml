let preds = [| "hd"; "mem"; "ord" |]

let op_pool =
  [|
    "insert";
    "replace";
    "cons";
    "append";
    "plus1";
    "minus1";
    "top";
    "bottom";
    "max";
    "min";
    "const0";
    "const1";
  |]

let libs = [| "Batchedq" |]

let i_err = ([ 1 ], [ 2; 3 ])

let sampling_rounds = 6

let p_size = 4

let post (f : Batchedq.t) (r : Batchedq.t) (f' : Batchedq.t) (r' : Batchedq.t)
    (u : int) (v : int) =
  iff (mem f' u || mem r' u || hd f u) (mem f u || mem r u)
  && implies (ord f' u v || ord r' v u) (ord f u v || ord r v u)
