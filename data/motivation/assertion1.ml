let preds = [| "hd"; "mem"; "<"; "ord" |]

let op_pool =
  [|
    "append";
    "cons";
    "list_destruct";
    "list_head";
    "list_last";
    "plus1";
    "minus1";
    "list_upper_bound";
    "list_lower_bound";
    "min";
    "max";
  |]

let libs = [| "List" |]

let i_err = ([ 1; 2 ], [ 3; 4 ])

let sampling_rounds = 10

let p_size = 4

let pre (l1 : List.t) (l2 : List.t) (u : int) (v : int) =
  implies (ord l1 u v) (u < v) && implies (ord l2 u v) (u < v)

let post (l1 : List.t) (l2 : List.t) (l3 : List.t) (u : int) (v : int) =
  implies (ord l3 u v) (u < v)
