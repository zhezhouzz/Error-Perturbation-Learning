let preds = [| "hd"; "mem"; "<" |]

let op_pool =
  [|
    "append";
    "cons";
    "list_destruct";
    "list_head";
    "list_tail";
    "list_last";
    "plus";
    "minus";
    "const0";
    "const1";
  |]

let libs = [| "List" |]

let i_err = ([ 1; 2 ], [ 3; 4 ])

let sampling_rounds = 14

let p_size = 4

let pre (l1 : List.t) (l2 : List.t) (u : int) (v : int) =
  implies (ord l1 u v) (u < v) && implies (ord l2 u v) (u < v)

let post (l1 : List.t) (l2 : List.t) (l3 : List.t) (u : int) (v : int) =
  implies (ord l3 u v) (u < v)
