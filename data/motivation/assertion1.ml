let preds = [| "hd"; "mem"; "<"; "ord" |]

let raw_op_pool =
  [|
    "append";
    "cons";
    "list_destruct";
    "list_head";
    "list_last";
    "const0";
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

let pre (l1 : List.t) (l2 : List.t) = strict_sort l1 && strict_sort l2

let post (l1 : List.t) (l2 : List.t) (l3 : List.t) = strict_sort l3
