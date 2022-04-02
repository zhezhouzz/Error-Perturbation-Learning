let preds = [| "hd"; "mem"; "ord" |]

let op_pool =
  [|
    "replace";
    "insert";
    "cons";
    "append";
    "list_destruct";
    "list_last_destruct";
    "list_mid_partition";
    "list_alter_partition";
    "max";
    "min";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Stream" |]

let i_err = ([ -1; 5; 6; 10; 11 ], [ -3; -5; -9; -10; -11 ])

let sampling_rounds = 15

let p_size = 4

let pre (acc : Stream.t) (s : Stream.t) (u : int) (v : int) =
  strict_sort acc && strict_sort_rev s && implies (mem acc u && mem s v) (v < u)

let post (acc : Stream.t) (s : Stream.t) (nu : Stream.t) = strict_sort nu
