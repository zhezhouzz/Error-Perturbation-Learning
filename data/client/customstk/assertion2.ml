let preds = [| "hd"; "mem"; "ord" |]

let op_pool =
  [|
    "cons";
    "append";
    "list_destruct";
    "list_last_destruct";
    "list_mid_partition";
    "list_alter_partition";
    "list_upper_bound";
    "list_lower_bound";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Customstk" |]

let i_err = ([ -10; -7; -6; -5 ], [ -5; 0; 4; 5; 7; 8 ])

let sampling_rounds = 14

let p_size = 5

let pre (l1 : List.t) (l2 : List.t) (u : int) (v : int) =
  implies (ord l1 u v) (u < v)
  && implies (ord l2 u v) (u < v)
  && implies (mem l1 u && mem l2 v) (u < v || u == v)

let post (l1 : List.t) (l2 : List.t) (l3 : List.t) (u : int) (v : int) =
  implies (ord l3 u v) (u < v)
