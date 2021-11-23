let preds = [| "hd"; "mem"; "<" |]

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

let libs = [| "List" |]

let i_err = ([ -1; 1; 2; 5; 7 ], [ 0; 3; 4; 6; 8 ])

let sampling_rounds = 14

let p_size = 4

let pre (l1 : List.t) (l2 : List.t) (u : int) (v : int) =
  implies (ord l1 u v) (u < v) && implies (ord l2 u v) (u < v)

let post (l1 : List.t) (l2 : List.t) (l3 : List.t) (u : int) (v : int) =
  implies (ord l3 u v) (u < v)
