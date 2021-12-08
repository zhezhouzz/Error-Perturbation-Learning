let preds = [| "hd"; "mem"; "ord"; "<" |]

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

let i_err = (1, [ 0; 3; 4; 6; 8 ])

let sampling_rounds = 14

let p_size = 4

let pre (x : int) (l : List.t) (u : int) (v : int) =
  (not (mem l x)) && implies (ord l u v && not (hd l u)) (u < v)

let post (x : int) (l : List.t) (nu : List.t) (u : int) (v : int) =
  implies (ord nu u v) (u < v)