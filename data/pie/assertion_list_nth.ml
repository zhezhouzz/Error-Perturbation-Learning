let preds = [| "list_hd" |]

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

let i_err = ([ -1; 1; 2; 5; 7; 10; 12; 13 ], 10)

let sampling_rounds = 10

let p_size = 4

let pre (l : List.t) (i : int) = true

let post (l : List.t) (i : int) (nu : int) = true
