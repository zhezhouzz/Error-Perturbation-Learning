let preds = [| "last"; "ord"; "<"; "==" |]

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

let libs = [| "List" |]

let i_err = (7, [ 0; 3; 4; 6; 5 ])

let sampling_rounds = 20

let p_size = 4

let pre (x : int) (l : List.t) (u : int) (v : int) =
  (not (mem l x)) && implies ((not (last l v)) && ord l u v) (u < v)

let post (x : int) (l : List.t) (nu : List.t) (u : int) (v : int) =
  implies (ord nu u v) (u < v)
