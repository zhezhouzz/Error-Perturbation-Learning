let preds = [| "hd"; "mem"; "ord" |]

let op_pool =
  [|
    "replace";
    "insert";
    "cons";
    "append";
    "list_destruct";
    "list_mid_partition";
    "list_alter_partition";
    "bottom";
    "max";
    "min";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Uniquel" |]

let i_err = (2, [ 1; 2; 3; 4 ])

let sampling_rounds = 6

let p_size = 4

let pre (x : int) (l1 : Uniquel.t) (l2 : Uniquel.t) (u : int) =
  implies (mem l1 u) (once l1 u)

let post (x : int) (l1 : Uniquel.t) (l2 : Uniquel.t) (u : int) =
  implies (mem l2 u) (once l2 u) && iff (mem l2 u) (mem l1 u || u == x)
