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

let libs = [| "Stream" |]

let i_err = ([ -1; 5 ], [ -3; -5; -9 ])

let sampling_rounds = 6

let p_size = 4

let pre (acc : Stream.t) (s : Stream.t) (u : int) (v : int) =
  implies (ord acc u v) (u < v)
  && implies (ord s u v) (v < u)
  && implies (mem acc u && mem s v) (v < u)

let post (acc : Stream.t) (s : Stream.t) (nu : Stream.t) (u : int) (v : int) =
  implies (ord nu u v) (u < v)
