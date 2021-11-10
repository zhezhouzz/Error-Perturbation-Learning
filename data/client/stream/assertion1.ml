let preds = [| "hd"; "mem"; "ord" |]

let op_pool =
  [|
    "insert";
    "replace";
    "cons";
    "append";
    "plus1";
    "minus1";
    "top";
    "bottom";
    "max";
    "min";
    "const0";
    "const1";
  |]

let libs = [| "Stream" |]

let i_err = ([ 3; 4 ], [ 2; 1 ])

let sampling_rounds = 6

let p_size = 4

let pre (acc : Stream.t) (s : Stream.t) (u : int) (v : int) =
  implies (ord acc u v) (u < v)
  && implies (ord s u v) (v < u)
  && implies (mem acc u && mem s v) (v < u)

let post (acc : Stream.t) (s : Stream.t) (nu : Stream.t) (u : int) (v : int) =
  implies (ord nu u v) (u < v)
