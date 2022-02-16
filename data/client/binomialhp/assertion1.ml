let preds = [| "binomialhp" |]

let op_pool =
  [|
    "binomialhp_list_last_destruct";
    "binomialhp_list_destruct";
    "binomialhp_list_cons";
    "binomialhp_list_append";
    "binomialhp_list_single";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Binomialhp" |]

let i_err =
  ( LNodeS (1, 0),
    LNode (3, 5, LNode (2, 3, LNodeS (1, 2), LNodeS (1, 4)), LNodeS (1, 6)) )

let sampling_rounds = 6

let p_size = 4

let pre (ts1 : Binomialhp.t) (ts2 : Binomialhp.t) =
  binomialhp ts1 && binomialhp ts2

let post (ts1 : Binomialhp.t) (ts2 : Binomialhp.t) (nu : Binomialhp.t) =
  binomialhp nu
