let preds = [| "binomialhp"; "mem" |]

let op_pool =
  [|
    "binomialhp_list_last_destruct";
    "binomialhp_list_destruct";
    "binomialhp_list_cons";
    "binomialhp_list_append";
    "binomialhp_single";
    "binomialhp_top";
    "binomialhp_lower_bound";
    "binomialhp_upper_bound";
    "binomialt_head";
    "binomialt_head_update";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Binomialhp" |]

let i_err =
  ( BiCons (BiNode (1, 0, BiNodeS (0, 2)), BiNil),
    BiCons (BiNode (1, 4, BiNodeS (0, 5)), BiNil) )
(* (BiNodeS (0, 0), BiNodeS (0, 2)) *)

let sampling_rounds = 6

let p_size = 4

let pre (ts1 : Binomialhp.t) (ts2 : Binomialhp.t) =
  binomialhp ts1 && binomialhp ts2

let post (ts1 : Binomialhp.t) (ts2 : Binomialhp.t) (nu : Binomialhp.t) (u : int)
    =
  binomialhp nu && iff (mem ts1 u || mem ts2 u) (mem nu u)
