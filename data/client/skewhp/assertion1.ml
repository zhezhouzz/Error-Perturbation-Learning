let preds = [| "skewhp"; "mem" |]

let op_pool =
  [|
    "skewhp_list_last_destruct";
    "skewhp_list_destruct";
    "skewhp_list_cons";
    "skewhp_list_append";
    "skewhp_single";
    "skewhp_top";
    "skewhp_lower_bound";
    "skewhp_upper_bound";
    "skewt_head";
    "skewt_head_update";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Skewhp" |]

let i_err =
  ( SkCons (SkNode (1, 0, [ 1 ], SkNodeS (0, 2, [])), SkNil),
    SkCons (SkNode (1, 5, [ 6 ], SkNodeS (0, 7, [])), SkNil) )
(* (SkNodeS (0, 0), SkNodeS (0, 2)) *)

let sampling_rounds = 6

let p_size = 4

let pre (ts1 : Skewhp.t) (ts2 : Skewhp.t) = skewhp ts1 && skewhp ts2

let post (ts1 : Skewhp.t) (ts2 : Skewhp.t) (nu : Skewhp.t) (u : int) =
  skewhp nu && iff (mem ts1 u || mem ts2 u) (mem nu u)
