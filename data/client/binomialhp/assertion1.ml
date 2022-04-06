let preds = [| "hd"; "mem"; "last"; "<"; "==" |]

let op_pool = [| "theta_binomialhp"; "theta_int" |]

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
