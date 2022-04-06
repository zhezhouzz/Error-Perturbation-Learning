let preds = [| "hd"; "mem"; "last"; "<"; "==" |]

let op_pool = [| "theta_pairinghp"; "theta_int" |]

let libs = [| "Pairinghp" |]

let i_err =
  ( PNode (1, PCons (PNodeS 2, PCons (PNodeS 3, PNil))),
    PNode (4, PCons (PNodeS 5, PCons (PNodeS 6, PNil))) )

let sampling_rounds = 6

let p_size = 4

let pre (ts1 : Pairinghp.t) (ts2 : Pairinghp.t) =
  pairinghp ts1 && pairinghp ts2 && pairinghp_sort ts1 && pairinghp_sort ts2

let post (ts1 : Pairinghp.t) (ts2 : Pairinghp.t) (nu : Pairinghp.t) (u : int) =
  pairinghp nu && iff (mem ts1 u || mem ts2 u) (mem nu u) && pairinghp_sort nu
