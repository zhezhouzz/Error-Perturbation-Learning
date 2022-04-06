let preds = [| "hd"; "last"; "mem"; "<"; "==" |]

let op_pool = [| "theta_skewhp" |]

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
