let preds = [| "hd"; "mem"; "last"; "<"; "==" |]

let op_pool = [| "theta_treei"; "theta_int" |]

let libs = [| "Leftisthp" |]

let i_err =
  ( LNodeS (1, 0),
    LNode (3, 5, LNode (2, 3, LNodeS (1, 2), LNodeS (1, 4)), LNodeS (1, 6)) )

let sampling_rounds = 6

let p_size = 4

let pre (h1 : Leftisthp.t) (h2 : Leftisthp.t) =
  leftist h1 && leftist h2 && strict_sort h1 && strict_sort h2

let post (h1 : Leftisthp.t) (h2 : Leftisthp.t) (nu : Leftisthp.t) =
  leftist nu && strict_sort nu
