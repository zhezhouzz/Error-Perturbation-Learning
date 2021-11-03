let preds = [| "hd"; "ord" |]

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
    "random_int";
    "const0";
    "const1";
  |]

let libs = [| "Bankersq" |]

let i_err = (2, [ 1; 2 ], 1, [ 3 ], 4)

let sampling_rounds = 6

let p_size = 4

let pre (lenf : int) (f : Bankersq.t) (lenr : int) (r : Bankersq.t) (x : int) =
  lenr < lenf && size f lenf && size r lenr

let post (lenf : int) (f : Bankersq.t) (lenr : int) (r : Bankersq.t) (x : int)
    (lenf' : int) (f' : Bankersq.t) (lenr' : int) (r' : Bankersq.t) (u : int) =
  lenr' < lenf' && size f' lenf' && size r' lenr'
  && iff
       (mem f u || mem r u)
       ((mem f' u && mem r' x) || ord r' x u || ord f' u x)
