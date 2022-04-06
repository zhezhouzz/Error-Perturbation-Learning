let preds = [| "hd"; "ord"; "mem"; "<"; "==" |]

let op_pool = [| "theta_list"; "theta_int" |]

let libs = [| "Bankersq" |]

(* let i_err = (2, [ 1; 2 ], 1, [ 3 ], 4) *)
let i_err = (1, [ 1 ], 0, [], 4)

let sampling_rounds = 6

let p_size = 4

let pre (lenf : nat) (f : Bankersq.t) (lenr : nat) (r : Bankersq.t) (x : int) =
  lenr < lenf && size f lenf && size r lenr

let post (lenf : nat) (f : Bankersq.t) (lenr : nat) (r : Bankersq.t) (x : int)
    (lenf' : nat) (f' : Bankersq.t) (lenr' : nat) (r' : Bankersq.t) (u : int) =
  lenr' < lenf' && size f' lenf' && size r' lenr'
  && iff (mem f u || mem r u || u == x) (mem f' u && mem r' u)
  && iff
       (mem f u || mem r u)
       ((mem f' u && mem r' x) || ord r' x u || ord f' u x)
