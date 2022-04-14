let preds = [| "hd"; "mem"; "ord"; "<"; "==" |]

let op_pool = [| "theta_list"; "theta_int" |]

let libs = [| "Uniquel" |]

let i_err = (0, [ 1; 2; 3; 4 ])

(* let i_err = (2, [ 1; 2 ]) *)
(* let i_err = (1, [ 2; 3 ]) *)

let sampling_rounds = 6

let p_size = 4

let pre (x : int) (l1 : Uniquel.t) (l2 : Uniquel.t) (u : int) =
  implies (mem l1 u) (x < u || u == x) && uniq l1

let post (x : int) (l1 : Uniquel.t) (l2 : Uniquel.t) (u : int) =
  uniq l2 && iff (mem l2 u) (mem l1 u || u == x)
