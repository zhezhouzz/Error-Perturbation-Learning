let preds = [| "hd"; "mem"; "ord"; "<"; "==" |]

let op_pool = [| "theta_list"; "theta_int" |]

let libs = [| "Uniquel" |]

(* let i_err = (2, [ 1; 2; 3; 4 ]) *)

(* let i_err = (2, [ 1; 2 ]) *)
let i_err = (1, [ 2; 3 ])

let sampling_rounds = 6

let p_size = 4

let pre (x : int) (l1 : Uniquel.t) (l2 : Uniquel.t) (u : int) =
  implies (mem l1 u) (once l1 u)

let post (x : int) (l1 : Uniquel.t) (l2 : Uniquel.t) (u : int) =
  implies (mem l2 u) (once l2 u) && iff (mem l2 u) (mem l1 u || u == x)
