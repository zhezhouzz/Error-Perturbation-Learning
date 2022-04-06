let preds =
  [| "hd"; "mem"; "last"; "left"; "right"; "left_mem"; "right_mem"; "<"; "==" |]

let op_pool = [| "theta_tree"; "theta_int" |]

let libs = [| "Unbset" |]

let i_err = (0, Node (3, Node (2, NodeS 1, Leaf), Node (5, NodeS 4, NodeS 6)))

let sampling_rounds = 6

let p_size = 4

let pre (x : int) (s : Unbset.t) = strict_sort s

let post (x : int) (s : Unbset.t) (nu : Unbset.t) = strict_sort nu
