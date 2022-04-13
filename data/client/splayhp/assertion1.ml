let preds =
  [|
    "mem";
    "hd";
    "last";
    "<";
    "==";
    "left_mem";
    "right_mem";
    "ll_mem";
    "lr_mem";
    "rl_mem";
    "rr_mem";
  |]

let op_pool = [| "theta_tree"; "theta_int" |]

let libs = [| "Splayhp" |]

let i_err =
  ( 15,
    Node
      ( 8,
        Node (6, Node (3, NodeS 1, NodeS 4), NodeS 7),
        Node (13, Node (11, NodeS 10, NodeS 12), NodeS 14) ) )

(* let i_err = *)
(*   (9, Node (8, NodeS 6, Node (13, Node (11, NodeS 10, NodeS 12), NodeS 14))) *)

(* let i_err = (9, Node (8, NodeS 6, Node (13, NodeS 11, NodeS 14))) *)

let sampling_rounds = 6

let p_size = 4

let pre (x : int) (tree1 : Splayhp.t) (u : int) =
  strict_sort tree1 && (not (size1 tree1)) && implies (mem tree1 u) (u < x)

let post (x : int) (tree1 : Splayhp.t) (tree2 : Splayhp.t) (tree3 : Splayhp.t)
    (u : int) =
  strict_sort tree2 && strict_sort tree3
  && iff (mem tree1 u) (mem tree2 u || mem tree3 u)
  && implies (mem tree2 u) (u < x || u == x)
  && implies (mem tree3 u) (x < u)
