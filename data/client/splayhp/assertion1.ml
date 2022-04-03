let preds = [| "mem" |]

let op_pool =
  [|
    "tree_node";
    "tree_node_single";
    "tree_destruct";
    "tree_flip";
    "tree_rec_flip";
    "tree_rotation_left";
    "tree_rotation_right";
    "tree_append_to_left_most";
    "tree_append_to_right_most";
    "tree_max";
    "tree_min";
    "tree_upper_bound";
    "tree_lower_bound";
    "tree_drop_bottom";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Splayhp" |]

let i_err =
  (* (9, Node (8, NodeS 6, Node (13, Node (11, NodeS 10, NodeS 12), NodeS 14))) *)
  ( 9,
    Node
      ( 8,
        Node (6, Node (3, NodeS 1, NodeS 4), NodeS 7),
        Node (13, Node (11, NodeS 10, NodeS 12), NodeS 14) ) )

let sampling_rounds = 6

let p_size = 4

let pre (x : int) (tree1 : Splayhp.t) = strict_sort tree1

let post (x : int) (tree1 : Splayhp.t) (tree2 : Splayhp.t) (tree3 : Splayhp.t)
    (u : int) =
  strict_sort tree2 && strict_sort tree3
  && iff (mem tree1 u) (mem tree2 u || mem tree3 u)
  && implies (mem tree2 u) (u < x || u == x)
  && implies (mem tree3 u) (x < u)
