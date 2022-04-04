let preds = [| "hd"; "mem"; "last"; "<" |]

let op_pool =
  [|
    "tree_node";
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

let libs = [| "Unbset" |]

let i_err = (0, Node (3, Node (2, NodeS 1, Leaf), Node (5, NodeS 4, NodeS 6)))

let sampling_rounds = 6

let p_size = 4

let pre (x : int) (s : Unbset.t) = strict_sort s

let post (x : int) (s : Unbset.t) (nu : Unbset.t) = strict_sort nu
