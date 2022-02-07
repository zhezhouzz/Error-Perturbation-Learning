let preds = [| "hd"; "mem"; "left"; "right"; "<" |]

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

let libs = [| "Tree" |]

let i_err = (0, Node (3, Node (2, NodeS 1, Leaf), Node (5, NodeS 4, NodeS 6)))

let sampling_rounds = 6

let p_size = 4

let pre (t : Tree.t) (t_ : Tree.t) (u : int) (v : int) =
  implies (hd t u && left t u v) (v < u || v == u)
  && (not (hd t u && right t u v))
  && (complete t u || hd t u)
  && implies (hd t_ u && left t_ u v) (v < u || v == u)
  && (not (hd t_ u && right t_ u v))
  && (complete t_ u || hd t_ u)
  && depth_eq t t_

let post (t : Tree.t) (t_ : Tree.t) (nu : Tree.t) (u : int) (v : int) =
  implies (hd nu u && left nu u v) (v < u || v == u)
  && (not (hd nu u && right nu u v))
  && (complete nu u || hd nu u)
  && depth_plus1 t nu
