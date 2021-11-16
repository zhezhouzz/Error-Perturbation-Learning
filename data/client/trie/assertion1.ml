let preds = [| "hd"; "mem"; "ord" |]

let op_pool =
  [|
    (* list   *)
    "insert";
    "replace";
    "cons";
    "append";
    "top";
    "bottom";
    "max";
    "min";
    (* tree *)
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
    (* int   *)
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Trie" |]

let i_err =
  ( 0,
    [ 1; 0 ],
    3,
    Node (0, NodeS 0, Node (1, Node (0, NodeS 0, NodeS 1), NodeS 1)) )

let sampling_rounds = 6

let p_size = 4

let pre (default : int) (i : Trie.tp) (a : int) (m : Trie.t) (u : int) (v : int)
    =
  (not (mem m a))
  && implies (para_adj m u v) (not (u == v))
  && implies (len m u && len i v) (v < u)

let post (default : int) (i : Trie.tp) (a : int) (m : Trie.t) (nu : Trie.t)
    (u : int) (v : int) =
  implies (len nu u && len m v) (u == v)
  && implies (mem nu u) (mem m u || u == default || u == a)
  (* && implies (left m u v) (left nu u v) *)
  && implies (para_adj m u v) (not (u == v))
  && implies (para m u v) (para nu u v)
