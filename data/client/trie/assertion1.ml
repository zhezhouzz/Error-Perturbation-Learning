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
    "tree_left_right_subtree";
    "tree_root";
    "tree_flip";
    "tree_rec_flip";
    "tree_rotation_left";
    "tree_rotation_right";
    "tree_append_to_left_most";
    "tree_append_to_right_most";
    "tree_max";
    "tree_min";
    (* int   *)
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Trie" |]

let i_err =
  (0, [ 3; 4 ], 7, Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)))

let sampling_rounds = 6

let p_size = 4

let post (default : int) (i : Trie.tp) (a : int) (m : Trie.t) (nu : Trie.t)
    (u : int) (v : int) =
  implies (mem nu u) (mem m u || u == default || u == a)
  && implies (para m u v) (para nu u v)
