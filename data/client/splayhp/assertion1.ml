let preds = [|"mem";|]
let op_pool = [|"tree_node"; "tree_left_right_subtree"; "tree_root";
                "tree_flip"; "tree_rec_flip";
                "tree_rotation_left"; "tree_rotation_right";
                "tree_append_to_left_most"; "tree_append_to_right_most";
                "tree_max"; "tree_min";
                "plus1"; "minus1"; "random_int";
                "const0"; "const1";|]
let libs = [|"Splayhp"|]
let i_err = 5, [3; [2; [()];[1;()]];[6; [4;()];[()]]]
let sampling_rounds = 6
let p_size = 4
let post (x: int) (tree1: Splayhp.t) (tree2: Splayhp.t) (tree3: Splayhp.t) =
  fun (u: int) ->
  (iff (mem tree1 u) (mem tree2 u || mem tree3 u))