let preds = [|"hd"; "mem"; "left"; "right"; "<"|]
let op_pool = [|"tree_node"; "tree_left_right_subtree"; "tree_root";
                "tree_flip"; "tree_rec_flip";
                "tree_rotation_left"; "tree_rotation_right";
                "tree_append_to_left_most"; "tree_append_to_right_most";
                "tree_max"; "tree_min";
                "plus1"; "minus1"; "random_int";
                "const0"; "const1";|]
let libs = [|"Unbset"|]
let i_err = 0, [5;[1;()];[6;()]]
let sampling_rounds = 6
let p_size = 4
let pre (x: int) (s: Unbset.t) =
  fun (u: int) (v: int) ->
  (implies ((hd s v) && (v < u)) (iff (mem s u) (right s v u))) &&
  (implies ((hd s v) && (u < v)) (iff (mem s u) (left s v u)))
let post (x: int) (s: Unbset.t) (nu: Unbset.t) =
  fun (u: int) (v: int) ->
  (implies ((hd nu v) && (v < u)) (iff (mem nu u) (right nu v u))) &&
  (implies ((hd nu v) && (u < v)) (iff (mem nu u) (left nu v u)))
