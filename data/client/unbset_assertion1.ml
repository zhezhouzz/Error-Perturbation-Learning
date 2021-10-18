let preds = [|"hd"; "mem"; "left"; "right"; "<"|]
let op_pool = [|"insert"; "replace"; "cons"; "append"; "plus1";
                "minus1"; "top"; "bottom"; "max"; "min"; "random_int";
                "const0"; "const1";|]
let libs = [|"List"|]
let i_err = [1;2;3], [2;[1;()];[3;()]]
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
