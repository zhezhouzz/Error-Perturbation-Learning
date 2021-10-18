let preds = [|"hd"; "mem"; "<"|]
let op_pool = [|"insert"; "replace"; "cons"; "append"; "plus1";
                "minus1"; "top"; "bottom"; "max"; "min"; "random_int";
                "const0"; "const1";|]
let libs = [|"List"|]
let i_err = [1;2], [3;4]
let sampling_rounds = 6
let p_size = 4
let pre (l1: List.t) (l2: List.t) =
  fun (u: int) (v: int) ->
  (implies (ord l1 u v) (u < v)) && (implies (ord l2 u v) (u < v))
let post (l1: List.t) (l2: List.t) (l3: List.t) =
  fun (u: int) (v: int) ->
  (implies (ord l3 u v) (u < v))
