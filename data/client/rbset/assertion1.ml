let preds = [| "hd"; "last"; "mem"; "<" |]

let op_pool =
  [|
    "treeb_node";
    "treeb_destruct";
    "treeb_flip";
    "treeb_rec_flip";
    "treeb_rotation_left";
    "treeb_rotation_right";
    "treeb_append_to_left_most";
    "treeb_append_to_right_most";
    "treeb_max";
    "treeb_min";
    "treeb_upper_bound";
    "treeb_lower_bound";
    "treeb_drop_bottom";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Rbset" |]

let i_err =
  ( false,
    1,
    LNodeS (false, 0),
    LNode
      ( true,
        5,
        LNode (true, 3, LNodeS (false, 2), LNodeS (false, 4)),
        LNodeS (false, 6) ) )

let sampling_rounds = 6

let p_size = 4

let pre (label : bool) (x : int) (tree1 : Rbset.t) (tree2 : Rbset.t) (u : int) =
  is_rb_alt tree1 && is_rb_alt tree2 && rb_balance2 tree1 tree2
  && strict_sort tree1 && strict_sort tree2
  && implies (mem tree1 u) (u < x)
  && implies (mem tree2 u) (x < u)

let post (label : bool) (x : int) (tree1 : Rbset.t) (tree2 : Rbset.t)
    (nu : Rbset.t) =
  is_rb_alt nu && rb_balance nu && strict_sort nu
