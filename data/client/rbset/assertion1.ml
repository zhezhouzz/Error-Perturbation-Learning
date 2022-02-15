let preds =
  [|
    "hd";
    "last";
    "left_adj";
    "right_adj";
    "label_is_true";
    "rb_blance";
    "rb_blance2";
    "left";
    "right";
    "<";
  |]

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

let pre (label : bool) (x : int) (tree1 : Rbset.t) (tree2 : Rbset.t) (u : int)
    (v : int) =
  implies
    ((left_adj tree1 u v || right_adj tree1 u v)
    && label_is_true tree1 u && label_is_true tree1 v)
    (hd tree1 u)
  && implies
       ((left_adj tree2 u v || right_adj tree2 u v)
       && label_is_true tree2 u && label_is_true tree2 v)
       (hd tree2 u)
  && rb_balance2 tree1 tree2
  && implies (right tree1 v u) (v < u)
  && implies (left tree1 v u) (u < v)
  && implies (right tree2 v u) (v < u)
  && implies (left tree2 v u) (u < v)
  && implies (mem tree1 u) (u < x)
  && implies (mem tree2 u) (x < u)

let post (label : bool) (x : int) (tree1 : Rbset.t) (tree2 : Rbset.t)
    (nu : Rbset.t) (u : int) (v : int) =
  (not
     ((left_adj nu u v || right_adj nu u v)
     && label_is_true nu u && label_is_true nu v))
  && rb_balance nu
  && implies (right nu v u) (v < u)
  && implies (left nu v u) (u < v)
