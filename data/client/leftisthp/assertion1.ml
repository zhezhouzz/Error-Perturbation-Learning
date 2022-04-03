let preds =
  [|
    "hd";
    "last";
    "left_adj";
    "right_adj";
    "label_is_true";
    "leftists";
    "left";
    "right";
    "<";
  |]

let op_pool =
  [|
    "treei_node";
    "treei_destruct";
    "treei_flip";
    "treei_rec_flip";
    "treei_rotation_left";
    "treei_rotation_right";
    "treei_append_to_left_most";
    "treei_append_to_right_most";
    "treei_max";
    "treei_min";
    "treei_upper_bound";
    "treei_lower_bound";
    "treei_drop_bottom";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Leftisthp" |]

let i_err =
  ( LNodeS (1, 0),
    LNode (3, 5, LNode (2, 3, LNodeS (1, 2), LNodeS (1, 4)), LNodeS (1, 6)) )

let sampling_rounds = 6

let p_size = 4

let pre (h1 : Leftisthp.t) (h2 : Leftisthp.t) =
  leftist h1 && leftist h2 && strict_sort h1 && strict_sort h2

let post (h1 : Leftisthp.t) (h2 : Leftisthp.t) (nu : Leftisthp.t) =
  leftist nu && strict_sort nu
