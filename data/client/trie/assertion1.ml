let preds = [| "hd"; "mem"; "last"; "<"; "==" |]

let op_pool = [| "theta_list"; "theta_tree"; "theta_int" |]

let libs = [| "Trie" |]

let i_err =
  ( 0,
    [ 1; 0 ],
    3,
    Node (0, NodeS 0, Node (1, Node (0, NodeS 0, NodeS 1), NodeS 1)) )

(* let i_err = (0, [ 0 ], 2, Node (1, Node (0, NodeS 0, NodeS 1), NodeS 1)) *)

let sampling_rounds = 6

let p_size = 4

let pre (default : int) (i : Trie.tp) (a : int) (m : Trie.t) =
  (not (mem m a)) && children_diff m && less_len i m

let post (default : int) (i : Trie.tp) (a : int) (m : Trie.t) (nu : Trie.t)
    (u : int) =
  eq_len nu m
  && implies (mem nu u) (mem m u || u == default || u == a)
  (* && implies (left m u v) (left nu u v) *)
  && children_diff nu
  && prefix m nu
