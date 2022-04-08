let preds = [| "hd"; "last"; "mem"; "<"; "==" |]

let op_pool = [| "theta_treeb"; "theta_int" |]

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

(* let i_err = *)
(*   ( false, *)
(*     1, *)
(*     LNodeS (false, 0), *)
(*     LNode (true, 5, LNodeS (false, 3), LNodeS (false, 6)) ) *)

let sampling_rounds = 6

let p_size = 4

let pre (label : bool) (x : int) (tree1 : Rbset.t) (tree2 : Rbset.t) (u : int) =
  is_rb_alt tree1 && is_rb_alt tree2 && rb_balance2 tree1 tree2
  && strict_sort tree1 && strict_sort tree2
  && implies (mem tree1 u) (u < x)
  && implies (mem tree2 u) (x < u)
  && (not (empty tree1))
  && not (empty tree2)

let post (label : bool) (x : int) (tree1 : Rbset.t) (tree2 : Rbset.t)
    (nu : Rbset.t) =
  is_rb_alt nu && rb_balance nu && strict_sort nu
