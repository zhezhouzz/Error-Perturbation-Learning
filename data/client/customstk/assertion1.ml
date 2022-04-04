let preds = [| "hd"; "mem"; "ord"; "<" |]

let op_pool =
  [|
    "replace";
    "insert";
    "cons";
    "append";
    "list_destruct";
    "list_last_destruct";
    "list_mid_partition";
    "list_alter_partition";
    (* "list_lower_bound"; *)
    (* "list_upper_bound"; *)
    "max";
    "min";
    "plus1";
    "minus1";
    "const0";
    "const1";
  |]

let libs = [| "Customstk" |]

let i_err = ([ -10; -5; -4; 0; 3 ], [ -8; -5; 4; 5 ])
(* let i_err = ([ 1; 2 ], [ 3; 4 ]) *)

let sampling_rounds = 14

let p_size = 4

let pre (l1 : List.t) (l2 : List.t) = strict_sort l1 && strict_sort l2

let post (l1 : List.t) (l2 : List.t) (l3 : List.t) = strict_sort l3
