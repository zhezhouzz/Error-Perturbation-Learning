open Value
open Basic_dt
open Imp
open Imp_helper

let table =
  [
    (* tree lib *)
    {
      imp_name = "tree_leaf";
      imp_itps = [];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [] -> Some [ T Tree.Leaf ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_node";
      imp_itps = [ Int; IntTree; IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ I a; T b; T c ] -> Some [ T (Tree.Node (a, b, c)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_node_elrond";
      imp_itps = [ IntTree; Int; IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ T b; I a; T c ] -> Some [ T (Tree.Node (a, b, c)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_leaf_rev";
      imp_itps = [ IntTree ];
      imp_otps = [];
      nondet = false;
      imp_exec =
        (function
        | [ T Tree.Leaf ] -> Some []
        | [ T _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_node_rev";
      imp_itps = [ IntTree ];
      imp_otps = [ Int; IntTree; IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ T (Tree.Node (a, b, c)) ] -> Some [ I a; T b; T c ]
        | [ T _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    (* tree manipulation *)
    {
      imp_name = "tree_left_right_subtree";
      imp_itps = [ IntTree ];
      imp_otps = [ IntTree; IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ T (Tree.Node (_, b, c)) ] -> Some [ T b; T c ]
        | [ T Tree.Leaf ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_root";
      imp_itps = [ IntTree ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ T (Tree.Node (a, _, _)) ] -> Some [ I a ]
        | [ T Tree.Leaf ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_flip";
      imp_itps = [ IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] -> Some [ T (Tree.flip tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_rec_flip";
      imp_itps = [ IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] -> Some [ T (Tree.rec_flip tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_rotation_left";
      imp_itps = [ IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] ->
            Sugar.(
              let* tr' = Tree.rotation_left_opt tr in
              Some [ T tr' ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_rotation_right";
      imp_itps = [ IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] ->
            Sugar.(
              let* tr' = Tree.rotation_right_opt tr in
              Some [ T tr' ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_append_to_left_most";
      imp_itps = [ Int; IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; T tr ] -> Some [ T (Tree.append_to_left_most x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_append_to_right_most";
      imp_itps = [ Int; IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; T tr ] -> Some [ T (Tree.append_to_right_most x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_max";
      imp_itps = [ IntTree ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] ->
            Sugar.(
              let* e = Tree.max_opt Stdlib.compare tr in
              Some [ I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_min";
      imp_itps = [ IntTree ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] ->
            Sugar.(
              let* e = Tree.min_opt Stdlib.compare tr in
              Some [ I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_upper_bound";
      imp_itps = [ IntTree ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] -> (
            match Tree.max_opt Stdlib.compare tr with
            | None -> Some [ I 0 ]
            | Some m -> Some [ I (m + 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_lower_bound";
      imp_itps = [ IntTree ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] -> (
            match Tree.min_opt Stdlib.compare tr with
            | None -> Some [ I 0 ]
            | Some m -> Some [ I (m - 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_add_to_bottom_left";
      imp_itps = [ Int; IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; T tr ] -> Some [ T (Tree.add_to_bottom_left x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_add_to_bottom_right";
      imp_itps = [ Int; IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; T tr ] -> Some [ T (Tree.add_to_bottom_right x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_drop_bottom";
      imp_itps = [ IntTree ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] -> Some [ T (Tree.drop_bottom tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_destruct";
      imp_itps = [ IntTree ];
      imp_otps = [ Int; IntTree; IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ T tr ] ->
            Sugar.(
              let* x, a, b = Tree.destruct_opt tr in
              Some [ I x; T a; T b ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "tree_node_single";
      imp_itps = [ Int ];
      imp_otps = [ IntTree ];
      nondet = false;
      imp_exec =
        (function
        | [ I i ] -> Some [ T (Node (i, Leaf, Leaf)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
