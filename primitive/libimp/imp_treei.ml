open Value
open Basic_dt
open Imp
open Imp_helper

let table =
  [
    (* treei lib *)
    {
      imp_name = "treei_leaf";
      imp_itps = [];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [] -> Some [ TI LabeledTree.Leaf ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_node";
      imp_itps = [ Int; Int; IntTreeI; IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ I label; I a; TI b; TI c ] ->
            Some [ TI (LabeledTree.Node (label, a, b, c)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_leaf_rev";
      imp_itps = [ IntTreeI ];
      imp_otps = [];
      nondet = false;
      imp_exec =
        (function
        | [ TI LabeledTree.Leaf ] -> Some []
        | [ TI _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_node_rev";
      imp_itps = [ IntTreeI ];
      imp_otps = [ Int; Int; IntTreeI; IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ TI (LabeledTree.Node (label, a, b, c)) ] ->
            Some [ I label; I a; TI b; TI c ]
        | [ TI _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_make_tree";
      imp_itps = [ Int; IntTreeI; IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ I a; TI b; TI c ] -> Some [ TI (LabeledTree.Node (0, a, b, c)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    (* tree manipulation *)
    {
      imp_name = "treei_left_right_subtree";
      imp_itps = [ IntTreeI ];
      imp_otps = [ IntTreeI; IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ TI (LabeledTree.Node (_, _, b, c)) ] -> Some [ TI b; TI c ]
        | [ TI LabeledTree.Leaf ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_root";
      imp_itps = [ IntTreeI ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TI (LabeledTree.Node (_, a, _, _)) ] -> Some [ I a ]
        | [ TI LabeledTree.Leaf ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_flip";
      imp_itps = [ IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] -> Some [ TI (LabeledTree.flip tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_rec_flip";
      imp_itps = [ IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] -> Some [ TI (LabeledTree.rec_flip tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_rotation_left";
      imp_itps = [ IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] ->
            Sugar.(
              let* tr' = LabeledTree.rotation_left_opt tr in
              Some [ TI tr' ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_rotation_right";
      imp_itps = [ IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] ->
            Sugar.(
              let* tr' = LabeledTree.rotation_right_opt tr in
              Some [ TI tr' ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_append_to_left_most";
      imp_itps = [ Int; Int; IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ I label; I x; TI tr ] ->
            Some [ TI (LabeledTree.append_to_left_most_label label x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_append_to_right_most";
      imp_itps = [ Int; Int; IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ I label; I x; TI tr ] ->
            Some [ TI (LabeledTree.append_to_right_most_label label x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_max";
      imp_itps = [ IntTreeI ];
      imp_otps = [ Int; Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] ->
            Sugar.(
              let* b, e = LabeledTree.max_opt Stdlib.compare tr in
              Some [ I b; I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_min";
      imp_itps = [ IntTreeI ];
      imp_otps = [ Int; Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] ->
            Sugar.(
              let* b, e = LabeledTree.min_opt Stdlib.compare tr in
              Some [ I b; I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_upper_bound";
      imp_itps = [ IntTreeI ];
      imp_otps = [ Int; Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] -> (
            match LabeledTree.max_opt Stdlib.compare tr with
            | None -> Some [ I 0; I 0 ]
            | Some (label, m) -> Some [ I label; I (m + 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_lower_bound";
      imp_itps = [ IntTreeI ];
      imp_otps = [ Int; Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] -> (
            match LabeledTree.min_opt Stdlib.compare tr with
            | None -> Some [ I 0; I 0 ]
            | Some (label, m) -> Some [ I label; I (m - 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_drop_bottom";
      imp_itps = [ IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] -> Some [ TI (LabeledTree.drop_bottom tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_destruct";
      imp_itps = [ IntTreeI ];
      imp_otps = [ Int; Int; IntTreeI; IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] -> (
            match tr with
            | LabeledTree.Leaf -> None
            | LabeledTree.Node (label, x, a, b) ->
                Some [ I label; I x; TI a; TI b ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_makeT";
      imp_itps = [ Int; IntTreeI; IntTreeI ];
      imp_otps = [ IntTreeI ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; TI tr1; TI tr2 ] -> Some [ TI (LabeledTree.makeT x tr1 tr2) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
