open Value
open Basic_dt
open Imp

let exn file line =
  failwith (spf "runtime operator(defined at file %s line %i) error" file line)

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
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] ->
            Sugar.(
              let* e = LabeledTree.max_opt Stdlib.compare tr in
              Some [ I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treei_min";
      imp_itps = [ IntTreeI ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TI tr ] ->
            Sugar.(
              let* e = LabeledTree.min_opt Stdlib.compare tr in
              Some [ I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
