open Value
open Basic_dt
open Imp
open Imp_helper

let table =
  [
    (* treeb lib *)
    {
      imp_name = "treeb_leaf";
      imp_itps = [];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [] -> Some [ TB LabeledTree.Leaf ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_node";
      imp_itps = [ Bool; Int; IntTreeB; IntTreeB ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ B label; I a; TB b; TB c ] ->
            Some [ TB (LabeledTree.Node (label, a, b, c)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_node_elrond";
      imp_itps = [ Bool; IntTreeB; Int; IntTreeB ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ B label; TB b; I a; TB c ] ->
            Some [ TB (LabeledTree.Node (label, a, b, c)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_leaf_rev";
      imp_itps = [ IntTreeB ];
      imp_otps = [];
      nondet = false;
      imp_exec =
        (function
        | [ TB LabeledTree.Leaf ] -> Some []
        | [ TB _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_node_rev";
      imp_itps = [ IntTreeB ];
      imp_otps = [ Bool; Int; IntTreeB; IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ TB (LabeledTree.Node (label, a, b, c)) ] ->
            Some [ B label; I a; TB b; TB c ]
        | [ TB _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    (* tree manipulation *)
    {
      imp_name = "treeb_left_right_subtree";
      imp_itps = [ IntTreeB ];
      imp_otps = [ IntTreeB; IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ TB (LabeledTree.Node (_, _, b, c)) ] -> Some [ TB b; TB c ]
        | [ TB LabeledTree.Leaf ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_root";
      imp_itps = [ IntTreeB ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TB (LabeledTree.Node (_, a, _, _)) ] -> Some [ I a ]
        | [ TB LabeledTree.Leaf ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_flip";
      imp_itps = [ IntTreeB ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] -> Some [ TB (LabeledTree.flip tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_rec_flip";
      imp_itps = [ IntTreeB ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] -> Some [ TB (LabeledTree.rec_flip tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_rotation_left";
      imp_itps = [ IntTreeB ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] ->
            Sugar.(
              let* tr' = LabeledTree.rotation_left_opt tr in
              Some [ TB tr' ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_rotation_right";
      imp_itps = [ IntTreeB ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] ->
            Sugar.(
              let* tr' = LabeledTree.rotation_right_opt tr in
              Some [ TB tr' ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_append_to_left_most";
      imp_itps = [ Bool; Int; IntTreeB ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ B label; I x; TB tr ] ->
            Some [ TB (LabeledTree.append_to_left_most_label label x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_append_to_right_most";
      imp_itps = [ Bool; Int; IntTreeB ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ B label; I x; TB tr ] ->
            Some [ TB (LabeledTree.append_to_right_most_label label x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_max";
      imp_itps = [ IntTreeB ];
      imp_otps = [ Bool; Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] ->
            Sugar.(
              let* b, e = LabeledTree.max_opt Stdlib.compare tr in
              Some [ B b; I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_min";
      imp_itps = [ IntTreeB ];
      imp_otps = [ Bool; Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] ->
            Sugar.(
              let* b, e = LabeledTree.min_opt Stdlib.compare tr in
              Some [ B b; I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_upper_bound";
      imp_itps = [ IntTreeB ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] -> (
            match LabeledTree.max_opt Stdlib.compare tr with
            | None -> Some [ I 0 ]
            | Some (_, m) -> Some [ I (m + 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_lower_bound";
      imp_itps = [ IntTreeB ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] -> (
            match LabeledTree.min_opt Stdlib.compare tr with
            | None -> Some [ I 0 ]
            | Some (_, m) -> Some [ I (m - 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_drop_bottom";
      imp_itps = [ IntTreeB ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] -> Some [ TB (LabeledTree.drop_bottom tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_destruct";
      imp_itps = [ IntTreeB ];
      imp_otps = [ Int; IntTreeB; IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ TB tr ] -> (
            match tr with
            | LabeledTree.Leaf -> None
            | LabeledTree.Node (_, x, a, b) -> Some [ I x; TB a; TB b ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "treeb_single";
      imp_itps = [ Bool; Int ];
      imp_otps = [ IntTreeB ];
      nondet = false;
      imp_exec =
        (function
        | [ B b; I i ] ->
            Some
              [
                TB (LabeledTree.Node (b, i, LabeledTree.Leaf, LabeledTree.Leaf));
              ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
