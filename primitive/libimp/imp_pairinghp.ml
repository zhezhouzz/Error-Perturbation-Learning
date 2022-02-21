open Value
open Basic_dt
open Imp
open Imp_helper

let table =
  [
    (* pairinghp lib *)
    {
      imp_name = "pairinghp_leaf";
      imp_itps = [];
      imp_otps = [ Uninterp "pairinghp" ];
      nondet = false;
      imp_exec =
        (function
        | [] -> Some [ Pairinghp Pairinghp.E ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_node";
      imp_itps = [ Int; Uninterp "pairingl" ];
      imp_otps = [ Uninterp "pairinghp" ];
      nondet = false;
      imp_exec =
        (function
        | [ I a; Pairingl b ] -> Some [ Pairinghp (Pairinghp.T (a, b)) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_cons";
      imp_itps = [ Uninterp "pairinghp"; Uninterp "pairingl" ];
      imp_otps = [ Uninterp "pairingl" ];
      nondet = false;
      imp_exec =
        (function
        | [ Pairinghp a; Pairingl b ] -> Some [ Pairingl (a :: b) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_leaf_rev";
      imp_itps = [ Uninterp "pairinghp" ];
      imp_otps = [];
      nondet = false;
      imp_exec =
        (function
        | [ Pairinghp Pairinghp.E ] -> Some []
        | [ Pairinghp _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_node_rev";
      imp_itps = [ Uninterp "pairinghp" ];
      imp_otps = [ Int; Uninterp "pairingl" ];
      nondet = false;
      imp_exec =
        (function
        | [ Pairinghp (Pairinghp.T (a, b)) ] -> Some [ I a; Pairingl b ]
        | [ Pairinghp _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_cons_rev";
      imp_itps = [ Uninterp "pairingl" ];
      imp_otps = [ Uninterp "pairinghp"; Uninterp "pairingl" ];
      nondet = false;
      imp_exec =
        (function
        | [ Pairingl (a :: b) ] -> Some [ Pairinghp a; Pairingl b ]
        | [ Pairinghp _ ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    (* tree manipulation *)
    {
      imp_name = "pairinghp_append_to_left_most";
      imp_itps = [ Int; Uninterp "pairinghp" ];
      imp_otps = [ Uninterp "pairinghp" ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; Pairinghp tr ] ->
            Some [ Pairinghp (Pairinghp.append_to_left_most_label x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_append_to_right_most";
      imp_itps = [ Int; Uninterp "pairinghp" ];
      imp_otps = [ Uninterp "pairinghp" ];
      nondet = false;
      imp_exec =
        (function
        | [ I x; Pairinghp tr ] ->
            Some [ Pairinghp (Pairinghp.append_to_right_most_label x tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_max";
      imp_itps = [ Uninterp "pairinghp" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Pairinghp tr ] ->
            Sugar.(
              let* e = Pairinghp.max_opt tr in
              Some [ I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_min";
      imp_itps = [ Uninterp "pairinghp" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Pairinghp tr ] ->
            Sugar.(
              let* e = Pairinghp.min_opt tr in
              Some [ I e ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_upper_bound";
      imp_itps = [ Uninterp "pairinghp" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Pairinghp tr ] -> (
            match Pairinghp.max_opt tr with
            | None -> Some [ I 0 ]
            | Some m -> Some [ I (m + 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_lower_bound";
      imp_itps = [ Uninterp "pairinghp" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Pairinghp tr ] -> (
            match Pairinghp.min_opt tr with
            | None -> Some [ I 0 ]
            | Some m -> Some [ I (m - 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_drop_bottom";
      imp_itps = [ Uninterp "pairinghp" ];
      imp_otps = [ Uninterp "pairinghp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Pairinghp tr ] -> Some [ Pairinghp (Pairinghp.drop_bottom tr) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_destruct";
      imp_itps = [ Uninterp "pairinghp" ];
      imp_otps = [ Int; Uninterp "pairingl" ];
      nondet = false;
      imp_exec =
        (function
        | [ Pairinghp tr ] -> (
            match tr with
            | Pairinghp.E -> None
            | Pairinghp.T (x, l) -> Some [ I x; Pairingl l ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "pairinghp_single";
      imp_itps = [ Int ];
      imp_otps = [ Uninterp "pairinghp" ];
      nondet = false;
      imp_exec =
        (function
        | [ I x ] -> Some [ Pairinghp (Pairinghp.single x) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
