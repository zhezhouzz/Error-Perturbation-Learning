open Value
open Basic_dt
open Imp
open Imp_helper

let table =
  [
    {
      imp_name = "skewhp_list_destruct";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewt"; Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l ] ->
            Sugar.(
              let* h, t = List.destruct_opt l in
              Some [ Skewt h; Skewhp t ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_list_last_destruct";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp"; Uninterp "skewt" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l ] ->
            Sugar.(
              let* t, h = List.last_destruct_opt l in
              Some [ Skewhp t; Skewt h ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_list_mid_partition";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp"; Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l ] ->
            let l, r = List.mid_partition l in
            Some [ Skewhp l; Skewhp r ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_list_alter_partition";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp"; Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l ] ->
            let l, r = List.alter_partition l in
            Some [ Skewhp l; Skewhp r ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_list_cons";
      imp_itps = [ Uninterp "skewt"; Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewt elem; Skewhp l ] -> Some [ Skewhp (elem :: l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_liblazy";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l ] -> Some [ Skewhp l ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_libforce";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l ] -> Some [ Skewhp l ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_concat";
      imp_itps = [ Uninterp "skewhp"; Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp a; Skewhp b ] -> Some [ Skewhp (a @ b) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_cons_rev";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewt"; Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp [] ] -> None
        | [ Skewhp (h :: t) ] -> Some [ Skewt h; Skewhp t ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_nil";
      imp_itps = [];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [] -> Some [ Skewhp [] ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_nil_rev";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp [] ] -> Some []
        | [ Skewhp (_ :: _) ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_list_append";
      imp_itps = [ Uninterp "skewhp"; Uninterp "skewt" ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l; Skewt elem ] -> Some [ Skewhp (l @ [ elem ]) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_rev";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l ] -> Some [ Skewhp (List.rev l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_top";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Int; IntList ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp [] ] -> None
        | [ Skewhp (Skewhp.Node (_, x, l, _) :: _) ] -> Some [ I x; L l ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_single";
      imp_itps = [ Int ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ I x ] -> Some [ Skewhp [ Skewhp.Node (0, x, [], []) ] ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_list_head";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewt" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp [] ] -> None
        | [ Skewhp (h :: _) ] -> Some [ Skewt h ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_bottom";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewht" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp [] ] -> None
        | [ Skewhp l ] -> Some [ Skewt (List.last l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_list_last";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewht" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp [] ] -> None
        | [ Skewhp l ] -> Some [ Skewt (List.last l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_is_empty";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Bool ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp [] ] -> Some [ B true ]
        | [ Skewhp _ ] -> Some [ B false ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_tail";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp [] ] -> None
        | [ Skewhp (_ :: t) ] -> Some [ Skewhp t ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_list_tail";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp [] ] -> None
        | [ Skewhp (_ :: t) ] -> Some [ Skewhp t ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_lower_bound";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l ] -> (
            match Skewhp.min_opt l with
            | None -> Some [ I 0 ]
            | Some x -> Some [ I (x - 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_upper_bound";
      imp_itps = [ Uninterp "skewhp" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewhp l ] -> (
            match Skewhp.max_opt l with
            | None -> Some [ I 0 ]
            | Some x -> Some [ I (x + 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_rank";
      imp_itps = [ Uninterp "skewt" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewt t ] -> Some [ I (Skewhp.rank t) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_ins_tree";
      imp_itps = [ Uninterp "skewt"; Uninterp "skewhp" ];
      imp_otps = [ Uninterp "skewhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewt t; Skewhp ts ] -> Some [ Skewhp (Skewhp.ins_tree t ts) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewhp_link";
      imp_itps = [ Uninterp "skewt"; Uninterp "skewt" ];
      imp_otps = [ Uninterp "skewt" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewt t; Skewt t' ] -> Some [ Skewt (Skewhp.link t t') ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewt_head";
      imp_itps = [ Uninterp "skewt" ];
      imp_otps = [ Nat; Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewt t ] ->
            let r, x = Skewhp.t_head t in
            Some [ I r; I x ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewt_head_l";
      imp_itps = [ Uninterp "skewt" ];
      imp_otps = [ Nat; Int; IntList ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewt t ] ->
            let r, x, l = Skewhp.t_head_l t in
            Some [ I r; I x; L l ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewt_head_update";
      imp_itps = [ Uninterp "skewt"; Int ];
      imp_otps = [ Uninterp "skewt" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewt t; I x ] ->
            let t' = Skewhp.t_head_update t x in
            Some [ Skewt t' ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "skewt_head_l_update";
      imp_itps = [ Uninterp "skewt"; Int ];
      imp_otps = [ Uninterp "skewt" ];
      nondet = false;
      imp_exec =
        (function
        | [ Skewt t; I x ] ->
            let t' = Skewhp.t_head_update t x in
            Some [ Skewt t' ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
