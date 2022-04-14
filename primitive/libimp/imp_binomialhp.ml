open Value
open Basic_dt
open Imp
open Imp_helper

let table =
  [
    {
      imp_name = "binomialhp_list_destruct";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialt"; Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l ] ->
            Sugar.(
              let* h, t = List.destruct_opt l in
              Some [ Binomialt h; Binomialhp t ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_list_last_destruct";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp"; Uninterp "binomialt" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l ] ->
            Sugar.(
              let* t, h = List.last_destruct_opt l in
              Some [ Binomialhp t; Binomialt h ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_list_mid_partition";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp"; Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l ] ->
            let l, r = List.mid_partition l in
            Some [ Binomialhp l; Binomialhp r ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_list_alter_partition";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp"; Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l ] ->
            let l, r = List.alter_partition l in
            Some [ Binomialhp l; Binomialhp r ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_list_cons";
      imp_itps = [ Uninterp "binomialt"; Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialt elem; Binomialhp l ] -> Some [ Binomialhp (elem :: l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_liblazy";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l ] -> Some [ Binomialhp l ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_libforce";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l ] -> Some [ Binomialhp l ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_concat";
      imp_itps = [ Uninterp "binomialhp"; Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp a; Binomialhp b ] -> Some [ Binomialhp (a @ b) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_cons_rev";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialt"; Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp [] ] -> None
        | [ Binomialhp (h :: t) ] -> Some [ Binomialt h; Binomialhp t ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_nil";
      imp_itps = [];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [] -> Some [ Binomialhp [] ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_nil_rev";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp [] ] -> Some []
        | [ Binomialhp (_ :: _) ] -> None
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_list_append";
      imp_itps = [ Uninterp "binomialhp"; Uninterp "binomialt" ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l; Binomialt elem ] -> Some [ Binomialhp (l @ [ elem ]) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_rev";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l ] -> Some [ Binomialhp (List.rev l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_top";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp [] ] -> None
        | [ Binomialhp (BinomialHeap.Node (_, x, _) :: _) ] -> Some [ I x ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_single";
      imp_itps = [ Int ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ I x ] -> Some [ Binomialhp [ BinomialHeap.Node (0, x, []) ] ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_list_head";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialt" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp [] ] -> None
        | [ Binomialhp (h :: _) ] -> Some [ Binomialt h ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_bottom";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialht" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp [] ] -> None
        | [ Binomialhp l ] -> Some [ Binomialt (List.last l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_list_last";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialht" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp [] ] -> None
        | [ Binomialhp l ] -> Some [ Binomialt (List.last l) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_is_empty";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Bool ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp [] ] -> Some [ B true ]
        | [ Binomialhp _ ] -> Some [ B false ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_tail";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp [] ] -> None
        | [ Binomialhp (_ :: t) ] -> Some [ Binomialhp t ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_list_tail";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp [] ] -> None
        | [ Binomialhp (_ :: t) ] -> Some [ Binomialhp t ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_lower_bound";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l ] -> (
            match BinomialHeap.min_opt l with
            | None -> Some [ I 0 ]
            | Some x -> Some [ I (x - 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_upper_bound";
      imp_itps = [ Uninterp "binomialhp" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l ] -> (
            match BinomialHeap.max_opt l with
            | None -> Some [ I 0 ]
            | Some x -> Some [ I (x + 1) ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_rank";
      imp_itps = [ Uninterp "binomialt" ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialt t ] -> Some [ I (BinomialHeap.rank t) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_ins_tree";
      imp_itps = [ Uninterp "binomialt"; Uninterp "binomialhp" ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialt t; Binomialhp ts ] ->
            Some [ Binomialhp (BinomialHeap.ins_tree t ts) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_link";
      imp_itps = [ Uninterp "binomialt"; Uninterp "binomialt" ];
      imp_otps = [ Uninterp "binomialt" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialt t; Binomialt t' ] ->
            Some [ Binomialt (BinomialHeap.link t t') ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialt_head";
      imp_itps = [ Uninterp "binomialt" ];
      imp_otps = [ Nat; Int ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialt t ] ->
            let r, x = BinomialHeap.t_head t in
            Some [ I r; I x ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialt_head_update";
      imp_itps = [ Uninterp "binomialt"; Int ];
      imp_otps = [ Uninterp "binomialt" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialt t; I x ] ->
            let t' = BinomialHeap.t_head_update t x in
            Some [ Binomialt t' ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "binomialhp_head_update";
      imp_itps = [ Uninterp "binomialhp"; Int ];
      imp_otps = [ Uninterp "binomialhp" ];
      nondet = false;
      imp_exec =
        (function
        | [ Binomialhp l; I x ] ->
            let t' = BinomialHeap.l_head_update l x in
            (* let _ = *)
            (*   Printf.printf "input: %s; %s\n output: %s\n" *)
            (*     (Value.layout (Binomialhp l)) *)
            (*     (Value.layout (I x)) *)
            (*     (Value.layout (Binomialhp t')) *)
            (* in *)
            Some [ Binomialhp t' ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
