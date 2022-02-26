open Value
open Basic_dt
open Imp
open Imp_helper

let ib_table =
  [
    {
      imp_name = "iblist_destruct";
      imp_itps = [ IntBoolList ];
      imp_otps = [ Int; Bool; IntBoolList ];
      nondet = false;
      imp_exec =
        (function
        | [ IBL l ] ->
            Sugar.(
              let* (i, b), t = List.destruct_opt l in
              Some [ I i; B b; IBL t ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "iblist_cons";
      imp_itps = [ Int; Bool; IntBoolList ];
      imp_otps = [ IntBoolList ];
      nondet = false;
      imp_exec =
        (function
        | [ I i; B b; IBL t ] -> Some [ IBL ((i, b) :: t) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]

let bib_table =
  [
    {
      imp_name = "biblist_destruct";
      imp_itps = [ BoolIntBoolList ];
      imp_otps = [ Bool; Int; Bool; BoolIntBoolList ];
      nondet = false;
      imp_exec =
        (function
        | [ BIBL l ] ->
            Sugar.(
              let* (b1, i, b2), t = List.destruct_opt l in
              Some [ B b1; I i; B b2; BIBL t ])
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "biblist_cons";
      imp_itps = [ Bool; Int; Bool; BoolIntBoolList ];
      imp_otps = [ BoolIntBoolList ];
      nondet = false;
      imp_exec =
        (function
        | [ B b1; I i; B b2; BIBL t ] -> Some [ BIBL ((b1, i, b2) :: t) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]

let table =
  ib_table @ bib_table
  @ [
      {
        imp_name = "list_destruct";
        imp_itps = [ IntList ];
        imp_otps = [ Int; IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] ->
              Sugar.(
                let* h, t = List.destruct_opt l in
                Some [ I h; L t ])
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_last_destruct";
        imp_itps = [ IntList ];
        imp_otps = [ IntList; Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] ->
              Sugar.(
                let* t, h = List.last_destruct_opt l in
                Some [ L t; I h ])
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_mid_partition";
        imp_itps = [ IntList ];
        imp_otps = [ IntList; IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] ->
              let l, r = List.mid_partition l in
              Some [ L l; L r ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_alter_partition";
        imp_itps = [ IntList ];
        imp_otps = [ IntList; IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] ->
              let l, r = List.alter_partition l in
              Some [ L l; L r ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "insert";
        imp_itps = [ IntList; Int; Int ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l; I idx; I elem ] ->
              let rec aux l n =
                if n == 0 then elem :: l
                else match l with [] -> [ elem ] | h :: t -> h :: aux t (n - 1)
              in
              Some [ L (aux l idx) ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "replace";
        imp_itps = [ IntList; Int; Int ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l; I idx; I elem ] ->
              Sugar.(
                let* x = List.replace_opt l idx elem in
                Some [ L x ])
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "swap";
        imp_itps = [ IntList; Int; Int ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l; I idx; I idx' ] ->
              Sugar.(
                let* x = List.swap_opt l idx idx' in
                Some [ L x ])
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "cons";
        imp_itps = [ Int; IntList ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ I elem; L l ] -> Some [ L (elem :: l) ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "liblazy";
        imp_itps = [ IntList ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] -> Some [ L l ] | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "libforce";
        imp_itps = [ IntList ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] -> Some [ L l ] | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "concat";
        imp_itps = [ IntList; IntList ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L a; L b ] -> Some [ L (a @ b) ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "cons_rev";
        imp_itps = [ IntList ];
        imp_otps = [ Int; IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L [] ] -> None
          | [ L (h :: t) ] -> Some [ I h; L t ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "nil";
        imp_itps = [];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function [] -> Some [ L [] ] | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "nil_rev";
        imp_itps = [ IntList ];
        imp_otps = [];
        nondet = false;
        imp_exec =
          (function
          | [ L [] ] -> Some []
          | [ L (_ :: _) ] -> None
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "append";
        imp_itps = [ IntList; Int ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l; I elem ] -> Some [ L (l @ [ elem ]) ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "rev";
        imp_itps = [ IntList ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] -> Some [ L (List.rev l) ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "top";
        imp_itps = [ IntList ];
        imp_otps = [ Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L [] ] -> None
          | [ L (h :: _) ] -> Some [ I h ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_single";
        imp_itps = [ Int ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ I x ] -> Some [ L [ x ] ] | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_head";
        imp_itps = [ IntList ];
        imp_otps = [ Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L [] ] -> None
          | [ L (h :: _) ] -> Some [ I h ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "bottom";
        imp_itps = [ IntList ];
        imp_otps = [ Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L [] ] -> None
          | [ L l ] -> Some [ I (List.last l) ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_last";
        imp_itps = [ IntList ];
        imp_otps = [ Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L [] ] -> None
          | [ L l ] -> Some [ I (List.last l) ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_len";
        imp_itps = [ IntList ];
        imp_otps = [ Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] -> Some [ I (List.length l) ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "max";
        imp_itps = [ IntList ];
        imp_otps = [ Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] ->
              Sugar.(
                let* x = IntList.max_opt l in
                Some [ I x ])
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "min";
        imp_itps = [ IntList ];
        imp_otps = [ Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] ->
              Sugar.(
                let* x = IntList.min_opt l in
                Some [ I x ])
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_upper_bound";
        imp_itps = [ IntList ];
        imp_otps = [ Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] ->
              Sugar.(
                let* x = IntList.max_opt l in
                Some [ I (x + 1) ])
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_lower_bound";
        imp_itps = [ IntList ];
        imp_otps = [ Int ];
        nondet = false;
        imp_exec =
          (function
          | [ L l ] ->
              Sugar.(
                let* x = IntList.min_opt l in
                Some [ I (x - 1) ])
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "is_empty";
        imp_itps = [ IntList ];
        imp_otps = [ Bool ];
        nondet = false;
        imp_exec =
          (function
          | [ L [] ] -> Some [ B true ]
          | [ L _ ] -> Some [ B false ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "tail";
        imp_itps = [ IntList ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L [] ] -> Some [ L [] ]
          | [ L (_ :: t) ] -> Some [ L t ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
      {
        imp_name = "list_tail";
        imp_itps = [ IntList ];
        imp_otps = [ IntList ];
        nondet = false;
        imp_exec =
          (function
          | [ L [] ] -> None
          | [ L (_ :: t) ] -> Some [ L t ]
          | _ -> raise @@ exn __FILE__ __LINE__);
      };
    ]
