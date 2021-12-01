open Value
open Basic_dt
open Imp

let exn file line =
  failwith (spf "runtime operator(defined at file %s line %i) error" file line)

let ib_table =
  [
    {
      imp_name = "neg";
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
