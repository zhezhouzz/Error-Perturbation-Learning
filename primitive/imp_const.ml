open Value
open Basic_dt
open Imp

let exn file line =
  failwith (spf "runtime operator(defined at file %s line %i) error" file line)

let table =
  [
    {
      imp_name = "random_int";
      imp_itps = [];
      imp_otps = [ Int ];
      nondet = true;
      imp_exec =
        (function
        | [] -> Some [ Randomgen.small_gen_one1 ~tp:Tp.Int ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "random_bool";
      imp_itps = [];
      imp_otps = [ Bool ];
      nondet = true;
      imp_exec =
        (function
        | [] -> Some [ Randomgen.small_gen_one1 ~tp:Tp.Bool ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "plus";
      imp_itps = [ Int; Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem; I elem' ] -> Some [ I (elem + elem') ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "plus1";
      imp_itps = [ Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem ] -> Some [ I (elem + 1) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "minus";
      imp_itps = [ Int; Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem; I elem' ] -> Some [ I (elem - elem') ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "minus1";
      imp_itps = [ Int ];
      imp_otps = [ Int ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem ] -> Some [ I (elem - 1) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "neg";
      imp_itps = [ Bool ];
      imp_otps = [ Bool ];
      nondet = false;
      imp_exec =
        (function
        | [ B v ] -> Some [ B (not v) ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
  @ List.map
      (fun b ->
        {
          imp_name = spf "const%b" b;
          imp_itps = [];
          imp_otps = [ Bool ];
          nondet = false;
          imp_exec =
            (function
            | [] -> Some [ B b ] | _ -> raise @@ exn __FILE__ __LINE__);
        })
      [ true; false ]
  @ List.init 4 (fun i ->
        {
          imp_name = spf "const%i" i;
          imp_itps = [];
          imp_otps = [ Int ];
          nondet = false;
          imp_exec =
            (function
            | [] -> Some [ I i ] | _ -> raise @@ exn __FILE__ __LINE__);
        })

let sigma_merge = function
  | [ L l1; L l2 ] ->
      let check = List.check_sorted (fun a b -> a < b) in
      check l1 && check l2
  | _ -> raise @@ failwith "runtime client sigma error"

let phi_merge = function
  | [ L l1 ] ->
      let check = List.check_sorted (fun a b -> a < b) in
      check l1
  | input ->
      raise
      @@ failwith
           (Printf.sprintf "runtime client phi error over(%s)"
           @@ Value.layout_l input)
