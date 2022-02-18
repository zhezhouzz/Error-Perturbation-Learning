open Value
open Basic_dt
open Imp
open Imp_helper

let table =
  [
    {
      imp_name = "nat_zero";
      imp_itps = [];
      imp_otps = [ Nat ];
      nondet = false;
      imp_exec =
        (function [] -> Some [ I 0 ] | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "nat_leq";
      imp_itps = [ Nat; Nat ];
      imp_otps = [ Bool ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem; I elem' ] -> Some [ B (elem <= elem') ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "nat_plus";
      imp_itps = [ Nat; Nat ];
      imp_otps = [ Nat ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem; I elem' ] -> Some [ I (elem + elem') ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "nat_plus1";
      imp_itps = [ Nat ];
      imp_otps = [ Nat ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem ] -> Some [ I (elem + 1) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "nat_minus";
      imp_itps = [ Nat; Nat ];
      imp_otps = [ Nat ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem; I elem' ] -> Some [ I (elem - elem') ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
    {
      imp_name = "nat_minus1";
      imp_itps = [ Nat ];
      imp_otps = [ Nat ];
      nondet = false;
      imp_exec =
        (function
        | [ I elem ] -> Some [ I (elem - 1) ]
        | _ -> raise @@ exn __FILE__ __LINE__);
    };
  ]
  @ List.init 4 (fun i ->
        {
          imp_name = spf "nat_const%i" i;
          imp_itps = [];
          imp_otps = [ Nat ];
          nondet = false;
          imp_exec =
            (function
            | [] -> Some [ I i ] | _ -> raise @@ exn __FILE__ __LINE__);
        })
