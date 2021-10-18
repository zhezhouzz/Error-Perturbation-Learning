open Value;;
open Basic_dt;;

type imp = {imp_name: string; imp_itps: Tp.t list; imp_otps: Tp.t list; imp_exec: t list -> (t list) option;
            nondet: bool}

let table = [
  {imp_name = "insert"; imp_itps = [IntList; Int; Int]; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [L l; I idx; I elem] ->
       let rec aux l n =
         if n == 0 then elem :: l else
           match l with
           | [] -> [elem]
           | h :: t -> h :: (aux t (n - 1))
       in
       Some [L (aux l idx)]
     | _ -> raise @@ failwith "runtime operator(insert) error"
  };
  {imp_name = "replace"; imp_itps = [IntList; Int; Int]; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [L l; I idx; I elem] -> Sugar.(let* x = List.replace_opt l idx elem in Some [L x])
     | _ -> raise @@ failwith "runtime operator(replace) error"
  };
  {imp_name = "swap"; imp_itps = [IntList; Int; Int]; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [L l; I idx; I idx'] -> Sugar.(let* x = List.swap_opt l idx idx' in Some [L x])
     | _ -> raise @@ failwith "runtime operator(swap) error"
  };
  {imp_name = "cons"; imp_itps = [Int; IntList;]; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [I elem; L l;] -> Some [L (elem :: l)]
     | _ -> raise @@ failwith "runtime operator(cons) error"
  };
  {imp_name = "cons_rev"; imp_itps = [IntList;]; imp_otps = [Int; IntList]; nondet = false;
   imp_exec = function
     | [L []] -> None
     | [L (h :: t)] -> Some [I h; L t]
     | _ -> raise @@ failwith "runtime operator(cons_rev) error"
  };
  {imp_name = "nil"; imp_itps = []; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [] -> Some [L []]
     | _ -> raise @@ failwith "runtime operator(nil) error"
  };
  {imp_name = "nil_rev"; imp_itps = [IntList]; imp_otps = []; nondet = false;
   imp_exec = function
     | [L []] -> Some []
     | [L (_ :: _)] -> None
     | _ -> raise @@ failwith "runtime operator(nil_rev) error"
  };
  {imp_name = "append"; imp_itps = [IntList; Int]; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [L l; I elem] -> Some [L (l @ [elem])]
     | _ -> raise @@ failwith "runtime operator(append) error"
  };
  {imp_name = "plus1"; imp_itps = [Int]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [I elem] -> Some [I (elem + 1)]
     | _ -> raise @@ failwith "runtime operator(plus1) error"
  };
  {imp_name = "minus1"; imp_itps = [Int]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [I elem] -> Some [I (elem - 1)]
     | _ -> raise @@ failwith "runtime operator(minus1) error"
  };
  {imp_name = "top"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L []] -> None
     | [L (h :: _)] -> Some [I h]
     | _ -> raise @@ failwith "runtime operator(top) error"
  };
  {imp_name = "bottom"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L []] -> None
     | [L l] -> Some [I (List.last l)]
     | _ -> raise @@ failwith "runtime operator(bottom) error"
  };
  {imp_name = "max"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L l] -> Sugar.(let* x = IntList.max_opt l in Some [I x])
     | _ -> raise @@ failwith "runtime operator(max) error"
  };
  {imp_name = "min"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L l] -> Sugar.(let* x = IntList.min_opt l in Some [I x])
     | _ -> raise @@ failwith "runtime operator(min) error"
  };
  {imp_name = "random_int"; imp_itps = []; imp_otps = [Int]; nondet = true;
   imp_exec = function
     | [] -> Some [Randomgen.small_gen_one1 ~tp:Tp.Int]
     | _ -> raise @@ failwith "runtime operator(random_int) error"
  };
  {imp_name = "is_empty"; imp_itps = [IntList]; imp_otps = [Bool]; nondet = false;
   imp_exec = function
     | [L []] -> Some [B true]
     | [L _] -> Some [B false]
     | _ -> raise @@ failwith "runtime operator(is_empty) error"
  };
  {imp_name = "tail"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L []] -> None
     | [L (_ :: t)] -> Some [L t]
     | _ -> raise @@ failwith "runtime operator(tail) error"
  };
  (* tree lib *)
  {imp_name = "tree_leaf"; imp_itps = []; imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [] -> Some [T Tree.Leaf]
     | _ -> raise @@ failwith "runtime operator(tree_e) error"
  };
  {imp_name = "tree_node"; imp_itps = [IntTree; Int; IntTree];
   imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [I a; T b; T c] -> Some [T (Tree.Node (a, b, c))]
     | _ -> raise @@ failwith "runtime operator(tree_n) error"
  };
  {imp_name = "tree_leaf_rev"; imp_itps = [IntTree]; imp_otps = []; nondet = false;
   imp_exec = function
     | [T Tree.Leaf] -> Some []
     | [T _] -> None
     | _ -> raise @@ failwith "runtime operator(tree_e_rev) error"
  };
  {imp_name = "tree_node_rev"; imp_itps = [IntTree];
   imp_otps = [IntTree; Int; IntTree]; nondet = false;
   imp_exec = function
     | [T (Tree.Node (a, b, c))] -> Some [I a; T b; T c]
     | [T _] -> None
     | _ -> raise @@ failwith "runtime operator(tree_n_rev) error"
  };
] @ List.init 4 (fun i ->
    {imp_name = spf "const%i" i; imp_itps = []; imp_otps = [Int]; nondet = false;
     imp_exec = function
       | [] -> Some [I i]
       | _ -> raise @@ failwith "runtime operator(const_value) error"
    }
  )


let sigma_merge = function
  | [L l1; L l2] ->
    let check = List.check_sorted (fun a b -> a < b) in
    check l1 && check l2
  | _ -> raise @@ failwith "runtime client sigma error"

let phi_merge = function
  | [L l1] ->
    let check = List.check_sorted (fun a b -> a < b) in
    check l1
  | input -> raise @@ failwith (Printf.sprintf "runtime client phi error over(%s)" @@ Value.layout_l input)
