open Value;;
open Basic_dt;;

type imp = {imp_name: string; imp_itps: Tp.t list; imp_otps: Tp.t list; imp_exec: t list -> (t list) option;
            nondet: bool}

let table =
  let exn file line = failwith (spf "runtime operator(defined at file %s line %i) error" file line) in
  [
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
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "replace"; imp_itps = [IntList; Int; Int]; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [L l; I idx; I elem] -> Sugar.(let* x = List.replace_opt l idx elem in Some [L x])
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "swap"; imp_itps = [IntList; Int; Int]; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [L l; I idx; I idx'] -> Sugar.(let* x = List.swap_opt l idx idx' in Some [L x])
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "cons"; imp_itps = [Int; IntList;]; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [I elem; L l;] -> Some [L (elem :: l)]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "cons_rev"; imp_itps = [IntList;]; imp_otps = [Int; IntList]; nondet = false;
   imp_exec = function
     | [L []] -> None
     | [L (h :: t)] -> Some [I h; L t]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "nil"; imp_itps = []; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [] -> Some [L []]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "nil_rev"; imp_itps = [IntList]; imp_otps = []; nondet = false;
   imp_exec = function
     | [L []] -> Some []
     | [L (_ :: _)] -> None
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "append"; imp_itps = [IntList; Int]; imp_otps = [IntList]; nondet = false;
   imp_exec = function
     | [L l; I elem] -> Some [L (l @ [elem])]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "plus1"; imp_itps = [Int]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [I elem] -> Some [I (elem + 1)]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "minus1"; imp_itps = [Int]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [I elem] -> Some [I (elem - 1)]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "top"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L []] -> None
     | [L (h :: _)] -> Some [I h]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "bottom"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L []] -> None
     | [L l] -> Some [I (List.last l)]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "max"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L l] -> Sugar.(let* x = IntList.max_opt l in Some [I x])
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "min"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L l] -> Sugar.(let* x = IntList.min_opt l in Some [I x])
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "random_int"; imp_itps = []; imp_otps = [Int]; nondet = true;
   imp_exec = function
     | [] -> Some [Randomgen.small_gen_one1 ~tp:Tp.Int]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "is_empty"; imp_itps = [IntList]; imp_otps = [Bool]; nondet = false;
   imp_exec = function
     | [L []] -> Some [B true]
     | [L _] -> Some [B false]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tail"; imp_itps = [IntList]; imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [L []] -> None
     | [L (_ :: t)] -> Some [L t]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
] @ [
  (* tree lib *)
  {imp_name = "tree_leaf"; imp_itps = []; imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [] -> Some [T Tree.Leaf]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_node"; imp_itps = [Int; IntTree; IntTree];
   imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [I a; T b; T c] -> Some [T (Tree.Node (a, b, c))]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_leaf_rev"; imp_itps = [IntTree]; imp_otps = []; nondet = false;
   imp_exec = function
     | [T Tree.Leaf] -> Some []
     | [T _] -> None
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_node_rev"; imp_itps = [IntTree];
   imp_otps = [Int; IntTree; IntTree]; nondet = false;
   imp_exec = function
     | [T (Tree.Node (a, b, c))] -> Some [I a; T b; T c]
     | [T _] -> None
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_left_right_subtree"; imp_itps = [IntTree];
   imp_otps = [IntTree; IntTree;]; nondet = false;
   imp_exec = function
     | [T (Tree.Node (_, b, c))] -> Some [T b; T c]
     | [T Tree.Leaf] -> None
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_root"; imp_itps = [IntTree];
   imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [T (Tree.Node (a, _, _))] -> Some [I a]
     | [T Tree.Leaf] -> None
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_flip"; imp_itps = [IntTree];
   imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [T tr] -> Some [T (Tree.flip tr)]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_rec_flip"; imp_itps = [IntTree];
   imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [T tr] -> Some [T (Tree.rec_flip tr)]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_rotation_left"; imp_itps = [IntTree];
   imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [T tr] ->
       Sugar.(let* tr' = Tree.rotation_left_opt tr in
             Some [T tr'])
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_rotation_right"; imp_itps = [IntTree];
   imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [T tr] ->
       Sugar.(let* tr' = Tree.rotation_right_opt tr in
              Some [T tr'])
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_append_to_left_most"; imp_itps = [Int; IntTree];
   imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [I x; T tr] ->
       Some [T (Tree.append_to_left_most x tr)]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_append_to_right_most"; imp_itps = [Int; IntTree];
   imp_otps = [IntTree]; nondet = false;
   imp_exec = function
     | [I x; T tr] ->
       Some [T (Tree.append_to_right_most x tr)]
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_max"; imp_itps = [IntTree];
   imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [T tr] ->
       Sugar.(let* e = Tree.max_opt Stdlib.compare tr in
              Some [I e])
     | _ -> raise @@ exn __FILE__ __LINE__
  };
  {imp_name = "tree_min"; imp_itps = [IntTree];
   imp_otps = [Int]; nondet = false;
   imp_exec = function
     | [T tr] ->
       Sugar.(let* e = Tree.min_opt Stdlib.compare tr in
              Some [I e])
     | _ -> raise @@ exn __FILE__ __LINE__
  };
] @ List.init 4 (fun i ->
    {imp_name = spf "const%i" i; imp_itps = []; imp_otps = [Int]; nondet = false;
     imp_exec = function
       | [] -> Some [I i]
       | _ -> raise @@ exn __FILE__ __LINE__
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
