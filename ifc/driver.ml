open Machine
open Rules
open Indist

let is_atom_low (a : atom) = match a with _, L -> true | _ -> false

let ssni (t : table) st1 st2 =
  let l1 = snd st1.st_pc in
  let l2 = snd st2.st_pc in
  match lookupInstr st1 with
  | Some _ ->
      if indist_state st1 st2 then
        match (l1, l2) with
        | L, L -> (
            match (exec t st1, exec t st2) with
            | Some st1', Some st2' -> indist_state st1' st2'
            | _, _ -> true (* collect "L,L,FAIL" true *))
        | H, H -> (
            match (exec t st1, exec t st2) with
            | Some st1', Some st2' ->
                if is_atom_low st1'.st_pc && is_atom_low st2'.st_pc then
                  (* whenFail ("Initial states: " ++ nl ++ show_pair st1 st2 ++ nl
                            ++ "Final states: " ++ nl ++ show_pair st1' st2' ++nl) *)
                  (* collect ("H -> L")*) indist_state st1' st2'
                else if is_atom_low st1'.st_pc then
                  (* whenFail ("states: " ++ nl ++ show_pair st2 st2' ++ nl )*)
                  (* collect ("H -> H")*) indist_state st2 st2'
                else
                  (*            whenFail ("states: " ++ nl ++ show_pair st1 st1' ++ nl )*)
                  (* collect ("H -> H")*) indist_state st1 st1'
            | _, _ -> true)
        | H, _ -> (
            match exec t st1 with
            | Some st1' ->
                (*             whenFail ("states: " ++ nl ++ show_pair st1 st1' ++ nl )*)
                (* collect "H -> H"*)
                indist_state st1 st1'
            | _ -> (*collect "H,_,FAIL" true *) true)
        | _, H -> (
            match exec t st2 with
            | Some st2' ->
                (*             whenFail ("states: " ++ nl ++ show_pair st2 st2' ++ nl )*)
                (* collect "H -> H"*)
                indist_state st2 st2'
            | _ -> (*collect "L,H,FAIL" true *) true)
      else (* collect "Not indist!" true*) true
  | _ -> true
