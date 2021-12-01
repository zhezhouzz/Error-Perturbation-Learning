open Machine
open Rules

(* Indistinguishability type class *)

let indist_atom a1 a2 =
  let x1, l1 = a1 in
  let x2, l2 = a2 in
  match (l1, l2) with L, L -> x1 == x2 | H, H -> true | _, _ -> false

let indist_mem m1 m2 =
  if List.length m1 != List.length m2 then false
  else List.for_all2 indist_atom m1 m2

let rec cropTop (s : stack) : stack =
  match s with
  | [] -> []
  | Satom _ :: s' -> cropTop s'
  | Sret (_, H) :: s' -> cropTop s'
  | Sret (_, L) :: _ -> s

(* Assumes stacks have been cropTopped! *)
let indist_stack s1 s2 =
  let rec aux s1 s2 =
    match (s1, s2) with
    | Satom a1 :: s1, Satom a2 :: s2 -> indist_atom a1 a2 && aux s1 s2
    | Sret a1 :: s1, Sret a2 :: s2 -> indist_atom a1 a2 && aux s1 s2
    | [], [] -> true
    | _, _ -> false
  in
  aux s1 s2

let indist_state st1 st2 =
  let { st_mem = mem1; st_stack = stk1; st_pc = pc1; _ } = st1 in
  let { st_mem = mem2; st_stack = stk2; st_pc = pc2; _ } = st2 in
  if not (indist_mem mem1 mem2) then (* trace "Memory" *) false
  else if not (indist_atom pc1 pc2) then (* trace "PC" *) false
  else
    let stk1, stk2 =
      match pc1 with _, H -> (cropTop stk1, cropTop stk2) | _ -> (stk1, stk2)
    in
    if not (indist_stack stk1 stk2) then (* trace "stack" *) false else true
