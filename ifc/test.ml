open Rules
open Machine
open Primitive
open Ifc_instruction

let state1 =
  {
    st_imem = [ Store ];
    st_mem = [ (1, L); (1, H); (1, H) ];
    st_stack = [ Satom (0, H); Satom (1, L); Satom (2, L) ];
    st_pc = (0, L);
  }

let state2 =
  {
    st_imem = [ Store ];
    st_mem = [ (1, L); (0, H); (0, H) ];
    st_stack = [ Satom (0, H); Satom (1, L); Satom (2, L) ];
    st_pc = (0, L);
  }

let correct = List.nth Errors.error_tables 0

let error1 = List.nth Errors.error_tables 1

(* implementation encoding *)

let to_label x = if x then L else H

let from_label = function L -> true | H -> false

let to_atom (n, x) = (n, to_label x)

let from_atom (n, x) = (n, from_label x)

let to_stack_elem (y, n, x) =
  if y then Satom (to_atom (n, x)) else Sret (to_atom (n, x))

let from_stack_elem = function
  | Satom a ->
      let n, x = from_atom a in
      (true, n, x)
  | Sret a ->
      let n, x = from_atom a in
      (false, n, x)

let to_state (st_imem1 : instruction list) (st_mem1 : (int * bool) list)
    (st_stack1 : (bool * int * bool) list) (st_pc_idx : int)
    (st_pc_label : bool) =
  {
    st_imem = st_imem1;
    st_mem = List.map to_atom st_mem1;
    st_stack = List.map to_stack_elem st_stack1;
    st_pc = (st_pc_idx, to_label st_pc_label);
  }

let from_state st =
  ( st.st_imem,
    List.map from_atom st.st_mem,
    List.map from_stack_elem st.st_stack,
    fst st.st_pc,
    from_label @@ snd st.st_pc )

let client_raw table (st_imem1 : instruction list) (st_mem1 : (int * bool) list)
    (st_stack1 : (bool * int * bool) list) (st_pc_idx1 : int)
    (st_pc_label1 : bool) (st_imem2 : instruction list)
    (st_mem2 : (int * bool) list) (st_stack2 : (bool * int * bool) list)
    (st_pc_idx2 : int) (st_pc_label2 : bool) =
  let state1 = to_state st_imem1 st_mem1 st_stack1 st_pc_idx1 st_pc_label1 in
  let state2 = to_state st_imem2 st_mem2 st_stack2 st_pc_idx2 st_pc_label2 in
  match Driver.client table state1 state2 with
  | None -> None
  | Some (st1', st2') ->
      let a1, b1, c1, d1, e1 = from_state st1' in
      let a2, b2, c2, d2, e2 = from_state st2' in
      Some (a1, b1, c1, d1, e1, a2, b2, c2, d2, e2)

let sigma_raw a1 b1 c1 d1 e1 a2 b2 c2 d2 e2 =
  let st1 = to_state a1 b1 c1 d1 e1 in
  let st2 = to_state a2 b2 c2 d2 e2 in
  Driver.sigma st1 st2

let phi_raw a1 b1 c1 d1 e1 a2 b2 c2 d2 e2 a1' b1' c1' d1' e1' a2' b2' c2' d2'
    e2' =
  let st1 = to_state a1 b1 c1 d1 e1 in
  let st2 = to_state a2 b2 c2 d2 e2 in
  let st1' = to_state a1' b1' c1' d1' e1' in
  let st2' = to_state a2' b2' c2' d2' e2' in
  Driver.phi st1 st2 st1' st2'

let test () =
  let () = Printf.printf "state1:\n%s\n" @@ layout_state state1 in
  let () = Printf.printf "state2:\n%s\n" @@ layout_state state2 in
  let a1, b1, c1, d1, e1 = from_state state1 in
  let a2, b2, c2, d2, e2 = from_state state2 in
  let () =
    Printf.printf "in sigma: %b\n" @@ sigma_raw a1 b1 c1 d1 e1 a2 b2 c2 d2 e2
  in
  match client_raw correct a1 b1 c1 d1 e1 a2 b2 c2 d2 e2 with
  | None -> Printf.printf "get stuck\n"
  | Some (a1', b1', c1', d1', e1', a2', b2', c2', d2', e2') ->
      let () =
        Printf.printf "in phi: %b\n"
        @@ phi_raw a1 b1 c1 d1 e1 a2 b2 c2 d2 e2 a1' b1' c1' d1' e1' a2' b2' c2'
             d2' e2'
      in
      ()
