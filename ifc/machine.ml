open Instructions
open Rules
open Basic_dt

let zreplicate (n : int) a =
  if n >= 0 then Some (List.init n (fun _ -> a)) else None

(** * Machine definition *)

type atom = int * label

let pc_lab (pc : atom) : label =
  let _, l = pc in
  l

type stack_elem = Satom of atom | Sret of atom

type stack = stack_elem list

let app_stack (l : atom list) (s : stack) : stack =
  List.map (fun a -> Satom a) l @ s

type i_mem = instruction list

type mem = atom list

type state = {
  st_imem : i_mem;
  (* instruction memory *)
  st_mem : mem;
  (* memory *)
  st_stack : stack;
  (* operand stack *)
  st_pc : atom; (* program counter *)
}

(* How many lables in a op *)
let labelCount (c : op_code) : int =
  match c with
  | OpBCall -> 1
  | OpBRet -> 2
  | OpNop -> 0
  | OpPush -> 0
  | OpAdd -> 2
  | OpLoad -> 2
  | OpStore -> 3

type table = op_code -> allow_modify

let v_pc = L_Var 0

let v_1 = L_Var 1

let v_2 = L_Var 2

let v_3 = L_Var 3

let default_table op =
  let mk (a, b, c) = { allow = a; labRes = b; labResPC = c } in
  match op with
  | OpBCall -> mk (A_True, Some v_pc, L_Join (v_1, v_pc))
  | OpBRet -> mk (A_True, Some (L_Join (v_2, v_pc)), v_1)
  | OpNop -> mk (A_True, None, v_pc)
  | OpPush -> mk (A_True, Some L_Bot, v_pc)
  | OpAdd -> mk (A_True, Some (L_Join (v_1, v_2)), v_pc)
  | OpLoad -> mk (A_True, Some (L_Join (v_1, v_2)), v_pc)
  | OpStore ->
      mk
        ( A_LE (L_Join (v_1, v_pc), v_3),
          Some (L_Join (v_pc, L_Join (v_1, v_2))),
          v_pc )

let run_tmr (t : table) (op : op_code) (labs : label list) (pc : label) :
    (label option * label) option =
  let r = t op in
  if List.length labs != labelCount op then raise @@ failwith "run_tmr"
  else apply_rule r labs pc

let rec insertRet (s : stack) (n : int) (a : atom) : stack option =
  match (n, s) with
  | 0, _ -> Some (Sret a :: s)
  | n, x :: xs ->
      Sugar.(
        let* s' = insertRet xs (n - 1) a in
        Some (x :: s'))
  | _, _ -> None

let rec findRet (s : stack) : (atom * stack) option =
  match s with
  | Sret x :: s' -> Some (x, s')
  | _ :: s' -> findRet s'
  | [] -> None

let insert (s : stack) (n : int) (a : atom) : stack option =
  if n < 0 then None else insertRet s n a

let lookupInstr (st : state) : instruction option =
  List.nth_opt st.st_imem @@ fst st.st_pc

let exec (t : table) (st : state) : state option =
  let open Sugar in
  let* instr = lookupInstr st in
  match (instr, st) with
  | ( BCall n,
      { st_imem; st_mem; st_stack = Satom (x, l) :: sigma; st_pc = xpc, lpc } )
    -> (
      match run_tmr t OpBCall [ l ] lpc with
      | Some (Some rl, rpcl) ->
          let pc' = (x, rpcl) in
          let ret_pc = (xpc + 1, rl) in
          let* sigma' = insert sigma n ret_pc in
          Some { st_imem; st_mem; st_stack = sigma'; st_pc = pc' }
      | _ -> None)
  | ( BRet,
      { st_imem; st_mem; st_stack = Satom (ax, al) :: sigma; st_pc = _, lpc } )
    -> (
      let* tmp = findRet sigma in
      let (xrpc, lrpc), sigma' = tmp in
      match run_tmr t OpBRet [ lrpc; al ] lpc with
      | Some (Some rl, rpcl) ->
          let pc' = (xrpc, rpcl) in
          Some
            {
              st_imem;
              st_mem;
              st_stack = Satom (ax, rl) :: sigma';
              st_pc = pc';
            }
      | _ -> None)
  | ( Load,
      { st_imem; st_mem; st_stack = Satom (x, l) :: sigma; st_pc = xpc, lpc } )
    -> (
      let* a = List.nth_opt st_mem x in
      let ax, al = a in
      match run_tmr t OpLoad [ al; l ] lpc with
      | Some (Some rl, rpcl) ->
          Some
            {
              st_imem;
              st_mem;
              st_stack = Satom (ax, rl) :: sigma;
              st_pc = (xpc + 1, rpcl);
            }
      | _ -> None)
  | ( Store,
      {
        st_imem;
        st_mem;
        st_stack = Satom (x, lx) :: Satom (a, la) :: sigma;
        st_pc = xpc, lpc;
      } ) -> (
      let* inMem = List.nth_opt st_mem x in
      match run_tmr t OpStore [ lx; la; pc_lab inMem ] lpc with
      | Some (Some rl, rpcl) ->
          let* m' = List.update_opt st_mem x (a, rl) in
          Some
            { st_imem; st_mem = m'; st_stack = sigma; st_pc = (xpc + 1, rpcl) }
      | _ -> None)
  | Push r, { st_imem; st_mem; st_stack = sigma; st_pc = xpc, lpc } -> (
      match run_tmr t OpPush [] lpc with
      | Some (Some rl, rpcl) ->
          Some
            {
              st_imem;
              st_mem;
              st_stack = Satom (r, rl) :: sigma;
              st_pc = (xpc + 1, rpcl);
            }
      | _ -> None)
  | Nop, { st_imem; st_mem; st_stack = sigma; st_pc = xpc, lpc } -> (
      match run_tmr t OpNop [] lpc with
      | Some (_, rpcl) ->
          Some { st_imem; st_mem; st_stack = sigma; st_pc = (xpc + 1, rpcl) }
      | _ -> None)
  | ( Add,
      {
        st_imem;
        st_mem;
        st_stack = Satom (x, lx) :: Satom (y, ly) :: sigma;
        st_pc = xpc, lpc;
      } ) -> (
      match run_tmr t OpAdd [ lx; ly ] lpc with
      | Some (Some rl, rpcl) ->
          Some
            {
              st_imem;
              st_mem;
              st_stack = Satom (x + y, rl) :: sigma;
              st_pc = (xpc + 1, rpcl);
            }
      | _ -> None)
  | _, _ -> None

let rec execN (t : table) (n : int) (s : state) : state list =
  match n with
  | 0 -> [ s ]
  | _ -> (
      match exec t s with Some s' -> s :: execN t (n - 1) s' | None -> [ s ])
