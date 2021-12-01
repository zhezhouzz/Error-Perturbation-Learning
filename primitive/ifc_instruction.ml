type instruction =
  | Nop
  | Push of int
  | BCall of int (* How many things to pass as arguments *)
  | BRet
  | Add
  | Load
  | Store

let layout_instruction = function
  | Nop -> "Nop"
  | Push n -> Printf.sprintf "Push %i" n
  | BCall n -> Printf.sprintf "BCall %i" n
  | BRet -> "BRet"
  | Add -> "Add"
  | Load -> "Load"
  | Store -> "Store"

type op_code = OpBCall | OpBRet | OpNop | OpPush | OpAdd | OpLoad | OpStore

let op_codes = [ OpBCall; OpBRet; OpNop; OpPush; OpAdd; OpLoad; OpStore ]

let opcode_of_instr (i : instruction) : op_code =
  match i with
  | BCall _ -> OpBCall
  | BRet -> OpBRet
  | Push _ -> OpPush
  | Nop -> OpNop
  | Add -> OpAdd
  | Load -> OpLoad
  | Store -> OpStore

let eq_instruction x y =
  match (x, y) with
  | Nop, Nop -> true
  | Push n, Push n' -> n == n'
  | BCall n, BCall n' -> n == n'
  | BRet, BRet -> true
  | Add, Add -> true
  | Load, Load -> true
  | Store, Store -> true
  | _ -> false

let i_to_int = function
  | Nop -> 0
  | Push n -> (n * 10) + 1
  | BCall n -> (n * 10) + 2
  | BRet -> 3
  | Add -> 4
  | Load -> 5
  | Store -> 6

let compare_instruction x y = compare (i_to_int x) (i_to_int y)
