type instruction =
  | Nop
  | Push of int
  | BCall of int (* How many things to pass as arguments *)
  | BRet
  | Add
  | Load
  | Store

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
