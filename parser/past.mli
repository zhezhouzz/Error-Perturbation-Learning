type loc = Lexing.position

type tp = string list * string list

type arg = loc * string * tp

type statement = loc * arg list * string * arg list

type prog = arg list * statement list * arg list
