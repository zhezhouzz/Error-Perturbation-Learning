type loc = Lexing.position
type tp = string list
type arg = loc * string * (string list)
type statement = loc * arg list * string * arg list
type prog = arg list * statement list * arg list
