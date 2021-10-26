module V = Primitive.Value;;
module T = Primitive.Tp;;
type t =
  | Var of string
  | Tuple of t list
  | Lit of V.t list
  | Op of string * (t list)
  | App of string * (t list)
  | Ift of t * t * t
  | Let of (T.t * string) list * t * t
  | Match of string list * (t * t) list
type func = {fname: string; args: T.tvar list; body: t; res: T.t list}
