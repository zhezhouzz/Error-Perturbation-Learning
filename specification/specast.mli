open Primitive;;
type t =
  | True
  | Bvar of Tp.t * string
  | Implies of t * t
  | Ite of t * t * t
  | Not of t
  | And of t list
  | Or of t list
  | Iff of t * t
  | MethodPredicate of string * (Tp.t * string) list
