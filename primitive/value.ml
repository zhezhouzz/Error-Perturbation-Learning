open Basic_dt
open Printf
type t =
  | L of int list
  | T of int Tree.t
  | I of int
  | B of bool
  | TI of (int, int) LabeledTree.t
  | TB of (int, bool) LabeledTree.t
  | NotADt
let layout = function
  | L l -> sprintf "[%s]" (IntList.to_string l)
  | T tr -> Tree.layout string_of_int tr
  | I i -> string_of_int i
  | B b -> string_of_bool b
  | TI tr -> LabeledTree.layout string_of_int tr
  | TB tr -> LabeledTree.layout string_of_int tr
  | NotADt -> "_"
let eq x y =
  let aux = function
    | (I x, I y) -> x == y
    | (B x, B y) -> x == y
    | (L x, L y) -> List.eq (fun x y -> x == y) x y
    | (T x, T y) -> Tree.eq (fun x y -> x == y) x y
    | (TI x, TI y) -> LabeledTree.eq (fun x y -> x == y) (fun x y -> x == y) x y
    | (TB x, TB y) -> LabeledTree.eq (fun x y -> x == y) (fun x y -> x == y) x y
    | (_, _) -> false
  in
  aux (x, y)
let flatten_forall = function
  | I _ | B _ | NotADt -> raise @@ failwith "flatten_forall: not a datatype"
  | L il -> List.flatten_forall (fun x y -> x == y) il
  | T it -> Tree.flatten_forall (fun x y -> x == y) it
  | TI iti -> LabeledTree.flatten_forall (fun x y -> x == y) iti
  | TB itb -> LabeledTree.flatten_forall (fun x y -> x == y) itb
let flatten_forall_l l =
  List.fold_left (fun r v ->
      match v with
      | I i -> i :: r
      | B _ -> r
      | L il -> (List.flatten_forall (fun x y -> x == y) il) @ r
      | T it -> (Tree.flatten_forall (fun x y -> x == y) it) @ r
      | TI iti -> (LabeledTree.flatten_forall (fun x y -> x == y) iti) @ r
      | TB itb -> (LabeledTree.flatten_forall (fun x y -> x == y) itb) @ r
      | NotADt -> raise @@ failwith "flatten_forall_l: not a value"
    ) [] l
let get_tp v =
  match v with
  | I _ -> Tp.Int
  | B _ -> Tp.Bool
  | L _ -> Tp.IntList
  | T _ -> Tp.IntTree
  | TI _ -> Tp.IntTreeI
  | TB _ -> Tp.IntTreeB
  | NotADt -> raise @@ failwith "get_tp: not a value"
