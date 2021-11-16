open Basic_dt
open Printf

type t =
  | U
  | L of int list
  | T of int Tree.t
  | I of int
  | B of bool
  | TI of (int, int) LabeledTree.t
  | TB of (int, bool) LabeledTree.t
  | NotADt

let layout = function
  | U -> "tt"
  | L l -> sprintf "[%s]" (IntList.to_string l)
  | T tr -> Tree.layout string_of_int tr
  | I i -> string_of_int i
  | B b -> string_of_bool b
  | TI tr -> LabeledTree.layout string_of_int tr
  | TB tr -> LabeledTree.layout string_of_int tr
  | NotADt -> "_"

let layout_l l = sprintf "[%s]" @@ List.split_by_comma layout l

let eq x y =
  let aux = function
    | U, U -> true
    | I x, I y -> x == y
    | B x, B y -> x == y
    | L x, L y -> List.eq (fun x y -> x == y) x y
    | T x, T y -> Tree.eq (fun x y -> x == y) x y
    | TI x, TI y -> LabeledTree.eq (fun x y -> x == y) (fun x y -> x == y) x y
    | TB x, TB y -> LabeledTree.eq (fun x y -> x == y) (fun x y -> x == y) x y
    | _, _ -> false
  in
  aux (x, y)

let compare x y =
  let aux = function
    | U, U -> 0
    | I x, I y -> compare x y
    | B x, B y -> compare x y
    | L x, L y -> List.compare compare x y
    | T x, T y -> Tree.compare compare x y
    | TI x, TI y -> LabeledTree.compare compare x y
    | TB x, TB y -> LabeledTree.compare compare x y
    | NotADt, NotADt -> 0
    | _, _ -> raise @@ failwith "two values cannot be compared"
  in
  aux (x, y)

let flatten_forall = function
  | U | I _ | B _ | NotADt -> raise @@ failwith "flatten_forall: not a datatype"
  | L il -> List.flatten_forall il
  | T it -> Tree.flatten_forall it
  | TI iti -> LabeledTree.flatten_forall iti
  | TB itb -> LabeledTree.flatten_forall itb

let flatten_forall_l l =
  List.fold_left
    (fun r v ->
      match v with
      | U -> []
      | I i -> i :: r
      | B _ -> r
      | L il -> List.flatten_forall il @ r
      | T it -> Tree.flatten_forall it @ r
      | TI iti -> LabeledTree.flatten_forall iti @ r
      | TB itb -> LabeledTree.flatten_forall itb @ r
      | NotADt -> raise @@ failwith "flatten_forall_l: not a value")
    [] l

let len = function
  | U | I _ | B _ -> 1
  | NotADt -> 0
  | L il -> List.length il
  | T it -> Tree.deep it
  | TI iti -> LabeledTree.deep iti
  | TB itb -> LabeledTree.deep itb

let layout_l_len l =
  sprintf "[%s]" @@ List.split_by_comma string_of_int @@ List.map len l

let get_tp v =
  match v with
  | U -> Tp.Unit
  | I _ -> Tp.Int
  | B _ -> Tp.Bool
  | L _ -> Tp.IntList
  | T _ -> Tp.IntTree
  | TI _ -> Tp.IntTreeI
  | TB _ -> Tp.IntTreeB
  | NotADt -> raise @@ failwith "get_tp: not a value"

let flatten_forall_l_unique_paddled l =
  let lens = List.map len l in
  let l = flatten_forall_l l in
  let l = List.remove_duplicates @@ lens @ l in
  let s = List.fold_left (fun s elem -> IntSet.add elem s) IntSet.empty l in
  match IntSet.min_elt_opt s with
  | None -> [ 0; 1 ]
  | Some minmial ->
      (minmial - 2) :: (minmial - 1) :: (List.of_seq @@ IntSet.to_seq s)
