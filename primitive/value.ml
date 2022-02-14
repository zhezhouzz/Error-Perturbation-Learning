open Basic_dt
open Printf
open Ifc_instruction

type t =
  | U
  | L of int list
  | T of int Tree.t
  | I of int
  | B of bool
  | TI of (int, int) LabeledTree.t
  | TB of (int, bool) LabeledTree.t
  | IInstr of instruction
  | IInstrL of instruction list
  | IBL of (int * bool) list
  | BIBL of (bool * int * bool) list
  | Binomialhp of BinomialHeap.t
  | Pairinghp of Pairinghp.t
  | Physicistsq of Physicistsq.t
  | Realtimeq of Realtimeq.t
  | Skewhp of Skewhp.t
  | NotADt

let layout = function
  | U -> "()"
  | L l -> sprintf "[%s]" (IntList.to_string l)
  | T tr -> Tree.layout string_of_int tr
  | I i -> string_of_int i
  | B b -> string_of_bool b
  | TI tr -> LabeledTree.layout string_of_int tr
  | TB tr -> LabeledTree.layout string_of_int tr
  | IInstr instr -> layout_instruction instr
  | IInstrL instrl ->
      sprintf "[%s]" @@ List.split_by ";" layout_instruction instrl
  | IBL ibl ->
      sprintf "[%s]"
      @@ List.split_by ";" (fun (i, b) -> sprintf "%i,%b" i b) ibl
  | BIBL bibl ->
      sprintf "[%s]"
      @@ List.split_by ";" (fun (b1, i, b2) -> sprintf "%b,%i,%b" b1 i b2) bibl
  | Binomialhp x -> BinomialHeap.to_string x
  | Pairinghp x -> Pairinghp.to_string x
  | Physicistsq x -> Physicistsq.to_string x
  | Realtimeq x -> Realtimeq.to_string x
  | Skewhp x -> Skewhp.to_string x
  | NotADt -> "_"

let layout_l l = sprintf "[%s]" @@ List.split_by_comma layout l

let formal_layout = function
  | U -> "tt"
  | L l -> sprintf "[%s]" (IntList.to_string l)
  | T tr -> Tree.formal_layout string_of_int tr
  | I i -> string_of_int i
  | B b -> string_of_bool b
  | TI tr -> LabeledTree.formal_layout string_of_int string_of_int tr
  | TB tr -> LabeledTree.formal_layout string_of_bool string_of_int tr
  | IInstr instr -> layout_instruction instr
  | IInstrL instrl ->
      sprintf "[%s]" @@ List.split_by ";" layout_instruction instrl
  | IBL ibl ->
      sprintf "[%s]"
      @@ List.split_by ";" (fun (i, b) -> sprintf "%i,%b" i b) ibl
  | BIBL bibl ->
      sprintf "[%s]"
      @@ List.split_by ";" (fun (b1, i, b2) -> sprintf "%b,%i,%b" b1 i b2) bibl
  | Binomialhp x -> BinomialHeap.to_string x
  | Pairinghp x -> Pairinghp.to_string x
  | Physicistsq x -> Physicistsq.to_string x
  | Realtimeq x -> Realtimeq.to_string x
  | Skewhp x -> Skewhp.to_string x
  | NotADt -> "_"

let formal_layout_l l = sprintf "(%s)" @@ List.split_by_comma formal_layout l

let eq x y =
  let aux = function
    | U, U -> true
    | I x, I y -> x == y
    | B x, B y -> x == y
    | L x, L y -> List.eq (fun x y -> x == y) x y
    | T x, T y -> Tree.eq (fun x y -> x == y) x y
    | TI x, TI y -> LabeledTree.eq (fun x y -> x == y) (fun x y -> x == y) x y
    | TB x, TB y -> LabeledTree.eq (fun x y -> x == y) (fun x y -> x == y) x y
    | IInstr x, IInstr y -> eq_instruction x y
    | IInstrL x, IInstrL y -> List.eq eq_instruction x y
    | IBL ibl1, IBL ibl2 ->
        List.eq (fun (i1, b1) (i2, b2) -> i1 == i2 && b1 == b2) ibl1 ibl2
    | BIBL bibl1, BIBL bibl2 ->
        List.eq
          (fun (b1, i1, b1') (b2, i2, b2') ->
            i1 == i2 && b1 == b2 && b1' == b2')
          bibl1 bibl2
    | Binomialhp x, Binomialhp y -> BinomialHeap.eq x y
    | Pairinghp x, Pairinghp y -> Pairinghp.eq x y
    | Physicistsq x, Physicistsq y -> Physicistsq.eq x y
    | Realtimeq x, Realtimeq y -> Realtimeq.eq x y
    | Skewhp x, Skewhp y -> Skewhp.eq x y
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
    | IInstr x, IInstr y -> compare_instruction x y
    | IInstrL x, IInstrL y -> List.compare compare_instruction x y
    | IBL ibl1, IBL ibl2 ->
        List.compare
          (fun (i1, b1) (i2, b2) ->
            let x = compare i1 i2 in
            if x == 0 then compare b1 b2 else x)
          ibl1 ibl2
    | BIBL bibl1, BIBL bibl2 ->
        List.compare
          (fun (b1, i1, b1') (b2, i2, b2') ->
            let x = compare b1 b2 in
            let y = compare i1 i2 in
            if x == 0 then if y == 0 then compare b1' b2' else y else x)
          bibl1 bibl2
    | NotADt, NotADt -> 0
    | Binomialhp x, Binomialhp y -> BinomialHeap.compare x y
    | Pairinghp x, Pairinghp y -> Pairinghp.compare x y
    | Physicistsq x, Physicistsq y -> Physicistsq.compare x y
    | Realtimeq x, Realtimeq y -> Realtimeq.compare x y
    | Skewhp x, Skewhp y -> Skewhp.compare x y
    | _, _ ->
        raise
        @@ failwith
             (spf "two values(%s, %s) cannot be compared" (layout x) (layout y))
  in
  aux (x, y)

(* TODO: shuold I flatten instructions? *)
let flatten_forall = function
  | U | I _ | B _ | IInstr _ | IInstrL _ | NotADt ->
      raise @@ failwith "flatten_forall: not a datatype"
  | L il -> List.flatten_forall il
  | IBL ibl -> List.flatten_forall @@ List.map fst ibl
  | BIBL bibl -> List.flatten_forall @@ List.map (fun (_, x, _) -> x) bibl
  | T it -> Tree.flatten_forall it
  | TI iti -> LabeledTree.flatten_forall iti
  | TB itb -> LabeledTree.flatten_forall itb
  | Binomialhp x -> List.remove_duplicates @@ BinomialHeap.flatten x
  | Pairinghp x -> List.remove_duplicates @@ Pairinghp.flatten x
  | Physicistsq x -> List.remove_duplicates @@ Physicistsq.flatten x
  | Realtimeq x -> List.remove_duplicates @@ Realtimeq.flatten x
  | Skewhp x -> List.remove_duplicates @@ Skewhp.flatten x

let flatten_forall_l l =
  List.fold_left
    (fun r v ->
      match v with
      | U | IInstr _ | IInstrL _ -> []
      | I i -> i :: r
      | B _ -> r
      | L il -> List.flatten_forall il @ r
      | IBL ibl -> (List.flatten_forall @@ List.map fst ibl) @ r
      | BIBL bibl ->
          (List.flatten_forall @@ List.map (fun (_, x, _) -> x) bibl) @ r
      | T it -> Tree.flatten_forall it @ r
      | TI iti -> LabeledTree.flatten_forall iti @ r
      | TB itb -> LabeledTree.flatten_forall itb @ r
      | NotADt -> raise @@ failwith "flatten_forall_l: not a value"
      | Binomialhp x -> r @ List.remove_duplicates @@ BinomialHeap.flatten x
      | Pairinghp x -> r @ List.remove_duplicates @@ Pairinghp.flatten x
      | Physicistsq x -> r @ List.remove_duplicates @@ Physicistsq.flatten x
      | Realtimeq x -> r @ List.remove_duplicates @@ Realtimeq.flatten x
      | Skewhp x -> r @ List.remove_duplicates @@ Skewhp.flatten x)
    [] l

let len = function
  | U | I _ | B _ | IInstr _ -> 1
  | NotADt -> 0
  | L il -> List.length il
  | IBL ibl -> List.length ibl
  | BIBL bibl -> List.length bibl
  | T it -> TreeTailCall.deep it
  | TI iti -> LabeledTreeTailCall.deep iti
  | TB itb -> LabeledTreeTailCall.deep itb
  | IInstrL l -> List.length l
  | Binomialhp x -> BinomialHeap.deep x
  | Pairinghp x -> Pairinghp.deep x
  | Physicistsq x -> Physicistsq.length x
  | Realtimeq x -> Realtimeq.length x
  | Skewhp x -> Skewhp.deep x

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
  | IInstr _ -> IfcInstr
  | IInstrL _ -> IfcInstrList
  | IBL _ -> IntBoolList
  | BIBL _ -> BoolIntBoolList
  | NotADt -> raise @@ failwith "get_tp: not a value"
  | Binomialhp _ -> Tp.Uninterp "binomialhp"
  | Pairinghp _ -> Tp.Uninterp "pairinghp"
  | Physicistsq _ -> Tp.Uninterp "physicistsq"
  | Realtimeq _ -> Tp.Uninterp "realtimeq"
  | Skewhp _ -> Tp.Uninterp "skewhp"

let get_tp_l = List.map get_tp

let flatten_forall_l_unique_paddled l =
  let lens = List.map len l in
  let l = flatten_forall_l l in
  let l = List.remove_duplicates @@ lens @ l in
  let s = List.fold_left (fun s elem -> IntSet.add elem s) IntSet.empty l in
  match IntSet.min_elt_opt s with
  | None -> [ 0; 1 ]
  | Some minmial ->
      (minmial - 2) :: (minmial - 1) :: (List.of_seq @@ IntSet.to_seq s)
