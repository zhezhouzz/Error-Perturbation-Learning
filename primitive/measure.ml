open Basic_dt
open Value

let measure = function
  | U | I _ | B _ | NotADt -> 0
  | L il -> List.length il
  | T it -> fastexpt 2 @@ Tree.deep it
  | TI iti -> fastexpt 2 @@ LabeledTree.deep iti
  | TB itb -> fastexpt 2 @@ LabeledTree.deep itb

let bound_min = 600

let bound_max = 2000

let coef = 3

let mk_measure_cond input =
  let size x = IntList.sum @@ List.map measure x in
  let bound = min bound_max @@ (max bound_min coef * size input) in
  fun v -> size v <= bound
