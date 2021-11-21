open Basic_dt
open Value

let tree_max_depth = 100

let size_measure = fastexpt 2

let measure = function
  | U | NotADt -> 0
  | I _ | B _ -> 1
  | L il -> List.length il
  | T it -> TreeTailCall.deep it
  | TI iti -> LabeledTreeTailCall.deep iti
  | TB itb -> LabeledTreeTailCall.deep itb

let bound_min = 20

let bound_max = 50

let coef = 2

let mk_measure_cond input =
  let size x =
    let s = IntList.sum @@ List.map measure x in
    (* Zlog.log_write @@ spf "size:%i" s; *)
    s
  in
  let s = coef * size input in
  let bound = min bound_max @@ max bound_min s in
  let () =
    Zlog.log_write
    @@ spf "mk_measure_cond(%i, %i, %i) -> %i" bound_min s bound_max bound
  in
  fun v -> size v <= bound
