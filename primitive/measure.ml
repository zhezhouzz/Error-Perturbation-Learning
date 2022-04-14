open Basic_dt
open Value

let tree_max_depth = 600

let binomialhp_max_deep = 12

let pairinghp_max_deep = 50

let physicistsq_max_deep = 100

let realtimeq_max_deep = 100

let skewhp_max_deep = 12

let size_measure = fastexpt 2

let weight = function
  | U | B _ | I _ | NotADt -> 0
  | L _ | IBL _ | BIBL _ | Physicistsq _ | Realtimeq _ -> 1
  | T _ | TB _ -> 2
  | Binomialhp _ | Binomialt _ | Pairinghp _ | Pairingl _ | Skewhp _ | Skewt _
    ->
      3
  | TI _ -> 5

let measure_ = function
  | U | B _ | I _ | NotADt -> 0
  | L il -> List.length il
  | IBL il -> List.length il
  | BIBL il -> List.length il
  | T it -> TreeTailCall.deep it
  | TI iti -> LabeledTreeTailCall.deep iti
  | TB itb -> LabeledTreeTailCall.deep itb
  | Binomialhp x -> BinomialhpTailCall.deep x
  | Binomialt x -> BinomialhpTailCall.deep [ x ]
  | Pairinghp x -> PairinghpTailCall.deep x
  | Pairingl x -> List.length x
  | Physicistsq x -> Physicistsq.length x
  | Realtimeq x -> Realtimeq.length x
  | Skewhp x -> SkewhpTailCall.deep x
  | Skewt x -> SkewhpTailCall.deep [ x ]

let measure t = weight t * measure_ t

let bound_min = 20

let bound_max = 60

let coef = 2

let measure_size x =
  let s = IntList.sum @@ List.map measure x in
  (* Zlog.log_write @@ spf "size:%i" s; *)
  s

let mk_measure_cond input =
  let s = coef * measure_size input in
  let bound = min bound_max @@ max bound_min s in
  let () =
    Zlog.log_write
    @@ spf "mk_measure_cond(%i, %i, %i) -> %i" bound_min s bound_max bound
  in
  fun v -> measure_size v <= bound

let mk_measure_cond_v2 input n =
  let s = measure_size input in
  let ws = List.map weight input in
  let w_sum = IntList.sum ws in
  let w = max 1 (w_sum / List.length ws) in
  (* let () = *)
  (*   Zlog.log_write @@ spf "w(%i) = min(%i, %i/%i)" w 1 w_sum (List.length ws) *)
  (* in *)
  let bound = s + max 2 (w * (n - 2)) in
  let () = Zlog.log_write @@ spf "mk_measure_cond(%i ~ %i) -> %i" s n bound in
  fun v -> measure_size v < bound
