open Basic_dt
module V = Value
module T = Tp

let bool_gen = QCheck.Gen.oneofl [ true; false ]

let int_gen (chooses : int list) = QCheck.Gen.oneofl chooses

(* let instruction_gen (chooses : int list) = *)
(*   let open Ifc_instruction in *)
(*   QCheck.Gen.( *)
(*     map2 *)
(*       (fun x y -> *)
(*         match x with *)
(*         | 0 -> Nop *)
(*         | 1 -> Push y *)
(*         | 2 -> BCall y *)
(*         | 3 -> BRet *)
(*         | 4 -> Add *)
(*         | 5 -> Load *)
(*         | _ -> Store) *)
(*       (int_bound 6) (oneofl chooses)) *)

(* let instruction_list_gen (chooses : int list) (bound : int) = *)
(*   QCheck.Gen.(list_size (int_bound bound) (instruction_gen chooses)) *)

let list_gen (chooses : int list) (bound : int) =
  QCheck.Gen.(list_size (int_bound bound) (oneofl chooses))

let iblist_gen (chooses : int list) (bound : int) =
  QCheck.Gen.(list_size (int_bound bound) (pair (oneofl chooses) bool_gen))

let biblist_gen (chooses : int list) (bound : int) =
  QCheck.Gen.(
    list_size (int_bound bound) (triple bool_gen (oneofl chooses) bool_gen))

let tree_gen (chooses : int list) (bound : int) =
  let node a l r = Tree.Node (a, l, r) in
  QCheck.Gen.(
    sized_size (int_bound bound)
    @@ fix (fun self n ->
           match n with
           | 0 -> oneofl [ Tree.Leaf ]
           | n ->
               frequency
                 [
                   (1, oneofl [ Tree.Leaf ]);
                   ( 3,
                     QCheck.Gen.map3 node (oneofl chooses)
                       (self (n - 1))
                       (self (n - 1)) );
                 ]))

let treei_gen (chooses : int list) (bound : int) =
  let node i a l r = LabeledTree.Node (i, a, l, r) in
  QCheck.Gen.(
    let map4 f x y z k st = f (x st) (y st) (z st) (k st) in
    sized_size (int_bound bound)
    @@ fix (fun self n ->
           match n with
           | 0 -> oneofl [ LabeledTree.Leaf ]
           | n ->
               frequency
                 [
                   (1, oneofl [ LabeledTree.Leaf ]);
                   ( 3,
                     map4 node (oneofl chooses) (oneofl chooses)
                       (self (n - 1))
                       (self (n - 1)) );
                 ]))

let treeb_gen (chooses : int list) (bound : int) =
  let node b a l r = LabeledTree.Node (b, a, l, r) in
  QCheck.Gen.(
    let map4 f x y z k st = f (x st) (y st) (z st) (k st) in
    sized_size (int_bound bound)
    @@ fix (fun self n ->
           match n with
           | 0 -> oneofl [ LabeledTree.Leaf ]
           | n ->
               frequency
                 [
                   (1, oneofl [ LabeledTree.Leaf ]);
                   ( 3,
                     map4 node bool_gen (oneofl chooses)
                       (self (n - 1))
                       (self (n - 1)) );
                 ]))

(* following the type *)
let binomialhp_gen (chooses : int list) (bound : int) =
  let open QCheck.Gen in
  let node a b c = BinomialHeap.Node (a, b, c) in
  sized_size (int_bound bound)
  @@ fix (fun self n ->
         list_size (int_bound n)
         @@ map3 node (oneofl chooses) (oneofl chooses)
         @@ self (n - 1))

let pairinghp_gen (chooses : int list) (bound : int) =
  let node a l = Pairinghp.T (a, l) in
  QCheck.Gen.(
    sized_size (int_bound bound)
    @@ fix (fun self n ->
           match n with
           | 0 -> oneofl [ Pairinghp.E ]
           | n ->
               frequency
                 [
                   (2, oneofl [ Pairinghp.E ]);
                   ( 1,
                     QCheck.Gen.map2 node (oneofl chooses)
                     @@ list_repeat 1 (self (n - 1)) );
                   ( 1,
                     QCheck.Gen.map2 node (oneofl chooses)
                     @@ list_repeat 2 (self (n - 1)) );
                   ( 1,
                     QCheck.Gen.map2 node (oneofl chooses)
                     @@ list_repeat 3 (self (n - 1)) );
                 ]))

let physicistsq_gen (chooses : int list) (bound : int) =
  let open QCheck.Gen in
  let make x lenf f lenr r st = (x st, lenf st, f st, lenr st, r st) in
  make (list_gen chooses bound)
    (int_bound @@ (bound + 2))
    (map (fun l -> lazy l) @@ list_gen chooses bound)
    (int_bound @@ (bound + 2))
    (list_gen chooses bound)

let stream_gen (chooses : int list) (bound : int) =
  QCheck.Gen.map Realtimeq.of_list @@ list_gen chooses bound

let realtimeq_gen (chooses : int list) (bound : int) =
  let make a b c st = (a st, b st, c st) in
  make (stream_gen chooses bound) (list_gen chooses bound)
    (stream_gen chooses bound)

let skewhp_gen (chooses : int list) (bound : int) =
  let open QCheck.Gen in
  let make r x l tl st = Skewhp.Node (r st, x st, l st, tl st) in
  sized_size (int_bound bound)
  @@ fix (fun self n ->
         list_repeat n
         @@ make (oneofl chooses) (oneofl chooses) (list_gen chooses bound)
         @@ self (n - 1))

let choose_gen chooses bound tp =
  match tp with
  | T.Unit -> QCheck.Gen.pure V.U
  | T.Int -> QCheck.Gen.map (fun x -> V.I x) (int_gen chooses)
  | T.Nat ->
      QCheck.Gen.map
        (fun x -> V.I x)
        (int_gen @@ List.filter (fun x -> x >= 0) chooses)
  | T.IntList -> QCheck.Gen.map (fun x -> V.L x) (list_gen chooses bound)
  | T.IntTree -> QCheck.Gen.map (fun x -> V.T x) (tree_gen chooses bound)
  | T.IntTreeI -> QCheck.Gen.map (fun x -> V.TI x) (treei_gen chooses bound)
  | T.IntTreeB -> QCheck.Gen.map (fun x -> V.TB x) (treeb_gen chooses bound)
  | T.Bool -> QCheck.Gen.map (fun x -> V.B x) bool_gen
  (* | T.IfcInstr -> QCheck.Gen.map (fun x -> V.IInstr x) (instruction_gen chooses) *)
  (* | T.IfcInstrList -> *)
  (*     QCheck.Gen.map (fun x -> V.IInstrL x) (instruction_list_gen chooses bound) *)
  | T.IntBoolList ->
      QCheck.Gen.map (fun x -> V.IBL x) (iblist_gen chooses bound)
  | T.BoolIntBoolList ->
      QCheck.Gen.map (fun x -> V.BIBL x) (biblist_gen chooses bound)
  | T.Uninterp "binomialhp" ->
      QCheck.Gen.map (fun x -> V.Binomialhp x) (binomialhp_gen chooses bound)
  | T.Uninterp "pairinghp" ->
      QCheck.Gen.map (fun x -> V.Pairinghp x) (pairinghp_gen chooses bound)
  | T.Uninterp "physicistsq" ->
      QCheck.Gen.map (fun x -> V.Physicistsq x) (physicistsq_gen chooses bound)
  | T.Uninterp "realtimeq" ->
      QCheck.Gen.map (fun x -> V.Realtimeq x) (realtimeq_gen chooses bound)
  | T.Uninterp "skewhp" ->
      QCheck.Gen.map (fun x -> V.Skewhp x) (skewhp_gen chooses bound)
  | T.Uninterp name -> raise @@ failwith (spf "no generator for type %s" name)

let gens ~chooses ~num ~tps ~bound =
  let gens = List.map (choose_gen chooses bound) tps in
  let gen = QCheck.Gen.(flatten_l gens) in
  QCheck.Gen.generate ~rand:(Random.State.make [| Random.int 100 |]) ~n:num gen

let gen_one ~chooses ~num ~tp ~bound =
  let gen = choose_gen chooses bound tp in
  QCheck.Gen.generate ~rand:(Random.State.make [| Random.int 100 |]) ~n:num gen

let small_nums = List.init 19 (fun i -> i - 10)

let paddled_small_nums =
  try (List.hd small_nums - 1) :: (small_nums @ [ List.last small_nums + 1 ])
  with _ -> raise @@ failwith "init error: paddle_small_nums"

let small_gen_one1 ~tp =
  let gen = choose_gen small_nums 8 tp in
  QCheck.Gen.generate1 gen

let small_gen_one ~num ~tp =
  let gen = choose_gen small_nums 8 tp in
  QCheck.Gen.generate ~n:num gen

let small_gen ~num ~tps =
  QCheck.Gen.generate ~n:num @@ QCheck.Gen.flatten_l
  @@ List.map (choose_gen small_nums 4) tps

(* There is a bug in QCheck *)
let range_subset ~size low high st =
  let open QCheck.Gen in
  if not (low <= high && size <= high - low + 1) then
    invalid_arg "Gen.range_subset";
  (* The algorithm below is attributed to Floyd, see for example
     https://eyalsch.wordpress.com/2010/04/01/random-sample/
     https://math.stackexchange.com/questions/178690
     Note: the code be made faster by checking membership in [arr]
     directly instead of using an additional Set. None of our
     dependencies implements dichotomic search, so using Set is
     easier.
  *)
  let module ISet = Set.Make (Int) in
  let s = ref ISet.empty in
  let arr = Array.make size 0 in
  (* let () = Printf.printf "high:%i size:%i len(arr):%i\n" high size size in *)
  for i = high - size to high - 1 do
    let pos = int_range low i st in
    (* let () = Printf.printf "i:%i pos:%i\n" i pos in *)
    let choice = if ISet.mem pos !s then i else pos in
    (* let () = Printf.printf "idx:%i choice:%i\n" (i - low) choice in *)
    arr.(i - high + size) <- choice;
    s := ISet.add choice !s
  done;
  arr

let array_subset size arr st =
  range_subset ~size 0 (Array.length arr - 1) st |> Array.map (fun i -> arr.(i))

let choose_n_from_list n l =
  Array.to_list @@ QCheck.Gen.generate1 (array_subset n @@ Array.of_list l)

let choose_n_from_arr arr max_pool n =
  List.map Array.to_list @@ QCheck.Gen.generate ~n (array_subset max_pool arr)
