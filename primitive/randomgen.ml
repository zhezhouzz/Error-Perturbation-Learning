open Basic_dt
module V = Value
module T = Tp

let bool_gen = QCheck.Gen.oneofl [ true; false ]

let int_gen (chooses : int list) = QCheck.Gen.oneofl chooses

let list_gen (chooses : int list) (bound : int) =
  QCheck.Gen.(list_size (int_bound bound) (oneofl chooses))

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

let choose_gen chooses bound tp =
  match tp with
  | T.Unit -> QCheck.Gen.pure V.U
  | T.Int -> QCheck.Gen.map (fun x -> V.I x) (int_gen chooses)
  | T.IntList -> QCheck.Gen.map (fun x -> V.L x) (list_gen chooses bound)
  | T.IntTree -> QCheck.Gen.map (fun x -> V.T x) (tree_gen chooses bound)
  | T.IntTreeI -> QCheck.Gen.map (fun x -> V.TI x) (treei_gen chooses bound)
  | T.IntTreeB -> QCheck.Gen.map (fun x -> V.TB x) (treeb_gen chooses bound)
  | T.Bool -> QCheck.Gen.map (fun x -> V.B x) bool_gen

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
