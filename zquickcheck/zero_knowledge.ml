open Primitive
open Basic_dt
module V = Value
module T = Tp

let bool_gen = QCheck.Gen.oneofl [ true; false ]

let int_gen (chooses : int list) = QCheck.Gen.oneofl chooses

let list_gen qc_conf (chooses : int list) =
  QCheck.Gen.(list_size (int_bound qc_conf.list) (oneofl chooses))

let tree_gen qc_conf (chooses : int list) =
  let node a l r = Tree.Node (a, l, r) in
  QCheck.Gen.(
    sized_size (int_bound qc_conf.tree.bound)
    @@ fix (fun self n ->
           match n with
           | 0 -> oneofl [ Tree.Leaf ]
           | n ->
               frequency
                 [
                   (qc_conf.tree.leaf, oneofl [ Tree.Leaf ]);
                   ( qc_conf.tree.node,
                     QCheck.Gen.map3 node (oneofl chooses)
                       (self (n - 1))
                       (self (n - 1)) );
                 ]))

let treei_gen qc_conf (chooses : int list) =
  let node i a l r = LabeledTree.Node (i, a, l, r) in
  QCheck.Gen.(
    let map4 f x y z k st = f (x st) (y st) (z st) (k st) in
    sized_size (int_bound qc_conf.labeled_tree.bound)
    @@ fix (fun self n ->
           match n with
           | 0 -> oneofl [ LabeledTree.Leaf ]
           | n ->
               frequency
                 [
                   (qc_conf.labeled_tree.leaf, oneofl [ LabeledTree.Leaf ]);
                   ( qc_conf.labeled_tree.node,
                     map4 node (oneofl chooses) (oneofl chooses)
                       (self (n - 1))
                       (self (n - 1)) );
                 ]))

let treeb_gen qc_conf (chooses : int list) =
  let node b a l r = LabeledTree.Node (b, a, l, r) in
  QCheck.Gen.(
    let map4 f x y z k st = f (x st) (y st) (z st) (k st) in
    sized_size (int_bound qc_conf.labeled_tree.bound)
    @@ fix (fun self n ->
           match n with
           | 0 -> oneofl [ LabeledTree.Leaf ]
           | n ->
               frequency
                 [
                   (qc_conf.labeled_tree.leaf, oneofl [ LabeledTree.Leaf ]);
                   ( qc_conf.labeled_tree.node,
                     map4 node bool_gen (oneofl chooses)
                       (self (n - 1))
                       (self (n - 1)) );
                 ]))

let choose_gen qc_conf chooses tp =
  match tp with
  | T.Unit -> QCheck.Gen.pure V.U
  | T.Int -> QCheck.Gen.map (fun x -> V.I x) (int_gen chooses)
  | T.IntList -> QCheck.Gen.map (fun x -> V.L x) (list_gen qc_conf chooses)
  | T.IntTree -> QCheck.Gen.map (fun x -> V.T x) (tree_gen qc_conf chooses)
  | T.IntTreeI -> QCheck.Gen.map (fun x -> V.TI x) (treei_gen qc_conf chooses)
  | T.IntTreeB -> QCheck.Gen.map (fun x -> V.TB x) (treeb_gen qc_conf chooses)
  | T.Bool -> QCheck.Gen.map (fun x -> V.B x) bool_gen

let small_nums = List.init 19 (fun i -> i - 10)

let small_gen ~qc_conf ~num ~tps =
  QCheck.Gen.generate ~n:num @@ QCheck.Gen.flatten_l
  @@ List.map (choose_gen qc_conf small_nums) tps
