open Primitive
open Basic_dt
module V = Value
module T = Tp
open Qc_config
open QCheck

let bool_gen = Gen.oneofl [ true; false ]

let int_gen conf =
  match conf with
  | SmallUnsign -> Gen.small_signed_int
  | LowUpper (l, u) -> Gen.int_range l u

let list_gen conf =
  let elem_conf, size_conf = conf in
  let elem_gen = int_gen elem_conf in
  match size_conf with
  | SmallNat -> Gen.list elem_gen
  | SizeBound n -> Gen.(list_size (int_bound n) elem_gen)

let tree_gen conf =
  let elem_conf, size_conf, fq = conf in
  let elem_gen = int_gen elem_conf in
  let node a l r = Tree.Node (a, l, r) in
  let body =
    Gen.(
      fun n ->
        if Measure.size_measure n > Measure.tree_max_depth then return None
        else
          map (fun x -> Some x)
          @@ fix
               (fun self n ->
                 match n with
                 | 0 -> oneofl [ Tree.Leaf ]
                 | n ->
                     frequency
                       [
                         (fq.fq_leaf, return Tree.Leaf);
                         ( fq.fq_node,
                           QCheck.Gen.map3 node elem_gen
                             (self (n - 1))
                             (self (n - 1)) );
                       ])
               n)
  in
  match size_conf with
  | SmallNat -> Gen.sized body
  | SizeBound n -> Gen.(sized_size (int_bound n) body)

let labeled_tree_gen conf label_gen =
  let elem_conf, size_conf, fq = conf in
  let elem_gen = int_gen elem_conf in
  let node i a l r = LabeledTree.Node (i, a, l, r) in
  let map4 f x y z k st = f (x st) (y st) (z st) (k st) in
  let body =
    Gen.(
      fun n ->
        if Measure.size_measure n > Measure.tree_max_depth then return None
        else
          map (fun x -> Some x)
          @@ (fix (fun self n ->
                  match n with
                  | 0 -> return LabeledTree.Leaf
                  | n ->
                      frequency
                        [
                          (fq.fq_leaf, return LabeledTree.Leaf);
                          ( fq.fq_node,
                            map4 node label_gen elem_gen
                              (self (n - 1))
                              (self (n - 1)) );
                        ]))
               n)
  in
  match size_conf with
  | SmallNat -> Gen.sized body
  | SizeBound n -> Gen.(sized_size (int_bound n) body)

let choose_gen conf tp =
  match tp with
  | T.Unit -> QCheck.Gen.return (Some V.U)
  | T.Int -> QCheck.Gen.map (fun x -> Some (V.I x)) (int_gen conf.int_conf)
  | T.IntList ->
      QCheck.Gen.map (fun x -> Some (V.L x)) (list_gen conf.list_conf)
  | T.IntTree ->
      QCheck.Gen.map
        (function None -> None | Some x -> Some (V.T x))
        (tree_gen conf.tree_conf)
  | T.IntTreeI ->
      QCheck.Gen.map
        (function None -> None | Some x -> Some (V.TI x))
        (labeled_tree_gen conf.treei_conf (int_gen conf.int_conf))
  | T.IntTreeB ->
      QCheck.Gen.map
        (function None -> None | Some x -> Some (V.TB x))
        (labeled_tree_gen conf.treeb_conf bool_gen)
  | T.Bool -> QCheck.Gen.map (fun x -> Some (V.B x)) bool_gen
