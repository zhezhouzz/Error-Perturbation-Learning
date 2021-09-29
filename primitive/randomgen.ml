open Basic_dt
module V = Value
module T = Tp

let bool_gen = QCheck.Gen.oneofl [true; false]

let int_gen (chooses: int list) = QCheck.Gen.oneofl chooses

let list_gen (chooses: int list) (bound: int) =
  QCheck.Gen.(list_size (int_bound bound) (oneofl chooses))

let tree_gen (chooses: int list) (bound: int) =
  let node a l r = Tree.Node (a, l, r) in
  QCheck.Gen.((sized_size (int_bound bound)) @@ fix
                (fun self n -> match n with
                   | 0 -> oneofl [Tree.Leaf]
                   | n ->
                     frequency
                       [1, oneofl [Tree.Leaf];
                        3, QCheck.Gen.map3 node (oneofl chooses)
                          (self (n - 1)) (self (n - 1))]
                ))

let treei_gen (chooses: int list) (bound: int) =
  let node i a l r = LabeledTree.Node (i, a, l, r) in
  QCheck.Gen.(
    let map4 f x y z k st = f (x st) (y st) (z st) (k st) in
    (sized_size (int_bound bound)) @@ fix
      (fun self n -> match n with
         | 0 -> oneofl [LabeledTree.Leaf]
         | n ->
           frequency
             [1, oneofl [LabeledTree.Leaf];
              3, map4 node (oneofl chooses) (oneofl chooses)
                (self (n - 1)) (self (n - 1))]
      )
  )

let treeb_gen (chooses: int list) (bound: int) =
  let node b a l r = LabeledTree.Node (b, a, l, r) in
  QCheck.Gen.(
    let map4 f x y z k st = f (x st) (y st) (z st) (k st) in
    (sized_size (int_bound bound)) @@ fix
      (fun self n -> match n with
         | 0 -> oneofl [LabeledTree.Leaf]
         | n ->
           frequency
             [1, oneofl [LabeledTree.Leaf];
              3, map4 node bool_gen (oneofl chooses)
                (self (n - 1)) (self (n - 1))]
      )
  )

let unique_gen gen num eq =
  let rec aux r =
    let trs = QCheck.Gen.generate ~rand:(Random.State.make [|Random.int 100|]) ~n:num gen in
    (* let trs = QCheck.Gen.generate ~rand:(Random.get_state ()) ~n:num gen in *)
    let trs = List.remove_duplicates eq (r @ trs) in
    if List.length trs < num then aux trs else
      List.sublist trs (0, num)
  in
  aux []

let randomgen_list (chooses: int list) (num: int) (bound: int) =
  (* let _ = Printf.printf "chooses:%s\n" (IntList.to_string chooses) in *)
  (* let chooses = [0;1;2] in *)
  let list_gen = list_gen chooses bound in
  let result = List.map (fun l -> V.L l) @@
    [] :: (unique_gen list_gen num IntList.eq) in
  (* let  _ = Printf.printf "list:\n";
   *   List.iter (fun l -> Printf.printf "%s\n" (V.layout l)) result in *)
  result

let randomgen_tree (chooses: int list) (num: int) (bound: int) =
  (* let _ = Printf.printf "chooses:%s\n" (IntList.to_string chooses) in *)
  let tree_gen = tree_gen chooses bound in
  Tree.Leaf :: (unique_gen tree_gen num (Tree.eq (fun x y -> x == y)))

let randomgen_labeled_tree gen (_: int list) (num: int) (bound: int) =
  (* let _ = Printf.printf "chooses:%s\n" (IntList.to_string chooses) in *)
  let chooses = [0;1;2] in
  (* let _ = raise @@ InterExn "zz" in *)
  let node labal a l r = LabeledTree.Node (labal, a, l, r) in
  let tree_gen =
    QCheck.Gen.(
      let map4 f x y z k st = f (x st) (y st) (z st) (k st) in
      (sized_size (int_bound bound)) @@ fix
        (fun self n -> match n with
           | 0 -> oneofl [LabeledTree.Leaf]
           | n ->
             frequency
               [1, oneofl [LabeledTree.Leaf];
                3, map4 node gen (oneofl chooses)
                  (self (n - 1)) (self (n - 1))]
        ))
  in
  unique_gen tree_gen num (LabeledTree.eq (fun x y -> x == y) (fun x y -> x == y))

let randomgen_labeled_treei chooses bound num =
  List.map (fun t -> LabeledTree.from_tree 0 t) (randomgen_tree chooses bound num)

let randomgen_labeled_treeb chooses bound num =
  List.map (fun t -> LabeledTree.from_tree true t)
    (randomgen_tree chooses bound num)

let gen ~chooses ~num ~tp ~bound =
  match tp with
  | T.Int -> List.map (fun i -> V.I i) chooses
  | T.IntList -> randomgen_list chooses num bound
  | T.IntTree -> List.map (fun l -> V.T l) @@ randomgen_tree chooses num bound
  | T.IntTreeI -> List.map (fun l -> V.TI l) @@ randomgen_labeled_treei chooses num bound
  | T.IntTreeB -> List.map (fun l -> V.TB l) @@ randomgen_labeled_treeb chooses num bound
  | T.Bool -> [V.B true; V.B false]

let choose_gen chooses bound tp =
  match tp with
  | T.Int -> QCheck.Gen.map (fun x -> V.I x) (int_gen chooses)
  | T.IntList -> QCheck.Gen.map (fun x -> V.L x) (list_gen chooses bound)
  | T.IntTree -> QCheck.Gen.map (fun x -> V.T x) (tree_gen chooses bound)
  | T.IntTreeI -> QCheck.Gen.map (fun x -> V.TI x) (treei_gen chooses bound)
  | T.IntTreeB -> QCheck.Gen.map (fun x -> V.TB x) (treeb_gen chooses bound)
  | T.Bool -> QCheck.Gen.map (fun x -> V.B x) bool_gen

let gens ~chooses ~num ~tps ~bound =
  let gens = List.map (choose_gen chooses bound) tps in
  let gen = QCheck.Gen.(flatten_l gens) in
  QCheck.Gen.generate ~rand:(Random.State.make [|Random.int 100|]) ~n:num gen

let gen_one ~chooses ~num ~tp ~bound =
  let gen = choose_gen chooses bound tp in
  QCheck.Gen.generate ~rand:(Random.State.make [|Random.int 100|]) ~n:num gen

let small_nums = List.init 9 (fun i -> i - 5)

let small_gen_one ~num ~tp =
  let gen = choose_gen small_nums 8 tp in
  QCheck.Gen.generate ~rand:(Random.State.make [|Random.int 100|]) ~n:num gen

let small_gen ~num ~tps =
  QCheck.Gen.generate ~rand:(Random.State.make [|Random.int 100|]) ~n:num @@
  QCheck.Gen.flatten_l @@ List.map (choose_gen small_nums 4) tps

let gen_tpvars ~tpvars ~num ~fv_num ~bound =
  let intvars, dtvars, bvars =
    List.fold_lefti (fun (intvars, dtvars, bvars) idx (tp, _) ->
        if T.is_dt tp then intvars, (tp, idx) :: dtvars, bvars else
          match tp with
          | T.Int -> (tp, idx) :: intvars, dtvars, bvars
          | T.Bool -> intvars, dtvars, (tp, idx) :: bvars
          | _ -> raise @@ failwith "gen_tpvars"
      ) ([], [], []) tpvars in
  (* let _ =
   *   List.iter (fun (tp, idx) -> Printf.printf "tp(%s)|idx(%i)\n" (T.layout tp) idx) intvars in
   * let _ =
   *   List.iter (fun (tp, idx) -> Printf.printf "tp(%s)|idx(%i)\n" (T.layout tp) idx) dtvars in *)
  let int_num = fv_num + (List.length intvars) + 1 in
  let chooses = List.init int_num (fun i -> i) in
  let bsamples = List.map (fun (tp, idx) ->
      idx, gen ~chooses:chooses ~num:num ~tp:tp ~bound:bound) bvars in
  let intsamples = match intvars with
    | (_, idx) :: t ->
      (idx, [V.I 0]) :: (List.map (fun (tp, idx) ->
          idx, gen ~chooses:chooses ~num:num ~tp:tp ~bound:bound) t)
    | [] -> [] in
  let dtsamples = List.map (fun (tp, idx) ->
      idx, gen ~chooses:chooses ~num:num ~tp:tp ~bound:bound) dtvars in
  (* let _ = List.iter (fun tr ->
   *     Printf.printf "tr:%s\n" (V.layout tr)
   *   ) (snd (List.nth dtsamples 0)) in *)
  let _, samples = List.split
      (List.sort (fun (a, _) (b, _) -> (compare a b)) (intsamples @ dtsamples @ bsamples)) in
  chooses, List.choose_list_list_order samples
