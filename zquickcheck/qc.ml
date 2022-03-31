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

let nat_gen conf =
  match conf with
  | SmallUnsign -> Gen.small_nat
  | LowUpper (l, u) -> Gen.int_range (if l < 0 then 0 else l) u

let list_gen conf =
  let elem_conf, size_conf = conf in
  let elem_gen = int_gen elem_conf in
  match size_conf with
  | SmallNat -> Gen.list elem_gen
  | SizeBound n -> Gen.(list_size (int_bound n) elem_gen)

(* let instruction_gen config = *)
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
(*       (int_bound 6) (int_gen config)) *)

(* let instruction_list_gen conf = *)
(*   let elem_conf, size_conf = conf in *)
(*   let elem_gen = instruction_gen elem_conf in *)
(*   match size_conf with *)
(*   | SmallNat -> Gen.list elem_gen *)
(*   | SizeBound n -> Gen.(list_size (int_bound n) elem_gen) *)

let iblist_gen conf =
  let elem_conf, size_conf = conf in
  let elem_gen = Gen.pair (int_gen elem_conf) bool_gen in
  match size_conf with
  | SmallNat -> Gen.list elem_gen
  | SizeBound n -> Gen.(list_size (int_bound n) elem_gen)

let biblist_gen conf =
  let elem_conf, size_conf = conf in
  let elem_gen = Gen.triple bool_gen (int_gen elem_conf) bool_gen in
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
        if n > Measure.tree_max_depth then return None
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
        if n > Measure.tree_max_depth then return None
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

let helper max_depth size_conf f =
  let body =
    Gen.(
      fun n ->
        if n > max_depth then
          (* let () = Printf.printf "n = %i > max_depth(%i)\n" n max_depth in *)
          return None
        else
          (* let () = *)
          (*   Printf.printf "n = %i <= max_depth(%i); make 20\n" n max_depth *)
          (* in *)
          map (fun x -> Some x) @@ f n)
  in
  match size_conf with
  | SmallNat -> Gen.sized body
  | SizeBound n -> Gen.(sized_size (int_bound n) body)

(* following the type *)
let binomialhp_gen conf label_gen =
  let elem_conf, size_conf = conf in
  let elem_gen = int_gen elem_conf in
  let node a b c = BinomialHeap.Node (a, b, c) in
  let f bound =
    Gen.(
      sized_size (int_bound bound)
      @@ fix (fun self n ->
             list_size (int_bound n)
             @@ map3 node label_gen elem_gen
             @@ self (n - 1)))
  in
  helper Measure.binomialhp_max_deep size_conf f

let pairinghp_gen conf =
  let elem_conf, size_conf = conf in
  let elem_gen = int_gen elem_conf in
  let node a l = Pairinghp.T (a, l) in
  let f bound =
    Gen.(
      sized_size (int_bound bound)
      @@ fix (fun self n ->
             match n with
             | 0 -> oneofl [ Pairinghp.E ]
             | n ->
                 frequency
                   [
                     (2, oneofl [ Pairinghp.E ]);
                     ( 1,
                       QCheck.Gen.map2 node elem_gen
                       @@ list_repeat 1 (self (n - 1)) );
                     ( 1,
                       QCheck.Gen.map2 node elem_gen
                       @@ list_repeat 2 (self (n - 1)) );
                     ( 1,
                       QCheck.Gen.map2 node elem_gen
                       @@ list_repeat 3 (self (n - 1)) );
                   ]))
  in
  helper Measure.pairinghp_max_deep size_conf f

let physicistsq_gen conf =
  let _, size_conf = conf in
  let make x lenf f lenr r st = (x st, lenf st, f st, lenr st, r st) in
  let f bound =
    Gen.(
      make (list_gen conf)
        (int_bound @@ (bound + 2))
        (map (fun l -> lazy l) @@ list_gen conf)
        (int_bound @@ (bound + 2))
        (list_gen conf))
  in
  helper Measure.physicistsq_max_deep size_conf f

let stream_gen conf = QCheck.Gen.map Realtimeq.of_list @@ list_gen conf

let realtimeq_gen conf =
  let make a b c st = (a st, b st, c st) in
  Gen.map (fun x -> Some x)
  @@ make (stream_gen conf) (list_gen conf) (stream_gen conf)

let skewhp_gen conf label_gen =
  let elem_conf, size_conf = conf in
  let elem_gen = int_gen elem_conf in
  let make r x l tl st = Skewhp.Node (r st, x st, l st, tl st) in
  let f bound =
    Gen.(
      sized_size (int_bound bound)
      @@ fix (fun self n ->
             list_size (int_bound n)
             @@ make label_gen elem_gen (list_size (int_bound n) elem_gen)
             @@ self (n - 1)))
  in
  helper Measure.skewhp_max_deep size_conf f

let choose_gen conf tp =
  match tp with
  | T.Unit -> QCheck.Gen.return (Some V.U)
  | T.Int -> QCheck.Gen.map (fun x -> Some (V.I x)) (int_gen conf.int_conf)
  | T.Nat -> QCheck.Gen.map (fun x -> Some (V.I x)) (nat_gen conf.int_conf)
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
  (* | T.IfcInstr -> *)
  (*     QCheck.Gen.map *)
  (*       (fun x -> Some (V.IInstr x)) *)
  (*       (instruction_gen conf.int_conf) *)
  (* | T.IfcInstrList -> *)
  (*     QCheck.Gen.map *)
  (*       (fun x -> Some (V.IInstrL x)) *)
  (*       (instruction_list_gen conf.list_conf) *)
  | T.IntBoolList ->
      QCheck.Gen.map (fun x -> Some (V.IBL x)) (iblist_gen conf.list_conf)
  | T.BoolIntBoolList ->
      QCheck.Gen.map (fun x -> Some (V.BIBL x)) (biblist_gen conf.list_conf)
  | T.Uninterp "binomialhp" ->
      QCheck.Gen.map
        (function None -> None | Some x -> Some (V.Binomialhp x))
        (binomialhp_gen conf.binomialhp_conf (int_gen conf.int_conf))
  | T.Uninterp "pairinghp" ->
      QCheck.Gen.map
        (function None -> None | Some x -> Some (V.Pairinghp x))
        (pairinghp_gen conf.pairinghp_conf)
  | T.Uninterp "physicistsq" ->
      QCheck.Gen.map
        (function None -> None | Some x -> Some (V.Physicistsq x))
        (physicistsq_gen conf.physicistsq_conf)
  | T.Uninterp "realtimeq" ->
      QCheck.Gen.map
        (function None -> None | Some x -> Some (V.Realtimeq x))
        (realtimeq_gen conf.realtimeq_conf)
  | T.Uninterp "skewhp" ->
      QCheck.Gen.map
        (function None -> None | Some x -> Some (V.Skewhp x))
        (skewhp_gen conf.skewhp_conf (int_gen conf.int_conf))
  | T.Uninterp name -> raise @@ failwith (spf "no generator for type %s" name)
