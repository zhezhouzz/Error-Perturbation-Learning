open Primitive
open Basic_dt
module V = Value

let value_int_map v f =
  let open V in
  match v with
  | B _ | U | NotADt -> v
  | I i -> I (f i)
  | L l -> L (List.map f l)
  | T tr -> T (Tree.map f tr)
  | TI tr -> TI (LabeledTree.map f tr)
  | TB tr -> TB (LabeledTree.map f tr)

let value_shift v k = value_int_map v (fun x -> x + k)

let value_shift_to_min_zero_normalize v =
  let values = V.flatten_forall_l v in
  match IntList.min_opt values with
  | None -> v
  | Some minimal ->
      let diff = 0 - minimal in
      List.map (fun v -> value_shift v diff) v

let value_only_perserve_order_normalize v =
  let values = V.flatten_forall_l v in
  let tbl = Hashtbl.create 1000 in
  let _ =
    List.fold_left
      (fun counter v ->
        Hashtbl.add tbl v counter;
        counter + 1)
      0 values
  in
  let v = List.map (fun v -> value_int_map v (fun x -> Hashtbl.find tbl x)) v in
  v

type normalization_method =
  | Value_shift_to_min_zero_normalize
  | Value_only_perserve_order_normalize

let normalize nmethod vs =
  let f =
    match nmethod with
    | Value_shift_to_min_zero_normalize -> value_shift_to_min_zero_normalize
    | Value_only_perserve_order_normalize -> value_only_perserve_order_normalize
  in
  Value_aux.remove_duplicates_l @@ List.map f vs

(* extention factor *)
let extention_factor = 4

let test_cost env progs =
  let open Synthesizer.Env in
  let cost i_err name prog =
    let env = Synthesizer.Mkenv.update_i_err env i_err in
    let measure = Primitive.Measure.mk_measure_cond env.i_err in
    let () = Zlog.log_write @@ spf "[%s]" name in
    let conds =
      Sampling.Scache.mk_conds measure env.sigma
        (fun v -> snd @@ env.client env.library_inspector v)
        env.phi
        (fun _ -> true)
    in
    let scache =
      Sampling.Scache.mk_generation !Config.conf.bias_method [ env.i_err ] conds
        prog
        (env.sampling_rounds * extention_factor)
    in
    let cost =
      Synthesizer.Cost.cal_cost conds
        (fun v -> env.client env.library_inspector v)
        env.i_err_non_trivial_info scache
    in
    cost
  in
  List.iter
    (fun (i_err, (name, f)) ->
      Printf.printf "%s\ncost: %f\n\n" name (cost i_err name f))
    progs
