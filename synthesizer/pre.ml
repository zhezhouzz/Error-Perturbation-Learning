open Classify
open Primitive
open Basic_dt
module V = Value
module S = Sampling.Scache

let perturbation_pre_infer (cctx : Cctx.t) (scache : S.t)
    (sigma : V.t list -> bool) (client : V.t list -> V.t list option)
    (phi : V.t list -> bool) =
  let i_list = List.remove_duplicates @@ List.flatten scache.gs in
  match i_list with
  | [] ->
      raise
      @@ failwith
           (Printf.sprintf "even have no initial error input in %s" __FUNCTION__)
  | _ ->
      let to_value inp_idx = S.Mem.itov scache.mem inp_idx in
      let to_label inp_idx =
        let inp = to_value inp_idx in
        let outs = S.Mem.get_outs scache.mem inp_idx in
        if List.length outs == 0 then false
        else
          let good, bad =
            List.partition
              (fun outp ->
                match client outp with
                | None -> false
                | Some outp -> sigma inp && not (phi (inp @ outp)))
              outs
          in
          good >= bad
      in
      let spec = Infer.spec_infer cctx i_list to_value to_label in
      let in_pre, out_pre =
        List.partition (fun inp -> Specification.Spec.eval spec inp)
        @@ List.map to_value i_list
      in
      let () =
        Zlog.log_write
        @@ Printf.sprintf "spec: %s\nin_pre %s\nout_pre %s\n"
             (Specification.Spec.layout spec)
             (List.split_by_comma V.layout_l in_pre)
             (List.split_by_comma V.layout_l out_pre)
      in
      (spec, (in_pre, out_pre))

let max_qv_num = 3

let qv_name_space =
  [ "u"; "v"; "w" ] @ List.init max_qv_num (fun i -> Printf.sprintf "k%i" i)

let pre_infer_from_env env init_set qvnum =
  if qvnum > max_qv_num then
    raise @@ failwith (spf "the max qv num is %i\n" max_qv_num)
  else
    let qv = List.init qvnum (fun i -> (Tp.Int, List.nth qv_name_space i)) in
    let open Env in
    match env.cur_p with
    | None ->
        raise
        @@ failwith
             "Precondition Inference: the perturbation function does not \
              exists."
    | Some cur_p ->
        let args =
          List.map
            (fun (tp, idx) -> (tp, Language.Oplang.layout_var (tp, idx)))
            cur_p.prog.fin
        in
        let cctx = Cctx.mk_cctx args qv env.preds in
        let () =
          Zlog.log_write
          @@ Printf.sprintf "fset: %s\n" (Feature.layout_set cctx.Cctx.fset)
        in
        let scache =
          S.mk_generation_measure_only env.measure_cond init_set cur_p.prog
            env.sampling_rounds
        in
        perturbation_pre_infer cctx scache env.sigma
          (fun v -> snd @@ env.client env.library_inspector v)
          env.phi

let fvtable_init_size = 10000

let mk_neg_engine env init_set prog candidate num =
  let open Env in
  let _, data =
    S.eval_sampling init_set prog (Measure.mk_measure_cond env.i_err) (2 * num)
  in
  let client inp = snd @@ env.client env.library_inspector inp in
  let data =
    List.filter
      (fun inp ->
        if not (env.sigma inp) then false
        else
          match client inp with
          | None -> false
          | Some outp -> not @@ env.phi (inp @ outp))
      data
  in
  if List.length data < num then raise @@ failwith "low quality prog"
  else List.filter (fun inp -> Specification.Spec.eval candidate inp) data

let test_num = 10000

let mk_pos_engine env qc_conf candidate num =
  let open Env in
  let rec aux res =
    if List.length res > num then res
    else
      let _, data = Zquickcheck.Qc_baseline.baseline qc_conf env.tps test_num in
      let client inp = snd @@ env.client env.library_inspector inp in
      let data =
        List.filter
          (fun inp ->
            if not (env.sigma inp) then false
            else
              match client inp with
              | None -> false
              | Some outp -> env.phi (inp @ outp))
          data
      in
      aux (res @ data)
  in
  let data = aux [] in
  List.filter (fun inp -> not @@ Specification.Spec.eval candidate inp) data

let infer_verified_pre env qc_conf qvnum =
  if qvnum > max_qv_num then
    raise @@ failwith (spf "the max qv num is %i\n" max_qv_num)
  else
    let qv = List.init qvnum (fun i -> (Tp.Int, List.nth qv_name_space i)) in
    let open Env in
    match env.cur_p with
    | None ->
        raise
        @@ failwith
             "Precondition Inference: the perturbation function does not \
              exists."
    | Some cur_p ->
        let args =
          List.map
            (fun (tp, idx) -> (tp, Language.Oplang.layout_var (tp, idx)))
            cur_p.prog.fin
        in
        let pos_engine = mk_pos_engine env qc_conf in
        let neg_engine = mk_neg_engine env [ env.i_err ] [ cur_p.prog ] in
        let gather_neg_fv = raise @@ failwith "unimp" in
        let gather_pos_fv = raise @@ failwith "unimp" in
        let classify = raise @@ failwith "unimp" in
        let fset = [] in
        let rec aux (fvtable, candidate) =
          match pos_engine candidate test_num with
          | [] -> (
              match neg_engine candidate test_num with
              | [] -> candidate
              | neg_cexs ->
                  let () = gather_neg_fv fset fvtable neg_cexs in
                  let candidate' = classify fset fvtable in
                  aux (fvtable, candidate'))
          | pos_cexs ->
              let () = gather_pos_fv fset fvtable pos_cexs in
              let candidate' = classify fset fvtable in
              aux (fvtable, candidate')
        in
        let fvtable = Hashtbl.create fvtable_init_size in
        let candidate = Specification.Spec.mk_true env.tps qvnum in
        aux (fvtable, candidate)
