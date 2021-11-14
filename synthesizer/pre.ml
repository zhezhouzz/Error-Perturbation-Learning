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

let max_qv_num = 100

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
          S.mk_generation_measure_only
            (Measure.mk_measure_cond env.i_err)
            init_set cur_p.prog env.sampling_rounds
        in
        perturbation_pre_infer cctx scache env.sigma
          (fun v -> snd @@ env.client env.library_inspector v)
          env.phi
