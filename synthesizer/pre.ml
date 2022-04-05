open Classify
open Primitive
open Basic_dt
module V = Value
module S = Sampling.Scache

let perturbation_pre_infer (cctx : Cctx.t) (scache : S.t)
    (sigma : V.t list -> bool) (client : V.t list -> V.t list option)
    (phi : V.t list -> bool) =
  let i_list = S.flatten_raw scache in
  match i_list with
  | [] ->
      raise
      @@ failwith
           (Printf.sprintf "even have no initial error input in %s" __FUNCTION__)
  | _ ->
      let to_value inp_idx = S.Mem.itov scache.mem inp_idx in
      let to_label inp_idx =
        let inp = to_value inp_idx in
        let outs =
          try S.Mem.get_outs scache.mem inp_idx
          with _ ->
            let () = Zlog.log_write @@ spf "%i(%s)" inp_idx (V.layout_l inp) in
            let () = Zlog.log_write @@ S.layout_raw scache in
            failwith "cannot find outs"
        in
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

module E = Sampling.Engine
module Spec = Specification.Spec

let inference_num_sampling = 1000

let infer_verified_pre env qc_conf prog sigma =
  let open Env in
  let args = sigma.Spec.args in
  let pos_engine = E.mk_qc_engine env.tps qc_conf in
  let neg_engine =
    E.mk_perb_engine [ env.i_err ] (Measure.mk_measure_cond env.i_err) prog
  in
  let cctx = Cctx.mk_cctx args sigma.Spec.qv env.preds in
  let pos_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> env.phi (inp @ outp)
  in
  let neg_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> not @@ env.phi (inp @ outp)
  in
  Infer.spec_refine_loop ~cctx ~pos_engine ~neg_engine ~pos_filter ~neg_filter
    ~init_body:sigma.Spec.body inference_num_sampling

let infer_erroneous_pre env qc_conf prog sigma =
  let open Env in
  let args = sigma.Spec.args in
  let pos_engine = E.mk_qc_engine env.tps qc_conf in
  let neg_engine = E.mk_perb_engine [ env.i_err ] (fun _ -> true) prog in
  let qv = [ (T.Int, "u"); (T.Int, "v") ] in
  let cctx = Cctx.mk_cctx args qv env.preds in
  let pos_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> env.phi (inp @ outp)
  in
  let neg_filter _ = true in
  let spec =
    Infer.spec_infer_once ~cctx ~pos_engine ~neg_engine ~pos_filter ~neg_filter
      inference_num_sampling
  in
  fun x -> not @@ Spec.eval spec x

let enum_pos env qc_conf num_sampling =
  let open Env in
  let pos_engine = E.mk_qc_engine env.tps qc_conf in
  let pos_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> env.phi (inp @ outp)
  in
  let _, pos_values = E.sampling_num pos_filter num_sampling pos_engine in
  pos_values

let enum_pos_b env qc_conf num_sampling =
  let open Env in
  let pos_engine = E.mk_qc_engine [ T.IntList; T.Int ] qc_conf in
  let convert = function
    | [ V.L f; V.I x ] -> [ V.I (List.length f); V.L f; V.I 0; V.L []; V.I x ]
    | _ -> raise @@ failwith "die"
  in
  let pos_filter inp =
    let inp = convert inp in
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> env.phi (inp @ outp)
  in
  let _, pos_values = E.sampling_num pos_filter num_sampling pos_engine in
  let pos_values = List.map convert pos_values in
  pos_values

let infer_erroneous_pre_v3 env pos prog sigma =
  let open Env in
  let args = sigma.Spec.args in
  let neg_engine =
    E.mk_perb_engine ~if_coverage:true [ env.i_err ] (fun _ -> true) prog
  in
  let qv = [ (T.Int, "u"); (T.Int, "v") ] in
  let cctx = Cctx.mk_cctx args qv env.preds in
  let pos_values = Primitive.Inpmap.get_inps pos inference_num_sampling in
  let neg_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> not @@ env.phi (inp @ outp)
  in
  match E.sampling_pt_opt neg_filter inference_num_sampling neg_engine with
  | None -> fun _ -> false
  | Some neg_values ->
      let () =
        Zlog.log_write
        @@ spf "len(neg_values): %i(%i)" (List.length neg_values)
             inference_num_sampling
      in
      let spec = Infer.pn_spec_infer cctx pos_values neg_values in
      let () = Zlog.log_write @@ Spec.layout spec in
      if Spec.is_true spec then
        let spec = Infer.pn_spec_infer cctx neg_values pos_values in
        let () = Zlog.log_write @@ Spec.layout spec in
        fun x -> Spec.eval spec x
      else fun x -> not @@ Spec.eval spec x

let infer_erroneous_pre_v2 env qc_conf prog sigma =
  let open Env in
  let args = sigma.Spec.args in
  let pos_engine = E.mk_qc_engine env.tps qc_conf in
  let neg_engine =
    E.mk_perb_engine ~if_coverage:true [ env.i_err ] (fun _ -> true) prog
  in
  let qv = [ (T.Int, "u"); (T.Int, "v") ] in
  let cctx = Cctx.mk_cctx args qv env.preds in
  let pos_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> env.phi (inp @ outp)
  in
  let neg_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> not @@ env.phi (inp @ outp)
  in
  let _, pos_values =
    E.sampling_num pos_filter inference_num_sampling pos_engine
  in
  let () = raise @@ failwith "zz" in
  let _, neg_values =
    E.sampling_num neg_filter inference_num_sampling neg_engine
  in
  let () =
    Zlog.log_write
    @@ spf "len(neg_values): %i(%i)" (List.length neg_values)
         inference_num_sampling
  in
  let () = raise @@ failwith "zz" in
  let spec = Infer.pn_spec_infer cctx pos_values neg_values in
  fun x -> not @@ Spec.eval spec x

let infer_erroneous_pre_moti env qc_conf prog sigma =
  let open Env in
  let args = sigma.Spec.args in
  let pos_engine = E.mk_qc_engine env.tps qc_conf in
  let neg_engine = E.mk_perb_engine [ env.i_err ] (fun _ -> true) prog in
  let qv = [ (T.Int, "u"); (T.Int, "v") ] in
  let cctx = Cctx.mk_cctx args qv env.preds in
  let pos_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> env.phi (inp @ outp)
  in
  let neg_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> not @@ env.phi (inp @ outp)
  in
  let _, pos_values =
    E.sampling_num pos_filter inference_num_sampling pos_engine
  in
  match E.sampling_pt_opt_moti neg_filter inference_num_sampling neg_engine with
  | None -> None
  | Some neg_values ->
      let spec = Infer.pn_spec_infer cctx pos_values neg_values in
      let () = Zlog.log_write @@ Spec.layout spec in
      Some (fun x -> not @@ Spec.eval spec x)

let infer_erroneous_pre_moti_pos env pos_values prog sigma =
  let open Env in
  let args = sigma.Spec.args in
  let neg_engine = E.mk_perb_engine [ env.i_err ] (fun _ -> true) prog in
  let qv = [ (T.Int, "u"); (T.Int, "v") ] in
  let cctx = Cctx.mk_cctx args qv env.preds in
  let neg_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> not @@ env.phi (inp @ outp)
  in
  match E.sampling_pt_opt neg_filter inference_num_sampling neg_engine with
  | None -> None
  | Some neg_values ->
      let spec = Infer.pn_spec_infer cctx pos_values neg_values in
      Some (fun x -> not @@ Spec.eval spec x)

let infer_pre_multi env qc_conf prog sigma =
  let open Env in
  let args = sigma.Spec.args in
  let pos_engine = E.mk_qc_engine env.tps qc_conf in
  let fs, d = prog in
  let progs = ([], d) :: List.map (fun (_, f) -> ([], f)) fs in
  let neg_engines =
    List.map (E.mk_perb_engine [ env.i_err ] (fun _ -> true)) progs
  in
  let qv = [ (T.Int, "u"); (T.Int, "v") ] in
  let cctx = Cctx.mk_cctx args qv env.preds in
  let pos_filter inp =
    if not @@ env.sigma inp then false
    else
      let _, outp = env.client env.library_inspector inp in
      match outp with None -> false | Some outp -> env.phi (inp @ outp)
  in
  let neg_filter _ = true in
  let spec =
    Infer.spec_infer_once_multi_engine ~cctx ~pos_engine ~neg_engines
      ~pos_filter ~neg_filter inference_num_sampling
  in
  spec
