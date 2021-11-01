open Classify
open Primitive
open Basic_dt
module V = Value

let perturbation_pre_infer (cctx : Cctx.t) (acache : Sampling.cache)
    (sigma : V.t list -> bool) (client : V.t list -> V.t list option)
    (phi : V.t list -> bool) =
  let valuem, io_list = Sampling.pure_sampled_input_output acache in
  let io_list =
    List.filter (fun (in_idx, _) -> sigma @@ Hashtbl.find valuem in_idx) io_list
  in
  match io_list with
  | [] ->
      raise
      @@ failwith
           (Printf.sprintf "even have no initial error input in %s" __FUNCTION__)
  | _ ->
      let to_value (in_idx, _) = Hashtbl.find valuem in_idx in
      let to_label (_, out_idx) =
        let i_err' = Hashtbl.find valuem out_idx in
        match client i_err' with
        | None -> false
        | Some o_err' -> sigma i_err' && not (phi (i_err' @ o_err'))
      in
      let spec = Infer.spec_infer cctx io_list to_value to_label in
      let in_pre, out_pre =
        List.partition (fun inp -> Specification.Spec.eval spec inp)
        @@ List.map to_value io_list
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

let pre_infer_from_env env qvnum =
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
          Sampling.cost_sampling_ env.Env.tps [ env.Env.i_err ] cur_p.prog
            env.sampling_rounds
        in
        perturbation_pre_infer cctx scache env.sigma
          (fun v -> snd @@ env.client env.library_inspector v)
          env.phi
