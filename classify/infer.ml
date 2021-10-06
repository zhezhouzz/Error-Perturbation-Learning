open Primitive;;
module T = Tp;;
module V = Value;;
module F = Feature;;
module FV = Feature_vector;;
open Basic_dt;;
let mk_infer_ctx args qv mps = FV.({args = args; qv = qv; fset = F.mk_set args qv mps; labeled_vecs = []})

let pre_infer ctx values judge =
  let _ = if List.length values == 0 then raise @@ failwith "no data in precondition inference" else () in
  let qv_values = List.map (fun i -> V.I i) Randomgen.paddled_small_nums in
  let qvs_values = List.choose_n qv_values (List.length ctx.FV.qv) in
  (* let _ = List.iteri (fun i vs -> Printf.printf "qv(%i) = [%s]" i (V.layout_l vs)) qvs_values in *)
  let data = FV.RealSample.add_to_feature_vectors ctx judge values qvs_values in
  let _ = Printf.printf "data:\n%s\n" @@ FV.layout data in
  let dt = Dtree.classify data in
  let body = Dtree.to_prop dt in
  let _ = Printf.printf "spec: %s\n" (Specification.Prop.layout body) in
  ();;
