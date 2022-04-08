open Synthesizer
open Specification
open Primitive

let make_client name =
  let aux = function
    | "Batchedq.rev" -> "rev"
    | "Batchedq.nil" -> "nil"
    | "Batchedq.is_empty" -> "is_empty"
    | "Batchedq.cons" -> "cons"
    | name -> failwith name
  in
  let name = aux name in
  let open Operator in
  (fst @@ get_tp_one name, get_imp name)

let make_op_pool _ =
  [
    "cons";
    "append";
    "list_destruct";
    "list_last_destruct";
    "list_mid_partition";
    "list_alter_partition";
    "list_lower_bound";
    "list_upper_bound";
    "max";
    "min";
    "plus1";
    "minus1";
    "const0";
    "const1";
  ]

let make_env_from_elrond spec name i_err =
  let tps, imp = make_client name in
  let sigma_raw = Spec.dummy_pre tps in
  Mkenv.mk_env_v2_ sigma_raw
    (fun _ -> true)
    (fun _ x -> ([], imp x))
    Env.BB.dummy_inspector (Spec.eval spec) tps i_err (make_op_pool name) [] 6 4
