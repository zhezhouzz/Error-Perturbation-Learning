open Synthesizer
open Specification
open Primitive
open Basic_dt

let make_client name =
  let aux = function
    | "Batchedq.rev" -> "rev"
    | "Batchedq.nil" -> "nil"
    | "Batchedq.is_empty" -> "is_empty"
    | "Batchedq.cons" -> "cons"
    | name -> failwith name
  in
  let name = aux name in
  let info =
    List.find "imp" (fun info -> String.equal name info.Imp.imp_name) Imps.imps
  in
  (info.Imp.imp_itps, info.Imp.imp_exec)

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

let snum = 10

let pf_to_sampless env pf samples =
  let rec aux i (s, res) =
    if i >= snum then res
    else
      let s' =
        List.filter_map (fun x -> Language.Oplang_interp.interp pf x) s
      in
      aux (i + 1) (s', s' @ res)
  in
  let d = aux 0 (samples, samples) in
  List.filter
    (fun d ->
      match Mkenv.to_c env d with
      | None -> false
      | Some x -> not @@ env.Env.phi (d @ x))
    d

let pfs_to_sampless env pfs samples =
  Value_aux.remove_duplicates_l @@ List.flatten
  @@ List.map
       (fun pf -> pf_to_sampless env pf samples)
       (snd pfs :: List.map snd (fst pfs))
