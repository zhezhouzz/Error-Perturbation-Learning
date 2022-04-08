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

let filter c phi samples =
  List.filter
    (fun d -> match c d with None -> false | Some x -> not @@ phi (d @ x))
    samples

let chosen = [ -1; 0; 1; 2; 3; 4; 5 ]

let make_env_from_elrond spec name _ =
  let tps, imp = make_client name in
  let phi = Spec.eval spec in
  let d = Randomgen.gens ~chooses:chosen ~num:2000 ~tps ~bound:4 in
  let _ = Zlog.log_write @@ spf "%s len(d): %i" name (List.length d) in
  let a = filter imp phi d in
  let _ = Zlog.log_write @@ spf "%s len(a): %i" name (List.length a) in
  ()

(* let rec aux = function *)
(*   | [] -> None *)
(*   | i_err :: t -> ( *)
(*       let sigma_raw = Spec.dummy_pre tps in *)
(*       try *)
(*         let env = *)
(*           Mkenv.mk_env_v2_ sigma_raw *)
(*             (fun _ -> true) *)
(*             (fun _ x -> ([], imp x)) *)
(*             Env.BB.dummy_inspector phi tps i_err (make_op_pool name) [] 6 4 *)
(*         in *)
(*         Some (env, a) *)
(*       with _ -> aux t) *)
(* in *)
(* aux a *)

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
  filter (Mkenv.to_c env) env.phi d

let pfs_to_sampless env pfs samples =
  Value_aux.remove_duplicates_l @@ List.flatten
  @@ List.map
       (fun pf -> pf_to_sampless env pf samples)
       (snd pfs :: List.map snd (fst pfs))
