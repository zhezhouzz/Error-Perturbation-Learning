open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux
open Specification
module S = Specification.Specast
open Primitive

(* let refinement_loop env qvnum pos_engine neg_engine = *)
(*   let qv = List.init qvnum (fun i -> (Tp.Int, List.nth qv_name_space i)) in *)
(*   let cctx = Synthesizer.Cctx.mk_cctx args qv env.preds in *)
(*   let rec aux (fvtable, candidate) = *)
(*   match pos_engine candidate test_num with *)
(*   | [] -> *)
(*     (match neg_engine candidate test_num with *)
(*      | [] -> candidate *)
(*      | neg_cexs -> *)
(*        let () = gather_neg_fv fset fvtable neg_cexs in *)
(*        let candidate' = classify fset fvtable in *)
(*        aux (fvtable, candidate') *)
(*     ) *)
(*   | pos_cexs -> *)
(*     let () = gather_pos_fv fset fvtable pos_cexs in *)
(*     let candidate' = classify fset fvtable in *)
(*     aux (fvtable, candidate') *)
(*   in *)
(*   let fvtable = Hashtbl.create fvtable_init_size in *)
(*   let candidate = Spec.mk_true tps qvnum in *)
(*   aux (fvtable, candidate) *)

(* let infer source_file meta_file prog_file verified_sigma_file = *)
(*   let env = mk_env_from_files source_file meta_file in *)
(*   let i_err, prog = Parse.parse_piecewise prog_file in *)
(*   let env = Synthesizer.Mkenv.update_i_err env i_err in *)
(*   let verified_sigma = Parse.parse_verified_sigma sigma sigma_file in  *)
