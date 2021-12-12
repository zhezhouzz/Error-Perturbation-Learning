open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux
open Specification
module S = Specification.Specast
module Sepc = Specification.Spec
open Primitive

(* HACK: hardcoded  *)
let qv_num = 2

let verify_ source_file meta_file prog_file qc_file verified_sigma_file =
  let prog = Ocaml_parser.Frontend.parse ~sourcefile:source_file in
  let meta = Ocaml_parser.Frontend.parse ~sourcefile:meta_file in
  let ( sigma,
        client,
        libs,
        i_err,
        phi,
        tps,
        op_pool,
        preds,
        sampling_rounds,
        p_size ) =
    Language.Of_ocamlast.load_client_and_meta prog meta
  in
  let env =
    Synthesizer.Mkenv.mk_env_v2 sigma client libs phi tps i_err op_pool preds
      sampling_rounds p_size
  in
  let i_err, prog = Parse.parse_piecewise prog_file in
  let env = Synthesizer.Mkenv.update_i_err env i_err in
  let qc_conf = Qc_config.load_config qc_file in
  let verified_sigma =
    Parse.parse_verified_sigma sigma.Spec.args verified_sigma_file
  in
  let spec = Synthesizer.Pre.infer_verified_pre env qc_conf prog qv_num in
  let res = Spec.check_verified ~verified_sigma ~sigma ~spec in
  let () =
    Printf.printf "Verify Result: (Sound? %b) (Complete? %b)\n" (fst res)
      (snd res)
  in
  res

let verify =
  Command.basic ~summary:"verify"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and prog_file = anon ("prog file" %: regular_file)
      and qc_file = anon ("quickcheck config file" %: regular_file)
      and verified_sigma_file =
        anon ("verified sigma file file" %: regular_file)
      in
      fun () ->
        Config.exec_main configfile (fun () ->
            let a, cost_time =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s] evaluation" __FILE__ __LINE__
                   __FUNCTION__) (fun () ->
                  verify_ source_file meta_file prog_file qc_file
                    verified_sigma_file)
            in
            ()))
