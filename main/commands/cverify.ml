open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux
open Specification
module S = Specification.Specast
module Sepc = Specification.Spec
open Primitive
open Caux

let verify_ source_file meta_file prog_file qc_file verified_sigma_file =
  let env = mk_env_from_files source_file meta_file in
  let i_err, prog = Parse.parse_piecewise prog_file in
  let env = Synthesizer.Mkenv.update_i_err env i_err in
  let qc_conf = Qc_config.load_config qc_file in
  let sigma = env.Synthesizer.Env.sigma_raw in
  let verified_sigma =
    Parse.parse_verified_sigma sigma.Spec.args verified_sigma_file
  in
  let spec = Synthesizer.Pre.infer_verified_pre env qc_conf prog sigma in
  let res = Spec.check_verified ~verified_sigma ~spec in
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
