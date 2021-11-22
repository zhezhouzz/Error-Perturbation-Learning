open Zlog
open Printf

let ppf = Format.err_formatter

open Core

let mk_standard_env () =
  let open Primitive in
  let i_err = [ Value.L [ 1; 2 ]; Value.L [ 3; 4 ] ] in
  let tps = [ Tp.IntList; Tp.IntList ] in
  let sampling_rounds = 6 in
  let prog_size = 4 in
  let libraries = [ "List" ] in
  let op_pool =
    [
      "insert";
      "replace";
      "cons";
      "append";
      "plus1";
      "minus1";
      "top";
      "bottom";
      "max";
      "min";
      "random_int";
      "const0";
      "const1";
    ]
  in
  let preds = [ "hd"; "mem" ] in
  let inspector = Language.Bblib.invocation_inspector_init libraries in
  Synthesizer.Mkenv.mk_env_v2_ Imp_const.sigma_merge Language.Bblib.merge
    inspector Imp_const.phi_merge tps i_err op_pool preds sampling_rounds
    prog_size

let mk_env_from_files source_file meta_file =
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
  Synthesizer.Mkenv.mk_env_v2 sigma client libs phi tps i_err op_pool preds
    sampling_rounds p_size

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")

let parse_benchmark_config source_configfile =
  let open Yojson.Basic.Util in
  let cases =
    Config.load_json source_configfile |> member "benchmarks" |> to_list
  in
  let get_setting j =
    let benchname = j |> member "name" |> to_string in
    let source_file = j |> member "source_file" |> to_string in
    let meta_file = j |> member "meta_file" |> to_string in
    let max_length = j |> member "max_length" |> to_int in
    let num_burn_in = j |> member "num_burn_in" |> to_int in
    let num_sampling = j |> member "num_sampling" |> to_int in
    let output_dir = j |> member "output_dir" |> to_string in
    ( benchname,
      source_file,
      meta_file,
      max_length,
      num_burn_in,
      num_sampling,
      output_dir )
  in
  List.map ~f:get_setting cases
