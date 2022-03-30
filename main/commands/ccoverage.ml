open Zlog
open Printf

let ppf = Format.err_formatter

open Core
open Caux
module V = Primitive.Value

let e_0 inputs =
  let rec inc i = function
    | [] -> true
    | h :: t -> Int.equal h i && inc (i + 1) t
  in
  match inputs with
  | [ V.L s1; V.L s2 ] ->
      List.equal ( = ) [ 3; 4 ] s2 && List.length s1 >= 2 && inc 1 s1
  | _ -> false

let gen_from_target_perturbation source_file meta_file target_prog_file
    prog_file qc_conf test_num =
  let env =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () ->
        Synthesizer.Mkenv.random_init_prog
        @@ mk_env_from_files source_file meta_file)
  in
  let _, prog = Parse.parse_piecewise prog_file in
  let epre =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
      (fun () -> Synthesizer.Syn.synthesize_erroneous_pre env qc_conf prog)
  in
  let i_err, target_prog = Parse.parse_piecewise target_prog_file in
  let env = Synthesizer.Mkenv.update_i_err env i_err in
  let conds =
    Sampling.Scache.mk_conds
      (fun _ -> true)
      env.sigma
      (fun inp -> snd @@ env.client env.library_inspector inp)
      env.phi
      (fun _ -> true)
  in
  let pool, num_none, data =
    Sampling.Scache.target_sampling conds [ i_err ] target_prog test_num
  in
  let () = Printf.printf "pre: %s\n" @@ Specification.Spec.layout epre in
  let n = List.length @@ List.filter ~f:e_0 data in
  let () = Printf.printf "e0: %i/%i\n" n (List.length data) in
  let n =
    List.length
    @@ List.filter ~f:(fun x -> not @@ Specification.Spec.eval epre x) data
  in
  let () = Printf.printf "pre: %i/%i\n" n (List.length data) in
  ()

let coverage =
  Command.basic ~summary:"coverage"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and source_file = anon ("source file" %: regular_file)
      and meta_file = anon ("meta file" %: regular_file)
      and target_prog_file = anon ("target prog file" %: regular_file)
      and prog_file = anon ("prog file" %: regular_file)
      and qc_file = anon ("qc file" %: regular_file)
      and test_num = anon ("test_num" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let qc_conf = Qc_config.load_config qc_file in
            gen_from_target_perturbation source_file meta_file target_prog_file
              prog_file qc_conf test_num))
