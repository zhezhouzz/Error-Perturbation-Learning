let mk_default_env () =
  let i_err = Ifc.Test.i_err_raw in
  Synthesizer.Mkenv.mk_env_v2_ Ifc.Test.sigma_raw
    (let c = Ifc.Test.client_raw Ifc.Test.error in
     fun _ inp -> ([], c inp))
    (Language.Bblib.invocation_inspector_init [])
    Ifc.Test.phi_raw Ifc.Test.tps_raw Ifc.Test.i_err_raw
    [
      "iblist_destruct";
      "iblist_cons";
      "biblist_destruct";
      "biblist_cons";
      "plus1";
    ]
    [] 15 5

let ifc_sampling_ progfile num_sampling =
  let prog = Parse.parse progfile in
  let env = mk_default_env () in
  let (_, num_none, data), cost_time =
    Zlog.event_time_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "sampling")
      (fun () ->
        Sampling.Scache.eval_sampling [ env.i_err ] [ prog ]
          (Primitive.Measure.mk_measure_cond env.i_err)
          num_sampling)
  in
  let stat =
    Zlog.event_
      (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "evaluation")
      (fun () ->
        Evaluation.Ev.evaluation num_none data env.sigma
          (fun inp -> snd @@ env.client env.library_inspector inp)
          env.phi)
  in
  let () =
    Printf.printf "%s\n" @@ Evaluation.Ev.layout_eval "" stat cost_time
  in
  ()

open Core
open Caux

let ifc_sampling =
  Command.basic ~summary:"ifc_sampling"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and prog_file = anon ("prog file" %: regular_file)
      and num_sampling = anon ("num sampling" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            ifc_sampling_ prog_file num_sampling))

let ifc_syn =
  Command.basic ~summary:"ifc_syn"
    Command.Let_syntax.(
      let%map_open configfile = anon ("configfile" %: regular_file)
      and max_length = anon ("maximal length of the pieces" %: int)
      and bound_time = anon ("time in second" %: int) in
      fun () ->
        Config.exec_main configfile (fun () ->
            let env = Synthesizer.Mkenv.random_init_prog @@ mk_default_env () in
            let result =
              Zlog.event_
                (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
                (fun () ->
                  (* Synthesizer.Syn.synthesize_piecewise env max_length num_burn_in *)
                  (*   num_sampling *)
                  Synthesizer.Syn.synthesize_multi_core env max_length
                    (Synthesizer.Syn.TimeBound (float_of_int bound_time)))
            in
            let () =
              Printf.printf "%s\n"
              @@ Language.Piecewise.layout_with_i_err env.i_err result
            in
            ()))
