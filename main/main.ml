open Core
open Commands

let command =
  Command.group ~summary:"Error Perturbation Learning"
    [
      ("test", Ctest.test);
      ("test-unbset", Ctest.test_unbset);
      ("test-qcgen", Ctest.qcgen_test);
      ("baseline", Cbaseline.baseline);
      ("baseline-time", Ctime.baseline_time);
      ("baseline-time-all", Ctime.baseline_time_all);
      ("sampling-time", Ctime.sampling_time);
      ("sampling-time-all", Ctime.sampling_time_all);
      ("analysis-baseline", Cev.analysis);
      ("batched-test", Ctest.batched_test);
      ("parse-input", Cparse.parse_input);
      ("parse-result", Cparse.parse_result);
      ("parse-result-one", Cparse.parse_result_one);
      ("eval-sampling", Cev.sampling);
      ("synthesize-piecewise", Csyn.synthesize_piecewise);
      ("synthesize", Csyn.synthesize);
      ("synthesize-time", Csyn.synthesize_time);
      ("synthesize-all", Csyn.synthesize_all);
      ("eval-baseline", Cev.eval_baseline);
      ("eval-result", Cev.eval_result);
      ("costing", Cev.costing);
      ("ifc-sampling", Cifc.ifc_sampling);
      ("ifc-syn", Cifc.ifc_syn);
      ("verify", Cverify.verify);
      ("moti-robu", Cmoti.moti_robu);
      ("moti", Cmoti.moti);
      ("moti-search", Cmoti.search);
      ("ind", Cind.ind);
      ("pie", Csyn.synthesize_pie);
      ("robu-init", Cind.robu_init);
      ("coverage", Ccoverage.coverage);
    ]

let () = Command.run command
