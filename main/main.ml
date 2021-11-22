open Core
open Commands

let command =
  Command.group ~summary:"Error Perturbation Learning"
    [
      ("test", Ctest.test);
      ("baseline", Cbaseline.baseline);
      ("baseline-time", Cbaseline.baseline_time);
      ("baseline-time-all", Cbaseline.baseline_time_all);
      ("analysis-baseline", Cev.analysis);
      ("batched-test", Ctest.batched_test);
      ("parse-input", Cparse.parse_input);
      ("parse-result", Cparse.parse_result);
      ("parse-result-one", Cparse.parse_result_one);
      ("sampling", Cev.sampling);
      ("synthesize", Csyn.synthesize);
      ("synthesize-all", Csyn.synthesize_all);
      ("eval-baseline", Cev.eval_baseline);
      ("eval-result", Cev.eval_result);
      ("costing", Cev.costing);
    ]

let () = Command.run command
