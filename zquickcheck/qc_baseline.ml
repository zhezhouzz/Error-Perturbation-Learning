open Primitive
module V = Value
module T = Tp

let baseline qc_conf (tps : T.t list) num =
  QCheck.Gen.generate ~n:num @@ QCheck.Gen.flatten_l
  @@ List.map (Qc.choose_gen qc_conf) tps
