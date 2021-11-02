open Primitive
module V = Value
module T = Tp

let baseline qc_conf (tps : T.t list) num = Qc.small_gen ~qc_conf ~num ~tps
