open Primitive

type state =
  | QCState of { tps : Tp.t list; qc_conf : Qc_config.quick_check_config }
  | PerbState of {
      init_set : Value.t list list;
      measure : Value.t list -> bool;
      prog : Language.Piecewise.t;
    }

let mk_qc_engine tps qc_conf = QCState { tps; qc_conf }

let mk_perb_engine init_set measure prog = PerbState { init_set; measure; prog }

type s = Value.t list

let sampling num = function
  | PerbState { init_set; measure; prog } ->
      let fs = (List.map snd @@ fst prog) @ [ snd prog ] in
      let init_set, num_none, data =
        Scache.eval_sampling init_set fs measure num
      in
      (PerbState { init_set; measure; prog }, num_none, data)
  | QCState { tps; qc_conf } ->
      let num_none, data = Zquickcheck.Qc_baseline.baseline qc_conf tps num in
      (QCState { tps; qc_conf }, num_none, data)

let sampling_num num state =
  let rec aux state res =
    if List.length res >= num then (state, res)
    else
      let state, _, x = sampling num state in
      aux state (res @ x)
  in
  aux state []
