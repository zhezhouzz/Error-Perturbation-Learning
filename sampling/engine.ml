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

let sampling_num filter num state =
  let batch_size =
    match state with PerbState _ -> num | QCState _ -> num * 30
  in
  let rec aux state res =
    if List.length res >= num then (state, res)
    else
      let state, _, x = sampling batch_size state in
      (* let _ = Printf.printf "len(x): %i\n" (List.length x) in *)
      let x = List.filter filter x in
      (* let x, _ = Zlog.event_time_ "filter" (fun () -> List.filter filter x) in *)
      (* let _ = Printf.printf "len(filter x): %i\n" (List.length x) in *)
      (* let _ = *)
      (*   match state with PerbState _ -> raise @@ failwith "zz" | _ -> () *)
      (* in *)
      aux state (res @ x)
  in
  aux state []

let sampling_pt_opt filter num state =
  let batch_size =
    match state with PerbState _ -> num | QCState _ -> raise @@ failwith "die"
  in
  let _, _, x = sampling batch_size state in
  (* let _ = List.iter (fun v -> Zlog.log_write @@ Value.layout_l v) x in *)
  let x = List.filter filter x in
  let x = Value_aux.remove_duplicates_l x in
  (* let _ = *)
  (*   Zlog.log_write *)
  (*   @@ Printf.sprintf "sampling_pt_opt: %i(%i)" (List.length x) batch_size *)
  (* in *)
  if List.length x < batch_size then None
  else Some (Value_aux.remove_duplicates_l x)
