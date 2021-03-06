open Primitive

type state =
  | QCState of { tps : Tp.t list; qc_conf : Qc_config.quick_check_config }
  | PerbState of {
      init_set : Value.t list list;
      measure : Value.t list -> bool;
      prog : Language.Piecewise.t;
      if_coverage : bool;
    }

let mk_qc_engine tps qc_conf = QCState { tps; qc_conf }

let mk_perb_engine ?(if_coverage = false) init_set measure prog =
  PerbState { init_set; measure; prog; if_coverage }

type s = Value.t list

let sampling num = function
  | PerbState { if_coverage; init_set; measure; prog } ->
      let fs = (List.map snd @@ fst prog) @ [ snd prog ] in
      (* let fs = [ snd prog ] in *)
      if if_coverage then
        let rec loop res (s, f) =
          if List.length res >= num then res
          else
            match Language.Oplang_interp.interp f s with
            | None -> res
            | Some x -> loop (x :: res) (x, f)
        in
        let ns, data =
          List.split
          @@ List.map
               (fun f ->
                 let d = loop init_set (List.nth init_set 0, f) in
                 let num_none =
                   num - List.length (Value_aux.remove_duplicates_l d)
                 in
                 (num_none, d))
               fs
        in
        let num_none = List.fold_left (fun res x -> res + x) 0 ns in
        ( PerbState { if_coverage; init_set; measure; prog },
          num_none,
          List.flatten data )
      else
        let init_set, num_none, data =
          Scache.eval_sampling init_set fs measure num
        in
        (PerbState { if_coverage; init_set; measure; prog }, num_none, data)
  | QCState { tps; qc_conf } ->
      let num_none, data = Zquickcheck.Qc_baseline.baseline qc_conf tps num in
      (QCState { tps; qc_conf }, num_none, data)

let sampling_num filter num state =
  let batch_size =
    match state with PerbState _ -> num | QCState _ -> num * 100
  in
  let rec aux state res =
    if List.length res >= num then (state, res)
    else
      let state, _, x = sampling batch_size state in
      (* let _ = *)
      (*   Printf.printf "len(x): %i; batch_size: %i\n" (List.length x) batch_size *)
      (* in *)
      let x = List.filter filter x in
      (* let x, _ = Zlog.event_time_ "filter" (fun () -> List.filter filter x) in *)
      (* let _ = Printf.printf "len(filter x): %i\n" (List.length x) in *)
      (* let _ = if List.length x > 0 then raise @@ failwith "zz" in *)
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
  if List.length x < 1 then None else Some x

let sampling_pt_opt_moti filter num state =
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
  if List.length x < num - 1 then None else Some x
