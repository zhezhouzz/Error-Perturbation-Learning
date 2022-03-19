open Primitive
module V = Value
module T = Tp

let baseline qc_conf (tps : T.t list) num =
  let data =
    QCheck.Gen.generate ~n:num @@ QCheck.Gen.flatten_l
    @@ List.map (Qc.choose_gen qc_conf) tps
  in
  let data =
    List.filter_map
      (fun x ->
        List.fold_left
          (fun res x ->
            match (x, res) with
            | None, _ | _, None -> None
            | Some x, Some res -> Some (res @ [ x ]))
          (Some []) x)
      data
  in
  (num - List.length data, data)

let make_tests qc_conf (tps : T.t list) cond num =
  let data =
    QCheck.Gen.generate ~n:num @@ QCheck.Gen.flatten_l
    @@ List.map (Qc.choose_gen qc_conf) tps
  in
  let data =
    List.filter_map
      (fun x ->
        List.fold_left
          (fun res x ->
            match (x, res) with
            | None, _ | _, None -> None
            | Some x, Some res -> Some (res @ [ x ]))
          (Some []) x)
      data
  in
  List.partition cond data

let default_batch = 2000

let gen_erroneous_inputs qc_conf (tps : T.t list) cond num =
  let rec aux res =
    if List.length res >= num then Basic_dt.List.sublist res (0, num)
    else
      let data =
        QCheck.Gen.generate ~n:default_batch
        @@ QCheck.Gen.flatten_l
        @@ List.map (Qc.choose_gen qc_conf) tps
      in
      let data =
        List.filter_map
          (fun x ->
            List.fold_left
              (fun res x ->
                match (x, res) with
                | None, _ | _, None -> None
                | Some x, Some res -> Some (res @ [ x ]))
              (Some []) x)
          data
      in
      aux (res @ List.filter cond data)
  in
  aux []
