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
