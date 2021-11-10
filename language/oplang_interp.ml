open Primitive
open Basic_dt
open Oplang
module V = Value

let build_env_list tvars values =
  try List.map (fun ((_, idx), v) -> (idx, v)) @@ List.combine tvars values
  with _ ->
    raise @@ failwith "runtime error: argsment mismatch during interpret..."

let interp { fin; body; fout } (input : V.t list) =
  let env = IntMap.from_kv_list @@ build_env_list fin input in
  let step env { op; args; res } =
    if Operator.is_unused op then Some env
    else
      let imp = Operator.get_imp op in
      let argsv =
        List.map
          (fun (_, idx) -> IntMap.find "runtime error: interpret..." env idx)
          args
      in
      match imp argsv with
      | None ->
          (* Zlog.log_write (Printf.sprintf "interp error with %s(%s)" op (List.split_by_comma V.layout argsv)); *)
          None
      | Some vs -> Some (IntMap.force_update_list env @@ build_env_list res vs)
  in
  let rec loop env body =
    match body with
    | [] ->
        Some
          (List.map
             (fun (_, idx) -> IntMap.find "runtime error: interpret..." env idx)
             fout)
    | h :: t -> ( match step env h with None -> None | Some env -> loop env t)
  in
  loop env body
