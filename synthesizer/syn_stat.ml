open Language;;
module ProgRecord = Map.Make(struct type t = Oplang.t let compare = Oplang.compare_prog end);;

let prog_record = ref (ProgRecord.empty: int ProgRecord.t)

let init () =
  let open Config in
  match !conf.exec_flag with
  | Debug _ -> prog_record := ProgRecord.empty
  | _ -> ()

let add prog =
  let open Config in
  match !conf.exec_flag with
  | Debug _ ->
    (let id = ProgRecord.cardinal (!prog_record) in
     prog_record := ProgRecord.add prog id (!prog_record);
     Zlog.log_write (Printf.sprintf "\t>> mcmc current id(%i) <<" id);
     Some id)
  | _ -> None

