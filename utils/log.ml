let time f =
  let t = Sys.time() in
  let fx = f () in
  let delta = (Sys.time() -. t) in
  fx, delta

let log_write message =
  let open Config in
  match !conf.exec_flag with
  | Debug {logc; _} -> Printf.fprintf logc "%s\n" message
  | _ -> ()

let event eventname f =
  let open Config in
  match !conf.exec_flag with
  | Debug _ ->
    let write x = log_write (Printf.sprintf "[%s] %s" eventname x)in
    let start_time = Sys.time () in
    let result = f () in
    let end_time = Sys.time () in
    let _ = write @@ Printf.sprintf "exec time:%f\n" (end_time -. start_time) in
    result
  | Opt -> f ()

let make_dir name =
  Core.Unix.mkdir_p name
