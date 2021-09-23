let time f =
  let t = Sys.time() in
  let fx = f () in
  let delta = (Sys.time() -. t) in
  fx, delta

type exec_flag = Debug of {logc: Core.Out_channel.t;}| Opt
type config = {exec_flag: exec_flag; if_random: bool}
let conf = ref {if_random = true; exec_flag = Opt}

let load_json fname =
  try Yojson.Basic.from_file fname with
    | _ -> raise @@ failwith (Printf.sprintf "cannot find json file(%s)" fname)

let load_config configfile =
  let open Yojson.Basic.Util in
  let exec_flag =
    try (load_json configfile) |> member "exec_flag" |> to_string with
    | _ -> raise @@ failwith "cannot load config::exec_flag" in
  let exec_flag =
    match exec_flag with
    | "debug" -> Debug {logc = Core.Out_channel.create ".log";}
    | "opt" -> Opt
    | _ -> raise @@ failwith "config error"
  in
  let if_random =
    try (load_json configfile) |> member "if_random" |> to_bool with
    | _ -> raise @@ failwith "cannot load config::if_random" in
  let _ = if if_random then Random.init 0 else () in
  conf := {exec_flag = exec_flag; if_random = if_random; }

let release_config () =
  match !conf.exec_flag with
  | Debug {logc; _} -> Core.Out_channel.close logc
  | Opt -> ()

let log_write oc message =
  Printf.fprintf oc "%s\n" message

let event eventname f =
  match !conf.exec_flag with
  | Debug {logc; _} ->
    let write x = log_write logc (Printf.sprintf "[%s] %s" eventname x)in
    let start_time = Sys.time () in
    let result = f () in
    let end_time = Sys.time () in
    let _ = write @@ Printf.sprintf "exec time:%f\n" (end_time -. start_time) in
    result
  | Opt -> f ()

let exec_main configfile main = load_config configfile; main (); release_config ()

let make_dir name =
  Core.Unix.mkdir_p name
