type exec_flag = Debug of { logc : Core.Out_channel.t } | Opt

type mutation_distribution = {
  op_replace : int;
  op_swap : int;
  op_deny : int;
  arg_reassign : int;
}

type bias_method = SamplingCutOff | CostPenalty | MeasureOnly | Correct

type config = {
  exec_flag : exec_flag;
  if_random : bool;
  z3_ctx : Z3.context option;
  mutation_distribution : mutation_distribution;
  bias_method : bias_method;
  arg_solve_bound : int;
}

let conf =
  ref
    {
      if_random = true;
      exec_flag = Opt;
      z3_ctx = None;
      mutation_distribution =
        { op_replace = 1; op_swap = 1; op_deny = 1; arg_reassign = 1 };
      bias_method = CostPenalty;
      arg_solve_bound = 35;
    }

let logdir = ".logdir"

let logfile = ".log"

let make_dir name = Core.Unix.mkdir_p name

let load_json fname =
  try Yojson.Basic.from_file fname
  with _ ->
    raise @@ failwith (Printf.sprintf "cannot find json file(%s)" fname)

let load_config configfile =
  let open Yojson.Basic.Util in
  let exec_flag =
    try load_json configfile |> member "exec_flag" |> to_string
    with _ -> raise @@ failwith "cannot load config::exec_flag"
  in
  let exec_flag =
    match exec_flag with
    | "debug" ->
        make_dir logdir;
        Debug { logc = Core.Out_channel.create (logdir ^ "/" ^ logfile) }
    | "opt" -> Opt
    | _ -> raise @@ failwith "config error"
  in
  let j = load_json configfile in
  let if_random =
    try j |> member "if_random" |> to_bool
    with _ -> raise @@ failwith "cannot load config::if_random"
  in
  let if_z3 =
    try j |> member "if_z3" |> to_bool
    with _ -> raise @@ failwith "cannot load config::z3"
  in
  let _ = if if_random then Random.init 0 else () in
  let z3_ctx =
    if if_z3 then
      Some
        (Z3.mk_context
           [ ("model", "true"); ("proof", "false"); ("timeout", "1999") ])
    else None
  in
  let mutation_distribution =
    try
      let md = j |> member "mutation_distribution" in
      {
        op_replace = md |> member "op_replace" |> to_int;
        op_swap = md |> member "op_swap" |> to_int;
        op_deny = md |> member "op_deny" |> to_int;
        arg_reassign = md |> member "arg_reassign" |> to_int;
      }
    with _ -> raise @@ failwith "cannot load config::mutation_distribution"
  in
  let bias_method =
    match j |> member "bias_method" |> to_string with
    | "SamplingCutOff" -> SamplingCutOff
    | "CostPenalty" -> CostPenalty
    | _ -> raise @@ failwith "cannot load config::bias_method"
  in
  let arg_solve_bound =
    try j |> member "arg_solve_bound" |> to_int
    with _ -> raise @@ failwith "cannot load config::arg_solve_bound"
  in
  conf :=
    {
      exec_flag;
      if_random;
      z3_ctx;
      mutation_distribution;
      bias_method;
      arg_solve_bound;
    }

let release_config () =
  match !conf.exec_flag with
  | Debug { logc; _ } -> Core.Out_channel.close logc
  | Opt -> ()

let refresh_logfile name =
  let new_name = logfile ^ "_" ^ name in
  let flag' =
    match !conf.exec_flag with
    | Debug { logc; _ } ->
        Core.Out_channel.close logc;
        Core.Unix.rename
          ~src:(logdir ^ "/" ^ logfile)
          ~dst:(logdir ^ "/" ^ new_name);
        Debug { logc = Core.Out_channel.create (logdir ^ "/" ^ logfile) }
    | Opt -> Opt
  in
  conf := { !conf with exec_flag = flag' }

let exec_main configfile main =
  load_config configfile;
  main ();
  release_config ()
