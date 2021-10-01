module V = Primitive.Value;;
module T = Primitive.Tp;;
open Basic_dt;;
open Env;;

let mk_env sigma client phi tps i_err op_pool sampling_rounds p_size =
  let gen = QCheck.Gen.flatten_l @@ List.init p_size (fun _ -> QCheck.Gen.oneofl op_pool) in
  let counter = ref 0 in
  let rec loop () =
    if !counter > 10 then
      raise @@ failwith "mkenv too many init(%s)"
    else
      let ops = QCheck.Gen.generate1 gen in
      let _ = Log.log_write (Printf.sprintf "ops: [%s]" (List.split_by_comma (fun x -> x) ops)) in
      let _ = counter := !counter + 1 in
      match Language.Arg_solving.arg_assign tps ops with
      | None -> loop ()
      | Some (prog, acache) -> (prog, acache)
  in
  let prog, acache = loop () in
  {sigma = sigma;
   client = client;
   phi = phi;
   tps = tps;
   op_pool = op_pool;
   i_err = i_err;
   sampling_rounds = sampling_rounds;
   cur_p = {prog = prog; acache = acache};
  }
