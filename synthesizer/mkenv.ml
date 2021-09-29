module V = Primitive.Value;;
module T = Primitive.Tp;;
open Env;;

let mk_env sigma client phi tps i_err op_pool sampling_rounds p_size =
  let gen = QCheck.Gen.flatten_l @@ List.init p_size (fun _ -> QCheck.Gen.oneofl op_pool) in
  let counter = ref 0 in
  let rec loop () =
    if !counter > 10 then raise @@ failwith "too many init" else
      let ops = QCheck.Gen.generate1 gen in
      let _ = counter := !counter + 1 in
      match Language.Arg_solving.arg_assign tps ops with
      | None -> loop ()
      | Some (_, ops, prog, acache) -> (ops, prog, acache)
  in
  let ops, prog, acache = loop () in
  {sigma = sigma;
   client = client;
   phi = phi;
   tps = tps;
   i_err = i_err;
   op_pool = op_pool;
   sampling_rounds = sampling_rounds;
   cur_p = {ops = ops; prog = prog; acache = acache};
  }
