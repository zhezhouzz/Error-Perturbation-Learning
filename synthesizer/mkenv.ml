module V = Primitive.Value;;
module T = Primitive.Tp;;
open Basic_dt;;
open Env;;

(* let mk_env sigma client library phi tps i_err op_pool sampling_rounds p_size = *)
(*   let gen = QCheck.Gen.flatten_l @@ List.init p_size (fun _ -> QCheck.Gen.oneofl op_pool) in *)
(*   let counter = ref 0 in *)
(*   let rec loop () = *)
(*     if !counter > 20 then *)
(*       raise @@ failwith "mkenv too many init" *)
(*     else *)
(*       let ops = QCheck.Gen.generate1 gen in *)
(*       let _ = Zlog.log_write (Printf.sprintf "ops: [%s]" (List.split_by_comma (fun x -> x) ops)) in *)
(*       let _ = counter := !counter + 1 in *)
(*       match Language.Arg_solving.arg_assign tps ops with *)
(*       | None -> loop () *)
(*       | Some (prog, acache) -> (prog, acache) *)
(*   in *)
(*   let prog, acache = loop () in *)
(*   let inspector = Primitive.Invocation.invocation_inspector_init library in *)
(*   if not (sigma i_err) then raise @@ failwith "mk_env: inconsistent i_err and sigma" *)
(*   else *)
(*     let info, i_err' = client inspector i_err in *)
(*     match i_err' with *)
(*     | None -> raise @@ failwith "mk_env: inconsistent i_err and client code" *)
(*     | Some i_err' -> *)
(*       if phi i_err' then raise @@ failwith "mk_env: inconsistent i_err and phi" else *)
(*         {sigma = sigma; *)
(*          client = client; *)
(*          library_inspector = inspector; *)
(*          phi = phi; *)
(*          tps = tps; *)
(*          op_pool = op_pool; *)
(*          i_err = i_err; *)
(*          i_err_non_trivial_info = info; *)
(*          sampling_rounds = sampling_rounds; *)
(*          cur_p = {prog = prog; acache = acache}; *)
(*         } *)

module Spec = Specification.Spec
let mk_env_v2_ (sigma: V.t list -> bool)
    (client: Language.Bblib.inspector -> V.t list -> int list * V.t list option) (inspector: Language.Bblib.inspector)
    (phi: V.t list -> bool) (tps: T.t list) (i_err: V.t list) (op_pool: string list) (sampling_rounds: int)
    (p_size: int) =
  let gen = QCheck.Gen.flatten_l @@ List.init p_size (fun _ -> QCheck.Gen.oneofl op_pool) in
  let counter = ref 0 in
  let rec loop () =
    if !counter > 20 then
      raise @@ failwith "mkenv too many init"
    else
      let ops = QCheck.Gen.generate1 gen in
      let _ = Zlog.log_write (Printf.sprintf "ops: [%s]" (List.split_by_comma (fun x -> x) ops)) in
      let _ = counter := !counter + 1 in
      match Language.Arg_solving.arg_assign tps ops with
      | None -> loop ()
      | Some (prog, acache) -> (prog, acache)
  in
  let prog, acache = loop () in
  if not (sigma i_err) then raise @@ failwith "mk_env: inconsistent i_err and sigma"
  else
    let info, i_err' = client inspector i_err in
    match i_err' with
    | None -> raise @@ failwith "mk_env: inconsistent i_err and client code"
    | Some i_err' ->
      if phi i_err' then raise @@ failwith "mk_env: inconsistent i_err and phi" else
        {sigma = sigma;
         client = client;
         library_inspector = inspector;
         phi = phi;
         tps = tps;
         op_pool = op_pool;
         i_err = i_err;
         i_err_non_trivial_info = info;
         sampling_rounds = sampling_rounds;
         cur_p = {prog = prog; acache = acache};
        }

let mk_env_v2 (sigma: Spec.t) (client: Language.Clientlang.func) (libraries: string list)
    (phi: Spec.t) (tps: T.t list) (i_err: V.t list) (op_pool: string list) (sampling_rounds: int)
    (p_size: int) : Env.t =
  let sigma = Spec.eval sigma in
  let phi = Spec.eval phi in
  let inspector = Language.Bblib.invocation_inspector_init libraries in
  let _ = match Language.Clientlang.type_check inspector client with
    | Some _ -> ()
    | _ -> raise @@ failwith "Type error!"
  in
  mk_env_v2_ sigma (Language.Clientlang.eval client) inspector phi tps i_err op_pool sampling_rounds p_size
