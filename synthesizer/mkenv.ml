module V = Primitive.Value
module T = Primitive.Tp
open Basic_dt
open Env
module Spec = Specification.Spec

let update_prog env prog =
  let open Language in
  let tps = Oplang.extract_tps prog in
  let ops = Oplang.extract_ops prog in
  (* let () = Printf.printf "tps:\n%s\nops:\n%s\n" *)
  (*     (List.split_by_comma T.layout tps) *)
  (*     (List.split_by_comma (fun x -> x) ops) in *)
  match Arg_solving.arg_assign tps ops with
  | None ->
      raise @@ failwith "Die: cannot find assignment of pertubation operators"
  | Some (_, acache) ->
      (* TODO: Can wen just reset cache here? *)
      {
        env with
        cur_p = Some { prog; acache = Arg_solving.cache_reset_prog acache };
      }

let update_init_sampling_set env init_sampling_set =
  { env with init_sampling_set }

let random_init_prog env =
  let gen =
    QCheck.Gen.flatten_l
    @@ List.init env.p_size (fun _ -> QCheck.Gen.oneofl env.op_pool)
  in
  let counter = ref 0 in
  let rec loop () =
    if !counter > 20 then raise @@ failwith "mkenv too many init"
    else
      let ops = QCheck.Gen.generate1 gen in
      let _ =
        Zlog.log_write
          (Printf.sprintf "ops: [%s]" (List.split_by_comma (fun x -> x) ops))
      in
      let _ = counter := !counter + 1 in
      match Language.Arg_solving.arg_assign env.tps ops with
      | None -> loop ()
      | Some (prog, acache) -> (prog, acache)
  in
  let prog, acache = loop () in
  { env with cur_p = Some { prog; acache } }

let mk_env_v2_ (sigma : V.t list -> bool)
    (client :
      Language.Bblib.inspector -> V.t list -> int list * V.t list option)
    (inspector : Language.Bblib.inspector) (phi : V.t list -> bool)
    (tps : T.t list) (i_err : V.t list) (op_pool : string list)
    (preds : string list) (sampling_rounds : int) (p_size : int) =
  if not (sigma i_err) then
    raise @@ failwith "mk_env: inconsistent i_err and sigma"
  else
    let info, i_err' = client inspector i_err in
    match i_err' with
    | None ->
        raise
        @@ failwith
             (spf "mk_env: inconsistent i_err(%s) and client code"
                (V.layout_l i_err))
    | Some i_err' ->
        if phi (i_err @ i_err') then
          raise
          @@ failwith
               (spf "mk_env: inconsistent i_err'(%s -> %s) and phi"
                  (V.layout_l i_err) (V.layout_l i_err'))
        else
          {
            sigma;
            client;
            library_inspector = inspector;
            phi;
            tps;
            op_pool;
            preds;
            i_err;
            i_err_non_trivial_info = info;
            init_sampling_set = [ i_err ];
            sampling_rounds;
            p_size;
            cur_p = None;
          }

let mk_env_v2 (sigma : Spec.t) (client : Language.Tinyocaml.func)
    (libraries : string list) (phi : Spec.t) (tps : T.t list) (i_err : V.t list)
    (op_pool : string list) (preds : string list) (sampling_rounds : int)
    (p_size : int) : Env.t =
  let sigma = Spec.eval sigma in
  let phi = Spec.eval phi in
  let inspector = Language.Bblib.invocation_inspector_init libraries in
  let _ =
    Language.Clientlang_tycheck.(
      match type_check inspector client with
      | TySafe _ -> ()
      | TyErr msg -> raise @@ failwith msg)
  in
  mk_env_v2_ sigma
    (Language.Clientlang.eval client)
    inspector phi tps i_err op_pool preds sampling_rounds p_size

(* let mk_env_random_init (sigma: Spec.t) (client: Language.Clientlang.func) (libraries: string list) *)
(*     (phi: Spec.t) (tps: T.t list) (i_err: V.t list) (op_pool: string list) *)
(*     (preds: string list) (sampling_rounds: int) *)
(*     (p_size: int) : Env.t = *)
(*  let env = mk_env_v2 sigma client libraries phi tps i_err *)
(*      op_pool preds sampling_rounds p_size in *)
(*  random_init_prog env *)
