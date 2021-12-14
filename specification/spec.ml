open Primitive
open Basic_dt
module V = Value

type t = { args : Tp.tvar list; qv : Tp.tvar list; body : Specast.t }

let fvnum_to_qv = function
  | 0 -> []
  | 1 -> [ (Tp.Int, "u") ]
  | 2 -> [ (Tp.Int, "u"); (Tp.Int, "v") ]
  | _ -> raise @@ failwith "qv num is too large"

let dummy_pre tps =
  {
    args = List.combine tps @@ Tp.Naming.auto_name tps;
    qv = [];
    body = Specast.True;
  }

let mk_true tps qvnum =
  {
    args = List.combine tps @@ Tp.Naming.auto_name tps;
    qv = fvnum_to_qv qvnum;
    body = Specast.True;
  }

let apply { args; qv; body } args' =
  if List.length args != List.length args' then
    raise @@ failwith "spec apply args mismatch"
  else
    let m =
      List.fold_left (fun m (k, v) -> Tp.TvarMap.add k v m) Tp.TvarMap.empty
      @@ List.combine args args'
    in
    let update x =
      match Tp.TvarMap.find_opt x m with None -> x | Some x' -> x'
    in
    (qv, Prop.subst update body)

let get_ints env =
  let c = StrMap.fold (fun _ v c -> V.flatten_forall v @ c) env [] in
  match List.remove_duplicates c with
  | [] -> [ 0 ]
  | _ as c -> (
      match IntList.max_opt c with
      | None -> raise @@ failwith "get_ints"
      | Some m -> (m + 1) :: c)

let forallformula_eval (qv, e) env =
  let _, qv = List.split qv in
  let us = get_ints env in
  let len = List.length us in
  let ids = List.init (List.length qv) (fun _ -> 0) in
  let rec next = function
    | [] -> None
    | h :: t ->
        if len == h + 1 then
          match next t with None -> None | Some t -> Some (0 :: t)
        else Some ((h + 1) :: t)
  in
  let rec aux ids =
    let us = List.map (fun x -> List.nth us x) ids in
    let us = List.combine qv us in
    let env =
      List.fold_left
        (fun m (name, value) -> StrMap.add name (V.I value) m)
        env us
    in
    if Prop.eval e env then
      match next ids with None -> true | Some ids -> aux ids
    else false
  in
  aux ids

let forallformula_to_z3 ctx (qv, e) =
  let forallvars = List.map (Prover.Z3aux.tpedvar_to_z3 ctx) qv in
  let body = Prop.to_z3 ctx e in
  Prover.Z3aux.make_forall ctx forallvars body Prover.Z3aux.V2

let layout { args; qv; body } =
  let layout_arg (tp, name) = spf "(%s:%s)" name (Tp.layout tp) in
  Printf.sprintf "let spec %s %s = %s"
    (List.split_by " " layout_arg args)
    (List.split_by " " layout_arg qv)
    (Prop.layout body)

let eval_range { args; qv; body } argsvalue qv_range =
  let env =
    try StrMap.from_kv_list (List.combine (List.map snd args) argsvalue)
    with _ ->
      raise
      @@ failwith
           (spf "spec' args(%s) and values(%s) are mismatched in spec::exec"
              (List.split_by_comma Tp.layouttvar args)
              (List.split_by_comma Value.layout argsvalue))
  in
  Effective_eval.forall_eval (qv, body) env qv_range

let cal_range l =
  List.map (fun x -> Value.I x) @@ Value.flatten_forall_l_unique_paddled l

type eval_version = VNormal | VEfficient

let eval_version = VEfficient

let eval { args; qv; body } argsvalue =
  match eval_version with
  | VEfficient ->
      (* Zlog.log_write (spf "eval %s" @@ layout { args; qv; body }); *)
      (* Zlog.log_write (Value.layout_l_size argsvalue); *)
      eval_range { args; qv; body } argsvalue (cal_range argsvalue)
  | VNormal ->
      let env =
        try StrMap.from_kv_list (List.combine (List.map snd args) argsvalue)
        with _ ->
          raise
          @@ failwith
               (spf "spec' args(%s) and values(%s) are mismatched in spec::exec"
                  (List.split_by_comma Tp.layouttvar args)
                  (List.split_by_comma Value.layout argsvalue))
      in
      forallformula_eval (qv, body) env

let is_true { body; _ } = match body with Specast.True -> true | _ -> false

let is_false { body; _ } =
  match body with Specast.(Not True) -> true | _ -> false

let spec_body_to_z3 ctx { qv; body; _ } = forallformula_to_z3 ctx (qv, body)

(* let lemma () = *)
(*   let open Specast in *)
(*   S *)
(* TODO: add lemmas *)
let check_verified ~verified_sigma ~spec =
  match !Config.conf.z3_ctx with
  | None -> raise @@ failwith "no z3 ctx"
  | Some ctx ->
      let args = verified_sigma.args in
      let verified_f = (verified_sigma.qv, verified_sigma.body) in
      let spec_f = apply spec args in
      (* let args_z3 = List.map (Prover.Z3aux.tpedvar_to_z3 ctx) args in *)
      let verified_sigma_z3, spec_z3 =
        Sugar.map2 (forallformula_to_z3 ctx) (verified_f, spec_f)
      in
      let mk_vc a b = Z3.Boolean.mk_not ctx @@ Z3.Boolean.mk_implies ctx a b in
      let checkb a b =
        match Prover.Reflect.check ctx @@ mk_vc a b with
        | Prover.Reflect.SmtUnsat -> true
        | Prover.Reflect.SmtSat _ -> false
        | Prover.Reflect.Timeout ->
            raise @@ failwith "check_spec_implies: time out"
      in
      (checkb spec_z3 verified_sigma_z3, checkb verified_sigma_z3 spec_z3)
