open Primitive
open Basic_dt
module V = Value

type t = { args : Tp.tvar list; qv : Tp.tvar list; body : Specast.t }

let dummy_pre tps =
  {
    args = List.combine tps @@ Tp.Naming.auto_name tps;
    qv = [];
    body = Specast.True;
  }

let get_ints env =
  let c =
    StrMap.fold
      (fun _ v c ->
        match v with
        | V.U -> []
        | V.I i -> i :: c
        | V.B _ -> c
        | V.L il -> il @ c
        | V.T tr -> Tree.flatten tr @ c
        | V.TI tr -> LabeledTree.flatten tr @ c
        | V.TB tr -> LabeledTree.flatten tr @ c
        | V.NotADt -> c)
      env []
  in
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
      Zlog.log_write (spf "eval %s" @@ layout { args; qv; body });
      Zlog.log_write (Value.layout_l_size argsvalue);
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
