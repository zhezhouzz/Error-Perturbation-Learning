open Primitive;;
open Basic_dt;;
module V = Value;;
type assertion_def = {pre: Specification.Spec.t; post: Specification.Spec.t;}

type t =
  | VarTuple of string list
  | LitB of bool
  | LitI of int
  | Op of string * string list
  | App of string * string list
  | Ift of t * t * t
  | Let of Tp.tvar list * t * t
  | Match of string * (string * string list * t) list

type func = {fname: string; args: Tp.tvar list; body: t; res: Tp.t list}

let layout_body =
  let rec aux = function
    | VarTuple vs -> spf "(%s)" (List.split_by_comma (fun x -> x) vs)
    | LitB b -> string_of_bool b
    | LitI i -> string_of_int i
    | Op (op, args) -> spf "%s(%s)" op (List.split_by_comma (fun x -> x) args)
    | App (op, args) -> spf "%s(%s)" op (List.split_by_comma (fun x -> x) args)
    | Ift (e1, e2, e3) -> spf "if (%s) \nthen (%s)\nelse (%s)" (aux e1) (aux e2) (aux e3)
    | Let (lfs, rhs, body) -> spf "let %s = %s in\n%s" (List.split_by_comma Tp.layouttvar lfs) (aux rhs) (aux body)
    | Match (a, cases) ->
      List.fold_left (fun str (constr, args, body) ->
          spf "%s| %s(%s) ->\n%s\n" str constr (List.split_by_comma (fun x -> x) args) (aux body)
        ) (spf "match %s with\n" a) cases
  in
  aux

let layout {fname; args; body; res} =
  spf "let rec %s %s: %s =\n%s\n" fname
    (List.split_by " " (fun x -> spf "(%s)" @@ Tp.layouttvar x) args)
    (List.split_by " * " (fun x -> spf "(%s)" @@ Tp.layout x) res)
    (layout_body body)

let type_check_vars tenv args =
  let argstp = List.filter_map (fun x -> x) @@ List.map (StrMap.find_opt tenv) args in
  if List.length argstp == List.length args then
    Some argstp
  else
    None

let type_check_op tenv op args =
  let argstp = List.filter_map (fun x -> x) @@ List.map (StrMap.find_opt tenv) args in
  if List.length argstp == List.length args
  then
    match op, argstp with
    | "==", [Tp.Int; Tp.Int;] -> Some [Tp.Bool]
    | "<", [Tp.Int; Tp.Int;] -> Some [Tp.Bool]
    | ">", [Tp.Int; Tp.Int;] -> Some [Tp.Bool]
    | _ -> raise @@ failwith (Printf.sprintf "unknown operators %s(%s) " op @@ List.split_by_comma Tp.layout argstp)
  else
    None

let lib_get_tp_by_name inspector name =
  Sugar.(
    let* stat = Hashtbl.find_opt inspector.Bblib.m name in
    Some (stat.Bblib.itps, stat.Bblib.otps)
  )

let type_check libdef funcdef =
  let layout_env env = Printf.sprintf "{%s}" @@
    List.split_by_comma (fun (name, value) -> Printf.sprintf "%s->%s" name @@ Tp.layout value) @@ StrMap.to_kv_list env
  in
  let compare_expected tps expected = if Tp.tps_eq expected tps then
      (* let () = Printf.printf "reached\n" in *)
      Some expected else None in
  let open Sugar in
  let rec aux tenv t expected =
    let () = Printf.printf "Env: %s\nterm: %s; expected: %s\n\n" (layout_env tenv) (layout_body t) (List.split_by_comma Tp.layout expected) in
    match t with
    | VarTuple vars ->
      let* tps = type_check_vars tenv vars in
      compare_expected tps expected
    | LitB _ -> compare_expected [Tp.Bool] expected
    | LitI _ -> compare_expected [Tp.Int] expected
    | Op (op, args) ->
      let* tps = type_check_op tenv op args in
      compare_expected tps expected
    | App (fanme, args) when String.equal funcdef.fname fanme ->
      let inpt = List.map fst funcdef.args in
      let outt = funcdef.res in
      let* argstp = type_check_vars tenv args in
      if List.equal Tp.eq inpt argstp then compare_expected outt expected else None
    | App (fname, args) ->
      let* inpt, outt = lib_get_tp_by_name libdef fname in
      let* argstp = type_check_vars tenv args in
      let () = Printf.printf "inpt: %s; outt: %s\n" (List.split_by_comma Tp.layout inpt)
          (List.split_by_comma Tp.layout outt) in
      let () = Printf.printf "args: %s; argstp: %s\n" (List.split_by_comma (fun x -> x) args)
          (List.split_by_comma Tp.layout argstp) in
      if List.equal Tp.eq inpt argstp then
        let () = Printf.printf "reached\n" in
        compare_expected outt expected
      else None
    | Ift (cond, t1, t2) ->
      let* condt = aux tenv cond [Tp.Bool] in
      if List.equal Tp.eq condt [Tp.Bool]
      then
        let* tps1 = aux tenv t1 expected in
        let* tps2 = aux tenv t2 expected in
        if List.equal Tp.eq tps1 tps2 then Some tps1 else None
      else
        None
    | Let (lfs, rhs, body) ->
      let lfstps = List.map fst lfs in
      let* rhstps = aux tenv rhs lfstps in
      if List.equal Tp.eq lfstps rhstps
      then aux (StrMap.add_seq (List.to_seq @@ List.map (fun (a, b) -> b, a) lfs) tenv) body expected
      else None
    | Match (term, cases) ->
      let* termstps = type_check_vars tenv [term] in
      let tps = List.map (fun (fname, args, body) ->
          let* inpt, outt = lib_get_tp_by_name libdef fname in
          let () = Printf.printf "\tinpt: %s; outt: %s\n"
              (List.split_by_comma Tp.layout inpt)
              (List.split_by_comma Tp.layout outt) in
          if List.equal Tp.eq termstps outt
          then
            aux (StrMap.add_seq (List.to_seq @@ List.combine args inpt) tenv) body expected
          else
            None
        ) cases in
      let tps' = List.filter_map (fun x -> x) tps in
      if List.length tps' == List.length cases
      then
        match tps' with
        | [] -> None
        | h :: t -> if List.for_all (fun h' -> List.equal Tp.eq h h') t then Some h else None
      else
        None
  in
  aux (StrMap.from_kv_list @@ List.map (fun (a, b) -> b, a) funcdef.args) funcdef.body funcdef.res

let eval funcdef libinsp inputs =
  let mkenv inputs =
    StrMap.from_kv_list @@ List.map (fun ((_, a), b) -> a, b) @@ List.combine funcdef.args inputs
  in
  let open Sugar in
  let eval_args (env: V.t StrMap.t) args =
    let values = List.filter_map (StrMap.find_opt env) args in
    if List.length values == List.length args
    then Some values
    else None
  in
  let layout_env env = Printf.sprintf "{%s}" @@
    List.split_by_comma (fun (name, value) -> Printf.sprintf "%s->%s" name @@ V.layout value) @@ StrMap.to_kv_list env
  in
  let rec aux env t =
    let () = Printf.printf "Env:\n%s\nTerm:\n%s\n" (layout_env env) (layout_body t) in
    match t with
    | VarTuple vars -> eval_args env vars
    | LitB b -> Some [V.B b]
    | LitI i -> Some [V.I i]
    | Op (op, args) ->
      let () = Printf.printf "Op\n" in
      let* values = eval_args env args in
      let () = Printf.printf "op values: %s\n" (V.layout_l values) in
      Some (Clientlang_op.eval op values)
    | App (fanme, args) when String.equal funcdef.fname fanme ->
      let* values = eval_args env args in
      let env' = mkenv values in
      aux env' funcdef.body
    | App (fname, args) ->
      let () = Printf.printf "App\n" in
      let* values = eval_args env args in
      let () = Printf.printf "func values: %s\n" (V.layout_l values) in
      Bblib.invocation_inspector_call libinsp fname values
    | Ift (cond, t1, t2) ->
      let* condt = aux env cond in
      let () = Printf.printf "condt: %s\n" (V.layout_l condt) in
      if List.equal V.eq condt [V.B true]
      then aux env t1
      else if List.equal V.eq condt [V.B false]
      then aux env t2
      else None
    | Let (lfs, rhs, body) ->
      let* values = aux env rhs in
      let lfsnames = List.map snd lfs in
      let env' = StrMap.add_seq (List.to_seq @@ List.combine lfsnames values) env in
      aux env' body
    | Match (term, cases) ->
      let* value = StrMap.find_opt env term in
      let rec loop cases =
        match cases with
        | [] -> raise @@ failwith "incomplete match cases"
        | (fname, args, body) :: t ->
          match Bblib.invocation_inspector_rev_call libinsp fname [value] with
          | None -> loop t
          | Some values ->
            let env' = StrMap.add_seq (List.to_seq @@ List.combine args values) env in
            aux env' body
      in
      loop cases
  in
  let _ = Bblib.invocation_inspector_clear libinsp in
  let result = aux (mkenv inputs) funcdef.body in
  let stat = Bblib.invocation_inspector_stat libinsp in
  stat, result
