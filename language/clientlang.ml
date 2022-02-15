open Primitive
open Basic_dt
module V = Value

type assertion_def = { pre : Specification.Spec.t; post : Specification.Spec.t }

open Tinyocaml

let layout_body =
  let rec aux = function
    | Var v -> v
    | Tuple vs -> spf "(%s)" (List.split_by_comma aux vs)
    | Lit lit -> V.layout_l lit
    | Op (op, args) -> spf "%s(%s)" op (List.split_by_comma aux args)
    | App (op, args) -> spf "%s(%s)" op (List.split_by_comma aux args)
    | Ift (e1, e2, e3) ->
        spf "if (%s) \nthen (%s)\nelse (%s)" (aux e1) (aux e2) (aux e3)
    | Let (lfs, rhs, body) ->
        spf "let %s = %s in\n%s"
          (List.split_by_comma Tp.layouttvar lfs)
          (aux rhs) (aux body)
    | Match (a, cases) ->
        List.fold_left
          (fun str (cobody, body) ->
            spf "%s| %s ->\n%s\n" str (aux cobody) (aux body))
          (spf "match %s with\n" @@ List.split_by_comma (fun x -> x) a)
          cases
  in
  aux

let rec to_lits = function
  | Tuple vs -> List.flatten @@ List.map to_lits vs
  | Lit lit -> lit
  | _ -> raise @@ failwith "to_lits: not a lit"

let layout_env env =
  Printf.sprintf "{%s}"
  @@ List.split_by_comma (fun (name, value) ->
         Printf.sprintf "%s->%s" name @@ V.layout_l value)
  @@ StrMap.to_kv_list env

let layout { fname; args; body; res } =
  spf "let rec %s %s: %s =\n%s\n" fname
    (List.split_by " " (fun x -> spf "(%s)" @@ Tp.layouttvar x) args)
    (List.split_by " * " (fun x -> spf "(%s)" @@ Tp.layout x) res)
    (layout_body body)

(* TODO: Currently only support single return value *)
let rev_eval libmember libinsp term values env =
  let merge ls =
    List.fold_left
      (fun l x ->
        match (x, l) with
        | None, _ -> None
        | _, None -> None
        | Some (x1, x2), Some (l1, l2) -> Some (x1 @ l1, x2 @ l2))
      (Some ([], []))
      ls
  in
  let rec aux term values =
    (* let () = Printf.printf "<Rev>term:\n%s\nvalues:\n%s\n" (layout_body term) (V.layout_l values) in *)
    match term with
    | Var var -> (
        match StrMap.find_opt libmember var with
        | Some values' ->
            if List.eq V.eq values' values then Some ([], [ var ]) else None
        | None -> Some ([ (var, values) ], []))
    | Lit lit -> if List.eq V.eq lit values then Some ([], []) else None
    | Tuple vars ->
        merge
        @@ List.map (fun (arg, value) -> aux arg [ value ])
        @@ List.combine vars values
    | App (fname, args) -> (
        match Bblib.invocation_inspector_rev_call libinsp fname values with
        | None -> None
        | Some values' ->
            if List.length values' != List.length args then
              raise
              @@ failwith "Fatal Error: type error which should be handled"
            else
              Sugar.(
                let* l, trace = aux (Tuple args) values' in
                Some (l, fname :: trace)))
    | _ ->
        raise
        @@ failwith
             (spf "Sementic Error: wrong match case: %s" @@ layout_body term)
  in
  match aux term values with
  | None -> None
  | Some (l, trace) ->
      Bblib.update_rev_stat libinsp trace;
      Some
        (List.fold_left
           (fun env (name, values) ->
             match StrMap.find_opt env name with
             | Some _ ->
                 raise
                 @@ failwith
                      "Sementic Error: match case should not rename existing \
                       variables"
             | None -> StrMap.add name values env)
           env l)

(* let lib_literl_replacement libinsp body = *)
(*   let lib_members = List.map (fun (_, name, v) -> name, v) @@ Bblib.lib_get_member libinsp in *)
(*   let env = StrMap.from_kv_list lib_members in *)
(*   let rec aux body = *)
(*     match body with *)
(*     | Var _ | Lit _ -> body *)
(*     | Tuple ts -> Tuple (List.map aux ts) *)
(*     | Op (op, args) -> Op (op, List.map aux args) *)
(*     | App (fanme, args) -> *)
(*       (match StrMap.find_opt env fanme, args with *)
(*        | Some values, [] -> Lit values *)
(*        | _, _ -> App (fanme, List.map aux args)) *)
(*     | Ift (cond, t1, t2) -> Ift (aux cond, aux t1, aux t2) *)
(*     | Let (lfs, rhs, body) -> Let (lfs, aux rhs, aux body) *)
(*     | Match (terms, cases) -> Match (terms, List.map (fun (a, b) -> aux a, aux b) cases) *)
(*   in *)
(*   aux body *)

let eval funcdef libinsp inputs =
  let () =
    if List.length inputs != List.length funcdef.args then
      raise @@ failwith
      @@ spf "eval: (%s) mismatch with (%s)"
           (List.split_by_comma T.layouttvar funcdef.args)
           (V.layout_l inputs)
    else ()
  in
  let lib_members =
    StrMap.from_kv_list
    @@ List.map (fun (_, name, v) -> (name, v))
    @@ Bblib.lib_get_member libinsp
  in
  let mkenv inputs =
    StrMap.from_kv_list
    @@ List.map (fun ((_, a), b) -> (a, [ b ]))
    @@ List.combine funcdef.args inputs
  in
  let open Sugar in
  let rec aux (env : V.t list StrMap.t) t : V.t list option =
    (* let () = Printf.printf "Env:\n%s\nTerm:\n%s\n" (layout_env env) (layout_body t) in *)
    match t with
    | Var var -> (
        match StrMap.find_opt env var with
        | None -> (
            match StrMap.find_opt lib_members var with
            | Some values' -> Some values'
            | None ->
                Zlog.log_write (spf "client eval: cannot find %s" var);
                None)
        | Some value -> Some value)
    | Tuple ts -> aux_multi env ts
    | Lit lit -> Some lit
    | Op (op, args) ->
        (* let () = Printf.printf "Op\n" in *)
        let* values = aux_multi env args in
        (* let () = Printf.printf "op values: %s\n" (V.layout_l values) in *)
        Some (Clientlang_op.eval op values)
    | App ("raise", _) -> None
    | App (fanme, args) when String.equal funcdef.fname fanme ->
        let* values = aux_multi env args in
        let env' = mkenv values in
        aux env' funcdef.body
    | App (fname, args) ->
        (* let () = Printf.printf "App\n" in *)
        let* values = aux_multi env args in
        (* let () = Printf.printf "func values: %s\n" (V.layout_l values) in *)
        Bblib.invocation_inspector_call libinsp fname values
    | Ift (cond, t1, t2) ->
        let* condt = aux env cond in
        (* let () = Printf.printf "condt: %s\n" (V.layout_l condt) in *)
        if List.equal V.eq condt [ V.B true ] then aux env t1
        else if List.equal V.eq condt [ V.B false ] then aux env t2
        else None
    | Let (lfs, rhs, body) ->
        let* values = aux env rhs in
        let lfsnames = List.map snd lfs in
        let env' =
          StrMap.add_seq
            (List.to_seq
            @@ List.map (fun (a, b) -> (a, [ b ]))
            @@ List.combine lfsnames values)
            env
        in
        aux env' body
    | Match (terms, cases) ->
        let* values = aux_multi env @@ List.map (fun name -> Var name) terms in
        let rec loop cases =
          match cases with
          | [] -> raise @@ failwith "incomplete match cases"
          | (case, body) :: t -> (
              match rev_eval lib_members libinsp case values env with
              | Some env' -> aux env' body
              | None -> loop t)
        in
        loop cases
  and aux_multi env ts : V.t list option =
    (* let () = Printf.printf "Env:\n%s\n[Terms]:\n%s\n" (layout_env env) (List.split_by "\n" layout_body ts) in *)
    let values = List.map (aux env) ts in
    (* let () = Printf.printf "[Terms]:\n%s\n->\n%s\n" (List.split_by "\n" layout_body ts) *)
    (*     (List.split_by_comma (function *)
    (*          | Some vs -> V.layout_l vs *)
    (*          | None -> "none") values) in *)
    let* values = Sugar.opt_list_to_list_opt values in
    Some (List.flatten values)
  in
  let _ = Bblib.invocation_inspector_clear libinsp in
  let result = aux (mkenv inputs) funcdef.body in
  let stat = Bblib.invocation_inspector_stat libinsp in
  (stat, result)
