open Primitive;;
open Basic_dt;;
open Tinyocaml;;
type 'a type_check_monad =
  | TySafe of 'a
  | TyErr of string

let tc_bind (tcr: 'a type_check_monad) (f: 'a -> 'b type_check_monad) =
  match tcr with
  | TyErr msg -> TyErr msg
  | TySafe a -> f a

let tpenv_safe_combine_one (old: (Tp.t list) StrMap.t) name tp =
      match StrMap.find_opt old name with
      | None -> TySafe (StrMap.add name tp old)
      | Some tp' ->
        if Tp.tps_eq tp tp'
        then TySafe old
        else TyErr (spf "type of %s cannot be unified: %s vs. %s" name (Tp.layout_l tp) (Tp.layout_l tp'))

let tpenv_safe_diff (old: (Tp.t list) StrMap.t) (newone: (Tp.t list) StrMap.t) =
  StrMap.fold (fun name tp (result: unit type_check_monad) ->
      match StrMap.find_opt old name with
      | None -> result
      | Some tp' ->
        if Tp.tps_eq tp tp'
        then TySafe ()
        else
          let msg = spf "type of %s cannot be unified: %s vs. %s" name (Tp.layout_l tp) (Tp.layout_l tp') in
          match result with
          | TySafe _ -> TyErr msg
          | TyErr msg' -> TyErr (spf "%s\n%s" msg' msg)
    ) newone (TySafe ())

(* let type_check_vars (tenv: ((Tp.t list) StrMap.t) type_check_monad) args expected = *)
(*   let (let*\) x f = tc_bind x f in *)
(*   if List.length expected != List.length args *)
(*   then *)
(*     TyErr (spf "the number of args(%s) does not match the requirement(%s)" *)
(*              (List.split_by_comma (fun x -> x) args) *)
(*              (List.split_by_comma Tp.layout expected) *)
(*           ) *)
(*   else *)
(*     List.fold_left (fun old (argname, expected) -> *)
(*         let* old' = old in *)
(*         tpenv_safe_combine_one old' argname expected *)
(*       ) tenv @@ List.combine args expected *)

let compare_expected tps expected =
  if Tp.tps_eq expected tps then TySafe tps
  else TyErr (spf "type mismatch [%s] != [%s]"
                (List.split_by_comma Tp.layout tps)
                (List.split_by_comma Tp.layout expected)
             )

let solve_op op =
  match op with
  | "==" | "<" | ">" -> TySafe ([Tp.Int; Tp.Int;], [Tp.Bool])
  | _ -> TyErr (spf "unknown op(%s)" op)

let type_check libdef funcdef =
  let (let*) x f = tc_bind x f in
  let kv_list_from_args = List.map (fun (a, b) -> b, [a]) in
  let handle_function inpt outt args expecteds =
    let* _ = compare_expected outt expecteds in
    if List.length inpt != List.length args
    then
      TyErr (spf "the number of args(%s) does not match the function requirement(%s)"
               (List.split_by_comma Clientlang.layout_body args)
               (List.split_by_comma Tp.layout inpt)
            )
    else
      TySafe (Tuple args, inpt)
  in
  let rec aux tenv t (expecteds: T.t list) =
    (* let () = Printf.printf "Env: %s\nterm: %s; expected: %s\n\n" (layout_env tenv) (layout_body t) (List.split_by_comma Tp.layout expected) in *)
    match t with
    | Var var -> tpenv_safe_combine_one tenv var expecteds
    (* | Var var, _ -> raise @@ *)
    (*   failwith (spf "does not support tuple type(%s:%s)" var @@ List.split_by_comma Tp.layout expecteds) *)
    | Tuple vars -> aux_multi tenv vars expecteds
    | Lit lit ->
      let* _ = compare_expected (List.map V.get_tp lit) expecteds in
      TySafe tenv
    | Op (op, args) ->
      let* inpt, outt = solve_op op in
      let* t', expecteds' = handle_function inpt outt args expecteds in
      aux tenv t' expecteds'
    | App (fanme, args) when String.equal funcdef.fname fanme ->
      let inpt = List.map fst funcdef.args in
      let outt = funcdef.res in
      let* t', expecteds' = handle_function inpt outt args expecteds in
      aux tenv t' expecteds'
    | App (fname, args) ->
      let* inpt, outt = match Bblib.lib_get_tp_by_name libdef fname with
        | None -> TyErr (spf "cannot find the definition of library function %s" fname)
        | Some x -> TySafe x
      in
      let* t', expecteds' = handle_function inpt outt args expecteds in
      aux tenv t' expecteds'
    | Ift (cond, t1, t2) ->
      let* tenv = aux tenv cond [Tp.Bool] in
      let* tenv = aux tenv t1 expecteds in
      aux tenv t2 expecteds
    | Let (lfs, rhs, body) ->
      let lfstps = List.map fst lfs in
      let* tenv = aux tenv rhs lfstps in
      aux (StrMap.add_seq (List.to_seq @@ kv_list_from_args lfs) tenv) body expecteds
    | Match (terms, cases) ->
      (* match here need the type of the matched term *)
      let* terms_tp = List.fold_left (fun r name ->
          let* r = r in
          match StrMap.find_opt tenv name with
          | None -> TyErr (spf "match here need the type of the matched term %s" name)
          | Some tp -> TySafe (r @ tp)
        ) (TySafe []) terms in
      List.fold_left (fun r (cobody, body) ->
          let* r = r in
          let* tenv' = aux r cobody terms_tp in
          aux tenv' body expecteds
        ) (TySafe tenv) cases
  and aux_multi tenv ts expecteds =
    if List.length ts != List.length expecteds
    then
      TyErr (spf "the number of variables(%s) does not match the types(%s)"
               (List.split_by_comma Clientlang.layout_body ts)
               (List.split_by_comma Tp.layout expecteds)
            )
    else
      List.fold_left (fun tenv (t, expected) ->
          let* tenv = tenv in
          aux tenv t [expected]
        ) (TySafe tenv) @@ List.combine ts expecteds
  in
  let lib_members = List.map (fun (tp, name, _) -> (name, [tp])) @@ Bblib.lib_get_member libdef in
  let tenv = StrMap.from_kv_list @@ lib_members @ kv_list_from_args funcdef.args in
  let* tenv' = aux tenv funcdef.body funcdef.res in
  tpenv_safe_diff tenv tenv'
