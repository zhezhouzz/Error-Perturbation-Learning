module S = Specast

module SMap = Map.Make (struct
  let compare t1 t2 =
    match (t1, t2) with
    | S.Bvar (_, b1), S.Bvar (_, b2) -> compare b1 b2
    | S.MethodPredicate (op1, args1), S.MethodPredicate (op2, args2) ->
        let c = compare op1 op2 in
        if c != 0 then c
        else
          let c = compare (List.length args1) (List.length args2) in
          if c != 0 then c
          else
            let rec aux = function
              | [] -> 0
              | ((_, arg1), (_, arg2)) :: t ->
                  let c = compare arg1 arg2 in
                  if c != 0 then c else aux t
            in
            aux @@ List.combine args1 args2
    | _ -> 0

  type t = S.t
end)

type t =
  | True
  | P of int
  | Implies of t * t
  | Ite of t * t * t
  | Not of t
  | And of t list
  | Or of t list
  | Iff of t * t

let translate (ast : S.t) =
  let m = SMap.empty in
  let update_return_idx m term =
    match SMap.find_opt term m with
    | None ->
        let idx = SMap.cardinal m in
        (SMap.add term idx m, idx)
    | Some idx -> (m, idx)
  in
  let rec aux m = function
    | S.True -> (m, True)
    | (S.Bvar (_, _) | MethodPredicate (_, _)) as term ->
        let m, idx = update_return_idx m term in
        (m, P idx)
    | S.Implies (e1, e2) ->
        let m, e1 = aux m e1 in
        let m, e2 = aux m e2 in
        (m, Implies (e1, e2))
    | S.Ite (e1, e2, e3) ->
        let m, e1 = aux m e1 in
        let m, e2 = aux m e2 in
        let m, e3 = aux m e3 in
        (m, Ite (e1, e2, e3))
    | S.Not e ->
        let m, e = aux m e in
        (m, Not e)
    | And l ->
        let m, l =
          List.fold_left
            (fun (m, l) e ->
              let m, e = aux m e in
              (m, e :: l))
            (m, []) l
        in
        (m, And l)
    | Or l ->
        let m, l =
          List.fold_left
            (fun (m, l) e ->
              let m, e = aux m e in
              (m, e :: l))
            (m, []) l
        in
        (m, Or l)
    | Iff (e1, e2) ->
        let m, e1 = aux m e1 in
        let m, e2 = aux m e2 in
        (m, Iff (e1, e2))
  in
  aux m ast

let eval_short_circuit (peval : int -> bool) e =
  let rec aux = function
    | True -> true
    | P idx -> peval idx
    | Implies (e1, e2) -> if aux e1 then aux e2 else true
    | Ite (e1, e2, e3) -> if aux e1 then aux e2 else aux e3
    | Not e -> not (aux e)
    | And l ->
        let rec and_aux = function
          | [] -> true
          | h :: t -> aux h && and_aux t
        in
        and_aux l
    | Or l ->
        let rec or_aux = function [] -> false | h :: t -> aux h || or_aux t in
        or_aux l
    | Iff (e1, e2) -> aux e1 == aux e2
  in
  aux e

open Basic_dt

let forall_eval (qvs, normal_ast) env qv_range =
  (* let open Primitive in *)
  (* let () = *)
  (*   Printf.printf "env: %s\n" *)
  (*     (List.split_by_comma (fun (k, v) -> spf "%s => %s" k @@ Value.layout v) *)
  (*     @@ List.of_seq @@ StrMap.to_seq env) *)
  (* in *)
  (* let () = *)
  (*   Printf.printf "u: %s\n" (List.split_by_comma Value.layout qv_range) *)
  (* in *)
  let qvs = List.map snd qvs in
  let qv_range_arr = Array.of_list qv_range in
  let len_qv_range_arr = Array.length qv_range_arr in
  let m, ast = translate normal_ast in
  let rec efficient_cache qvs env =
    match qvs with
    | [] ->
        let arr = Array.make (SMap.cardinal m) true in
        let () =
          SMap.iter (fun term idx -> arr.(idx) <- Prop.eval term env) m
        in
        eval_short_circuit (fun idx -> arr.(idx)) ast
    | [ u ] ->
        let arr =
          Array.make_matrix (Array.length qv_range_arr) (SMap.cardinal m) true
        in
        (* let () = *)
        (*   Printf.printf "m: %s\n" *)
        (*     (List.split_by_comma Prop.layout *)
        (*     @@ List.map fst @@ List.of_seq @@ SMap.to_seq m) *)
        (* in *)
        let () =
          Array.iteri
            (fun i u_value ->
              let env = Basic_dt.StrMap.add u u_value env in
              SMap.iter (fun term j -> arr.(i).(j) <- Prop.eval term env) m)
            qv_range_arr
        in
        (* let () = *)
        (*   Array.iter *)
        (*     (fun arr' -> *)
        (*       Printf.printf "%s\n" *)
        (*       @@ List.split_by_comma string_of_bool *)
        (*       @@ Array.to_list arr') *)
        (*     arr *)
        (* in *)
        let rec aux i =
          if i >= len_qv_range_arr then true
          else if not @@ eval_short_circuit (fun j -> arr.(i).(j)) ast then
            (* let () = *)
            (*   Printf.printf "%s:=%s rejects\n" u *)
            (*   @@ Value.layout qv_range_arr.(i) *)
            (* in *)
            false
          else aux @@ (i + 1)
        in
        aux 0
    | h :: t ->
        let rec aux i =
          if i >= len_qv_range_arr then true
          else if
            not
            @@ efficient_cache t (Basic_dt.StrMap.add h qv_range_arr.(i) env)
          then
            (* let () = *)
            (*   Printf.printf "%s:=%s rejects\n" h *)
            (*   @@ Value.layout qv_range_arr.(i) *)
            (* in *)
            false
          else aux @@ (i + 1)
        in
        aux 0
  in
  let result = efficient_cache qvs env in
  (* let () = Printf.printf "eval(%s) = %b\n" (Prop.layout normal_ast) result in *)
  (* let () = raise @@ failwith "end" in *)
  result
