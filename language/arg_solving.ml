open Primitive;;
open Z3;;
open Z3.Boolean
open Z3.Arithmetic;;
open Prover;;

(* let int_to_z3 ctx i = Expr.mk_numeral_int ctx i (Integer.mk_sort ctx)
 * 
 * let tpedvar_to_z3 ctx (tp, name) =
 *   let open Tp in
 *   match tp with
 *   | Int -> Integer.mk_const_s ctx name
 *   | Bool -> Boolean.mk_const_s ctx name
 *   | IntList | IntTree | IntTreeI | IntTreeB ->
 *     Integer.mk_const_s ctx name *)

open Basic_dt;;
type t = Tp.t * int * int
let spf = Printf.sprintf
let to_string (tp, place, source) = spf "h!%s!%i_%i" (Tp.layout tp) place source
(* TODO: improve cache *)
type cache = {total: t list;
              total_z3: Expr.expr list;
              v0: Expr.expr;
              v1: Expr.expr;
              query: Expr.expr;
              solutions: (int IntMap.t) list}

open Oplang;;
let one_hole_one_arg_constraint prog =
  let one env (place_tp, place_idx) =
    List.filter_map (fun (tp, idx) -> if Tp.eq tp place_tp then Some (tp, place_idx, idx) else None) env
  in
  let aux (r, env) {args; res; _} =
    let cs = List.map (fun arg -> one env arg) args in
    let env = env @ res in
    r @ cs, env
  in
  let (cs, env) = List.fold_left aux ([], prog.fin) prog.body in
  cs @ (List.map (fun arg -> one env arg) prog.fout)

open Z3aux;;
let basic_constraint cs =
  let aux m c =
    List.fold_left (fun m (tp, place, source) ->
        IntMap.update source (fun origin ->
            match origin with
            | None -> Some (tp, [place])
            | Some (_, l) -> Some (tp, place :: l)
          ) m) m c
  in
  let m = List.fold_left aux IntMap.empty cs in
  let m = IntMap.map (fun (tp, l) -> (tp, List.remove_duplicates_eq l)) m in
  let must_used_constraint =
    IntMap.fold (fun source (tp, l) cs ->
        (List.map (fun place -> (tp, place, source)) l) :: cs
      ) m [] in
  let total = List.remove_duplicates (fun (tp1, place1, source1) (tp2, place2, source2) ->
      Tp.eq tp1 tp2 && place1 == place2 && source1 == source2
    ) (List.flatten must_used_constraint) in
  total, must_used_constraint

let early_check cs =
  if List.exists (fun c -> List.length c == 0) cs then false else true

let one_hole_one_arg_to_z3 ctx cs (_, v1) =
  let aux c =
    let holes = List.map (fun v -> tpedvar_to_z3 ctx (Tp.Int, to_string v)) c in
    mk_eq ctx (mk_add ctx holes) v1
  in
  mk_and ctx (List.map aux cs)

let must_used_to_z3 ctx cs (_, v1) =
  let aux c =
    let holes = List.map (fun v -> tpedvar_to_z3 ctx (Tp.Int, to_string v)) c in
    mk_ge ctx (mk_add ctx holes) v1
  in
  mk_and ctx (List.map aux cs)

let must_boolean_to_z3 ctx total (v0, v1)=
  let vs = List.map (fun v -> tpedvar_to_z3 ctx (Tp.Int, to_string v)) total in
  let aux v =
    mk_or ctx [mk_eq ctx v v0; mk_eq ctx v v1]
  in
  vs, mk_and ctx (List.map aux vs)

let make_constraint prog =
  let one_arg = one_hole_one_arg_constraint prog in
  let total, must_used = basic_constraint one_arg in
  (total, one_arg, must_used)

let arg_reflect model total =
  List.fold_left (fun m ((_, place, source), z3encoding) ->
      let v = Reflect.get_int model z3encoding in
      if v == 1 then
        IntMap.update place (function
            | None -> Some source
            | Some _ -> raise @@ failwith "wrong SMT solver result") m
      else m
    ) IntMap.empty total

let init_cache ctx test_prog =
  let v0 = int_to_z3 ctx 0 in
  let v1 = int_to_z3 ctx 1 in
  let total, one_arg, must_used = make_constraint test_prog in
  if not (early_check one_arg) || not (early_check must_used) then None else
    let vs, total_z3 = must_boolean_to_z3 ctx total (v0, v1) in
    let query = mk_and ctx [
        total_z3;
        one_hole_one_arg_to_z3 ctx one_arg (v0, v1);
        must_used_to_z3 ctx must_used (v0, v1)] in
    Some {total = total; total_z3 = vs; v0 = v0; v1 = v1; query = query; solutions = []}


let solve_one ctx (total, vs, query) =
  let _ = Printf.printf "query:\n%s\n" (Expr.to_string query) in
  match Reflect.check ctx query with
  | Reflect.SmtSat model ->
    Some (arg_reflect model (List.combine total vs))
  | Reflect.SmtUnsat -> None
  | Reflect.Timeout -> raise @@ failwith "argument solving timeout"

let nodup_qeury ctx (total, vs, m, v0, v1) =
  mk_not ctx @@ mk_and ctx @@
  List.map (fun ((_, place, source), v) ->
      if source == IntMap.find "nodup_qeury" m place then (mk_eq ctx v v1) else (mk_eq ctx v v0)
    ) (List.combine total vs)

let solve ctx cache =
  let rec loop no_dup cache =
    match solve_one ctx (cache.total, cache.total_z3, mk_and ctx (cache.query :: no_dup)) with
    | Some m -> loop ((nodup_qeury ctx (cache.total, cache.total_z3, m, cache.v0, cache.v1))::no_dup)
                  {cache with solutions = m :: cache.solutions}
    | None -> cache
  in
  loop [] cache

let arg_assign_ ctx prog =
  Sugar.opt_fmap (solve ctx) (init_cache ctx prog)

let arg_assign tps ops =
  let init_prog = initial_naming tps ops in
  let ctx = match Config.(!conf.z3_ctx) with Some ctx -> ctx | None -> raise @@ failwith "wrong config z3" in
  Sugar.opt_bind (fun cache ->
      match cache.solutions with
      | [] -> None
      | m :: _ -> Some (tps, ops, subst m init_prog, cache)
    ) @@ arg_assign_ ctx init_prog

(* TEST *)
let test () =
  let ctx = match Config.(!conf.z3_ctx) with Some ctx -> ctx | None -> raise @@ failwith "wrong config z3" in
  let _ = Printf.printf "prog:\n%s\n" (layout test_prog) in
  let cache = arg_assign_ ctx test_prog in
  match cache with
  | None -> Printf.printf "len(solutions) = %i\n" 0
  | Some cache -> Printf.printf "len(solutions) = %i\n" (List.length cache.solutions);
    let _ = List.iter (fun m ->
        let prog' = subst m test_prog in
        Printf.printf "new prog:\n%s\n" (layout prog');
        let input = [Value.L [0]] in
        Printf.printf "execute over:\n%s\n" (List.to_string Value.layout input);
        match Oplang_interp.interp prog' input with
        | None -> Printf.printf "runtime execption\n"
        | Some values -> Printf.printf "res:\n%s\n" (List.to_string Value.layout values)
      ) cache.solutions in
    ();;


