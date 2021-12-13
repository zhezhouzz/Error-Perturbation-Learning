open Primitive
open Basic_dt
module F = Feature
module T = Tp
module V = Value
open Cctx

let values_to_vec_ { args; qv; fset; _ } values =
  let vars = args @ qv in
  let make_env vars value =
    List.fold_left
      (fun m ((_, name), v) -> StrMap.add name v m)
      StrMap.empty (List.combine vars value)
  in
  List.map
    (fun value ->
      let m = make_env vars value in
      Array.of_list @@ List.map (fun feature -> F.eval feature m) fset)
    values

let values_mk_qv_space ctx args_values =
  let qv_elem_values = List.map (fun i -> V.I i) Randomgen.paddled_small_nums in
  let qv_values =
    (List.map (fun i -> V.I i)
    @@ List.remove_duplicates (List.map V.len @@ List.flatten args_values))
    @ qv_elem_values
  in
  let qvs_values = List.choose_n qv_values (List.length ctx.qv) in
  qvs_values

let value_to_vec ctx qvs_values args_value =
  let values = List.map (fun b -> args_value @ b) qvs_values in
  values_to_vec_ ctx values

let add_vecs_always fvtab vecs =
  List.iter (fun (v, label) -> Hashtbl.replace fvtab v label) vecs

let add_vecs_if_new fvtab vecs =
  List.fold_left
    (fun (dup, updated) (v, label) ->
      match Hashtbl.find_opt fvtab v with
      | Some label' ->
          if Label.eq_label label' label then (dup + 1, updated)
          else (dup, updated)
      | None ->
          Hashtbl.add fvtab v label;
          (dup, updated + 1))
    (0, 0) vecs

type cex_extraction = Gready | Minial

type rule_out_result = Indistinguishable | DoNothing | Updated of int

let minimal_rule_out ht input_label fvs =
  let fvs_label = List.map (fun fv -> Hashtbl.find_opt ht fv) fvs in
  if
    List.exists
      (function
        | Some label' -> Label.eq_label label' input_label | None -> false)
      fvs_label
  then DoNothing
  else
    let key_fvs =
      List.filter_map (fun (vec, label) ->
          match label with
          | None -> None
          | Some label' when Label.eq_label label' input_label ->
              raise @@ failwith "rule out should never happen"
          | Some _ -> Some vec)
      @@ List.combine fvs fvs_label
    in
    match key_fvs with
    | [] -> Indistinguishable
    | _ ->
        let _ = raise @@ failwith (spf "len: %i" (List.length key_fvs)) in
        let fv =
          try List.nth key_fvs @@ Random.int (List.length key_fvs)
          with _ -> raise @@ failwith "random bound error, never happen"
        in
        Hashtbl.add ht fv input_label;
        Updated 1

let gready_rule_out ht input_label fvs =
  let fvs = List.map (fun x -> (x, input_label)) fvs in
  let dup, updated = add_vecs_if_new ht fvs in
  if dup == 0 && updated == 0 then Indistinguishable
  else if updated == 0 then DoNothing
  else Updated updated

let rule_out cex_extraction ht fvss input_label =
  match cex_extraction with
  | Gready -> gready_rule_out ht input_label fvss
  | Minial -> minimal_rule_out ht input_label fvss

let pos_gather cctx args_values =
  let qv_sapce = values_mk_qv_space cctx args_values in
  List.iter
    (fun args_value ->
      add_vecs_always cctx.fvtab
      @@ List.map (fun x -> (x, Label.Pos))
      @@ value_to_vec cctx qv_sapce args_value)
    args_values

let neg_gather cctx args_values =
  let qv_sapce = values_mk_qv_space cctx args_values in
  let aux data fvs =
    let x = rule_out Gready cctx.fvtab fvs Label.Neg in
    match x with
    | Indistinguishable ->
        Zlog.log_write @@ spf "rule out fail: %s" (V.layout_l data);
        Zlog.log_write @@ spf "%s" (Feature.layout_set cctx.fset);
        Zlog.log_write
        @@ spf "rule out fail: %s"
             (layout_vecs (List.map (fun k -> (k, Label.Neg)) fvs));
        x
    | _ -> x
  in
  let state =
    List.fold_left
      (fun state args_value ->
        let fvs = value_to_vec cctx qv_sapce args_value in
        match state with
        | Indistinguishable -> Indistinguishable
        | DoNothing -> aux args_value fvs
        | Updated n -> (
            match aux args_value fvs with
            | Indistinguishable -> Indistinguishable
            | DoNothing -> Updated n
            | Updated n' -> Updated (n + n')))
      DoNothing args_values
  in
  match state with
  | Indistinguishable ->
      Zlog.log_write "Indistinguishable!";
      raise @@ failwith "Indistinguishable!"
  | DoNothing ->
      Zlog.log_write "DoNothing!";
      raise @@ failwith "DoNothing!"
  | Updated n ->
      Zlog.log_write (spf "Updated %i!" n);
      ()
