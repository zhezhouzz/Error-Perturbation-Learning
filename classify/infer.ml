open Primitive;;
module T = Tp;;
module V = Value;;
module F = Feature;;
module FV = Feature_vector;;
open Basic_dt;;

type cex_extraction =
  | Gready
  | Minial

type rule_out_result =
  | Indistinguishable
  | DoNothing
  | Updated of int

let minimal_rule_out ht input_label fvs =
  let fvs_label = List.map (fun fv ->
      Hashtbl.find_opt ht fv
    ) fvs in
  if List.exists (function
      | Some label' -> Label.eq_label label' input_label
      | None -> false) fvs_label
  then DoNothing
  else
    let key_fvs = List.filter_map (fun (vec, label) ->
        match label with
        | None -> None
        | Some label' when Label.eq_label label' input_label -> raise @@ failwith "rule out should never happen"
        | Some _ -> Some vec
      ) @@ List.combine fvs fvs_label in
    match key_fvs with
    | [] -> Indistinguishable
    | _ ->
      let _ = raise @@ failwith (spf "len: %i" (List.length key_fvs)) in
      let fv =
        try List.nth key_fvs @@ Random.int (List.length key_fvs) with
        | _ -> raise @@ failwith "random bound error, never happen"
      in
      Hashtbl.add ht fv input_label; Updated 1

let gready_rule_out ht input_label fvs =
  let fvs = List.map (fun x -> x, input_label) fvs in
  let dup, updated = FV.add_vecs_if_new ht fvs in
  if dup == 0 && updated == 0
  then Indistinguishable
  else if updated == 0
  then DoNothing
  else Updated updated

let rule_out cex_extraction ht fvss input_label =
  let aux = match cex_extraction with
    | Gready -> gready_rule_out ht input_label
    | Minial -> minimal_rule_out ht input_label
  in
  List.fold_left (fun state fvs ->
      match state with
      | Indistinguishable -> Indistinguishable
      | DoNothing -> aux fvs
      | Updated n ->
        match aux fvs with
        | Indistinguishable -> Indistinguishable
        | DoNothing -> Updated n
        | Updated n' -> Updated (n + n')
    ) DoNothing fvss

let spec_infer ctx (data: 'a list) (to_values: 'a -> V.t list) (judge: 'a -> bool) =
  let _ = if List.length data == 0 then raise @@ failwith "no data in precondition inference" else () in
  let pos_values, neg_values = Sugar.map2 (List.map fst) @@
    List.partition snd @@ List.map (fun d -> to_values d, judge d) data in
  Printf.printf "pos:\n%s\n" (List.split_by "\n" (fun x -> x) @@ List.map V.layout_l pos_values);
  Printf.printf "neg:\n%s\n" (List.split_by "\n" (fun x -> x) @@ List.map V.layout_l neg_values);
  let pos_data = FV.RealSample.values_to_vec ctx pos_values in
  let neg_data = FV.RealSample.values_to_vec ctx neg_values in
  (* Printf.printf "pos:\n%s\n" @@ FV.layout_vecs pos_data; *)
  (* Printf.printf "neg:\n%s\n" @@ FV.layout_vecs neg_data; *)
  (* let s = List.find_opt (fun (vec, _) -> *)
  (*     vec.(3) && vec.(6) && not vec.(7) && not vec.(9) *)
  (*   ) neg_data in *)
  (* let _ = match s with *)
  (*   | None -> Printf.printf "no such vec\n" *)
  (*   | Some (x, label) -> Printf.printf "it is:\n%s\n" @@ FV.layout_vecs [x, label] in *)
  FV.add_vecs_always ctx.fvtab @@ List.map (fun x -> x, Label.Pos) @@ List.flatten pos_data;
  let () =
    match rule_out Gready ctx.fvtab neg_data Label.Neg with
    | Indistinguishable ->
      Zlog.log_write "Indistinguishable!"; ()
    | _ -> () in
  let _ = Printf.printf "fvctx:\n%s\n" @@ FV.layout_fvctx ctx in
  let dt, _ = Dtree.classify ctx in
  let body = Specification.Simplify.simplify_ite @@ Dtree.to_prop dt in
  let _ = Printf.printf "spec: %s\n" (Specification.Prop.pretty_layout_prop body) in
  (* let _ = raise @@ failwith "end" in *)
  ();;
