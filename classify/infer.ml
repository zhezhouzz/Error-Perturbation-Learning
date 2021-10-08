open Primitive;;
module T = Tp;;
module V = Value;;
module F = Feature;;
module FV = Feature_vector;;
open Basic_dt;;

let spec_infer ctx (data: 'a list) (to_values: 'a -> V.t list) (judge: 'a -> bool) =
  let _ = if List.length data == 0 then raise @@ failwith "no data in precondition inference" else () in
  let pos_values, neg_values = Sugar.map2 (List.map fst) @@
    List.partition snd @@ List.map (fun d -> to_values d, judge d) data in
  Printf.printf "pos:\n%s\n" (List.split_by "\n" (fun x -> x) @@ List.map V.layout_l pos_values);
  Printf.printf "neg:\n%s\n" (List.split_by "\n" (fun x -> x) @@ List.map V.layout_l neg_values);
  let pos_data = FV.RealSample.values_to_vec ctx pos_values Label.Pos in
  let neg_data = FV.RealSample.values_to_vec ctx neg_values Label.Neg in
  (* Printf.printf "pos:\n%s\n" @@ FV.layout_vecs pos_data; *)
  (* Printf.printf "neg:\n%s\n" @@ FV.layout_vecs neg_data; *)
  (* let s = List.find_opt (fun (vec, _) -> *)
  (*     vec.(3) && vec.(6) && not vec.(7) && not vec.(9) *)
  (*   ) neg_data in *)
  (* let _ = match s with *)
  (*   | None -> Printf.printf "no such vec\n" *)
  (*   | Some (x, label) -> Printf.printf "it is:\n%s\n" @@ FV.layout_vecs [x, label] in *)
  FV.add_vecs_always ctx pos_data;
  FV.add_vecs_if_new ctx neg_data;
  let _ = Printf.printf "fvctx:\n%s\n" @@ FV.layout_fvctx ctx in
  let dt, _ = Dtree.classify ctx in
  let body = Specification.Simplify.simplify_ite @@ Dtree.to_prop dt in
  let _ = Printf.printf "spec: %s\n" (Specification.Prop.pretty_layout_prop body) in
  (* let _ = raise @@ failwith "end" in *)
  ();;
