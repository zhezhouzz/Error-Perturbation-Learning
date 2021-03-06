open Primitive
open Basic_dt
module F = Feature
module T = Tp
module V = Value
open Label

module RealSample = struct
  let values_to_vec_ { args; qv; fset; _ } values =
    let vars = args @ qv in
    let make_env vars value =
      List.fold_left
        (fun m ((_, name), v) -> StrMap.add name v m)
        StrMap.empty (List.combine vars value)
    in
    List.map (fun vec -> vec)
    @@ List.map
         (fun value ->
           let m = make_env vars value in
           Array.of_list @@ List.map (fun feature -> F.eval feature m) fset)
         values

  let values_to_vec ctx args_values =
    let qv_values = List.map (fun i -> V.I i) Randomgen.paddled_small_nums in
    let qvs_values = List.choose_n qv_values (List.length ctx.qv) in
    let values =
      List.map (fun a -> List.map (fun b -> a @ b) qvs_values) args_values
    in
    List.map (fun value -> values_to_vec_ ctx value) values
end

let add_vecs_always fvtab vecs =
  List.iter (fun (v, label) -> Hashtbl.add fvtab v label) vecs

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
