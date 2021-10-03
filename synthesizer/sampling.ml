open Primitive;;
open Language;;
open Basic_dt;;

module VM = struct
  include Value_map.ValueVectorMap;;
  let find key m = try find key m with _ -> raise @@ failwith "ValueVectorMap cannot find"
end

type phase = {idx: int; data: (Value.t list) list}
type cache = {tps: Tp.t list;
              generation_hierarchy_rev: int list;
              (* iter_num: int; *)
              datam: int VM.t;
              datam_rev: (int, Value.t list) Hashtbl.t;
              (* TODO: Improve the efficiency *)
              jump_table: ((int list) array) list;
             }

let cache_init tps vs =
  let m = Hashtbl.create 1000 in
  let _ = List.iteri (fun i v -> Hashtbl.add m i v) vs in
  let datam = List.fold_lefti (fun m i v -> VM.add v i m) VM.empty vs in
  {tps = tps;
   (* iter_num = 0; *)
   generation_hierarchy_rev = [1];
   datam = datam;
   datam_rev = m;
   jump_table = []}

(* If non-det, sample for multiple times *)
let non_det_sampling_times = 3
let next_iteration f cache =
  let sampling_times = if Oplang.check_non_det f then non_det_sampling_times else 1 in
  let sampling_result =
    VM.fold (fun v idx sampling_result ->
        let r =
            List.filter_map (fun x -> x) @@
            List.init sampling_times (fun _ -> Oplang_interp.interp f v)
        in
        (idx, r) :: sampling_result
      ) cache.datam []
  in
  let total = VM.cardinal cache.datam in
  let arr = Array.make total [] in
  let counter = ref total in
  (* let idx' = !counter in *)
  (* (counter := idx' + 1); *)
  let update_m m v' =
    match VM.find_opt v' m with
    | None ->
      let idx' = !counter in
      (counter := idx' + 1);
      (Hashtbl.add cache.datam_rev idx' v');
      VM.add v' idx' m, idx'
    | Some idx' ->
      m, idx'
  in
  let m' = List.fold_left (fun m (idx, vs') ->
      let m, vidxs = List.fold_left (fun (m, vidxs) v' ->
          let m, vidx = update_m m v' in
          m, vidx :: vidxs
        ) (m, []) vs' in
      (Array.set arr idx vidxs);
      m
    ) cache.datam sampling_result in
  {tps = cache.tps;
   (* iter_num = cache.iter_num + 1; *)
   generation_hierarchy_rev = (VM.cardinal m') :: cache.generation_hierarchy_rev;
   datam = m';
   datam_rev = cache.datam_rev;
   jump_table = cache.jump_table @ [arr]}

let jump_layout datam_rev arr =
  Array.fold_lefti (fun str i j ->
      Printf.sprintf "%s; %s -> |%s|" str
        (Value.layout_l @@ Hashtbl.find datam_rev i)
        (List.split_by_comma (fun idx -> Value.layout_l @@ Hashtbl.find datam_rev idx) j)
    ) "" arr

let cache_layout cache =
  let jump_table = List.fold_lefti (fun r idx j ->
      Printf.sprintf "%s\n[iter %i]:\n%s" r idx @@ jump_layout cache.datam_rev j
    ) "" cache.jump_table
  in
  jump_table

let cost_sampling_ tps (init_set: Value.t list list) f num =
  let cache = cache_init tps init_set in
  let rec loop n cache =
    if n >= num then cache else
      loop (n + 1) @@ next_iteration f cache
  in
  loop 0 cache

let cost_sampling (env: Env.t) =
  let open Env in
  cost_sampling_ env.tps [env.i_err] env.cur_p.prog env.sampling_rounds

let test () =
  let tps, ops = Oplang.test_tps_ops in
  match Arg_solving.arg_assign tps ops with
  | None -> raise @@ failwith "never happen"
  | Some (prog, _) ->
    let _ = Printf.printf "prog:\n%s\n" (Oplang.layout prog) in
    let cache = cost_sampling_ tps [[Value.L [0]]] prog 1 in
    let _ = Printf.printf "sample cache:\n%s\n" (cache_layout cache) in
    ()
;;

