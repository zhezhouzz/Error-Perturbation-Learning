open Primitive
open Language
open Basic_dt

module VM = struct
  include Value_map.ValueVectorMap

  let find key m =
    try find key m with _ -> raise @@ failwith "ValueVectorMap cannot find"
end

type phase = { idx : int; data : Value.t list list }

type cache = {
  tps : Tp.t list;
  generation_hierarchy_rev : int list;
  (* iter_num: int; *)
  datam : int VM.t;
  datam_rev : (int, Value.t list) Hashtbl.t;
  (* TODO: Improve the efficiency *)
  jump_table : int list array list;
}

let to_data { datam_rev; jump_table; _ } =
  List.fold_left
    (fun res arr ->
       List.fold_left
         (fun res outs -> res @ List.map (Hashtbl.find datam_rev) outs)
         res
       @@ Array.to_list arr)
    [] jump_table

let pure_sampled_input_output { datam_rev; jump_table; _ } =
  match jump_table with
  | [] -> raise @@ failwith (spf "sampling empty in %s" __FUNCTION__)
  | _ ->
    let j = List.last jump_table in
    let tmp_table = Hashtbl.create (Array.length j) in
    let _ =
      Array.iteri
        (fun in_idx out_idxs ->
           List.iter
             (fun out_idx -> Hashtbl.add tmp_table (in_idx, out_idx) ())
             out_idxs)
        j
    in
    (datam_rev, List.of_seq @@ Hashtbl.to_seq_keys @@ tmp_table)

let cache_init tps vs =
  let m = Hashtbl.create 1000 in
  let _ = List.iteri (fun i v -> Hashtbl.add m i v) vs in
  let datam = List.fold_lefti (fun m i v -> VM.add v i m) VM.empty vs in
  {
    tps;
    (* iter_num = 0; *)
    generation_hierarchy_rev = [ 1 ];
    datam;
    datam_rev = m;
    jump_table = [];
  }

(* If non-det, sample for multiple times *)
let non_det_sampling_times = 3

let bais_rate = 0.5

let biased_sample bias f sampling_times datam =
  VM.fold
    (fun v idx sampling_result ->
       if bias v || Random.float 1.0 > bais_rate then
         let r =
           List.filter_map (fun x -> x)
           @@ List.init sampling_times (fun _ -> Oplang_interp.interp f v)
         in
         (idx, r) :: sampling_result
       else (idx, []) :: sampling_result)
    datam []

let unbiased_sample f sampling_times datam =
  VM.fold
    (fun v idx sampling_result ->
       let r =
         List.filter_map (fun x -> x)
         @@ List.init sampling_times (fun _ -> Oplang_interp.interp f v)
       in
       (idx, r) :: sampling_result)
    datam []

let biased_next_iteration bias f cache =
  let sampling_times =
    if Oplang.check_non_det f then non_det_sampling_times else 1
  in
  let sampling_result =
    match !Config.conf.bias_method with
    | Config.SamplingCutOff -> biased_sample bias f sampling_times cache.datam
    | Config.CostPenalty -> unbiased_sample f sampling_times cache.datam
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
      counter := idx' + 1;
      Hashtbl.add cache.datam_rev idx' v';
      (VM.add v' idx' m, idx')
    | Some idx' -> (m, idx')
  in
  let m' =
    List.fold_left
      (fun m (idx, vs') ->
         let m, vidxs =
           List.fold_left
             (fun (m, vidxs) v' ->
                let m, vidx = update_m m v' in
                (m, vidx :: vidxs))
             (m, []) vs'
         in
         Array.set arr idx vidxs;
         m)
      cache.datam sampling_result
  in
  {
    tps = cache.tps;
    (* iter_num = cache.iter_num + 1; *)
    generation_hierarchy_rev = VM.cardinal m' :: cache.generation_hierarchy_rev;
    datam = m';
    datam_rev = cache.datam_rev;
    jump_table = cache.jump_table @ [ arr ];
  }

let jump_layout datam_rev arr =
  Array.fold_lefti
    (fun str i j ->
       Printf.sprintf "%s; %s -> |%s|" str
         (Value.layout_l @@ Hashtbl.find datam_rev i)
         (List.split_by_comma
            (fun idx -> Value.layout_l @@ Hashtbl.find datam_rev idx)
            j))
    "" arr

let cache_layout cache =
  let jump_table =
    List.fold_lefti
      (fun r idx j ->
         Printf.sprintf "%s\n[iter %i]:\n%s" r idx
         @@ jump_layout cache.datam_rev j)
      "" cache.jump_table
  in
  jump_table

let cost_sampling_ tps (init_set : Value.t list list) f num =
  let cache = cache_init tps init_set in
  let rec loop n cache =
    if n >= num then cache
    else loop (n + 1) @@ biased_next_iteration (fun _ -> true) f cache
  in
  loop 0 cache

(* let cost_sampling (env: Env.t) = *)
(*   let open Env in *)
(*   match env.cur_p with *)
(*   | None -> raise @@ failwith "the env has not prog initialized" *)
(*   | Some cur_p -> *)
(*     cost_sampling_ env.tps [env.i_err] cur_p.prog env.sampling_rounds *)

(* TODO: Effiency *)
let biased_cost_sampling (bias : Value.t list -> bool) tps
    (init_set : Value.t list list) f num =
  let cache = cache_init tps init_set in
  let rec loop n cache =
    if n >= num then cache
    else loop (n + 1) @@ biased_next_iteration bias f cache
  in
  loop 0 cache

let sampling_to_data (init_set : Value.t list list) f num =
  let max_pool = num / 8 in
  let res = ref [] in
  let rec aux pool =
    let samples =
      List.flatten
      @@ List.map
        (fun x -> Piecewise.eval_sampling f x non_det_sampling_times)
        pool
    in
    res := !res @ samples;
    if List.length !res >= num then List.sublist !res (0, num)
    else
      let pool' =
        let pool' = List.filter_map (fun x -> x) samples in
        if List.length pool' == 0 then pool
        else if List.length pool' > max_pool then
          Array.to_list
          @@ QCheck.Gen.generate1
            (QCheck.Gen.array_subset max_pool (Array.of_list pool'))
        else pool'
      in
      aux pool'
  in
  aux init_set

let test () =
  let tps, ops = Oplang.test_tps_ops in
  match Arg_solving.arg_assign tps ops with
  | None -> raise @@ failwith "never happen"
  | Some (prog, _) ->
    let _ = Printf.printf "prog:\n%s\n" (Oplang.layout prog) in
    let cache = cost_sampling_ tps [ [ Value.L [ 0 ] ] ] prog 1 in
    let _ = Printf.printf "sample cache:\n%s\n" (cache_layout cache) in
    ()
