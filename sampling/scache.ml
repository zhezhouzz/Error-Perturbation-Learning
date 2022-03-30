open Primitive
open Language
open Basic_dt

module VM = struct
  include Value_aux.ValueVectorMap

  let find key m =
    try find key m with _ -> raise @@ failwith "ValueVectorMap cannot find"
end

(* exception *)
module Mem = struct
  type t = {
    datam : int VM.t;
    datam_rev : (int, Value.t list) Hashtbl.t;
    jump_table : (int, int list) Hashtbl.t;
  }

  let i_exists { datam_rev; _ } = Hashtbl.mem datam_rev

  let itov { datam_rev; _ } = Hashtbl.find datam_rev

  let vtoi { datam; _ } value = VM.find value datam

  let get_out_idxs mem inp_idx =
    (* Zlog.log_write @@ spf "get_out_idxs: inp_idx %i" inp_idx; *)
    Hashtbl.find mem.jump_table inp_idx

  let get_outs mem inp_idx =
    List.map (itov mem) (Hashtbl.find mem.jump_table inp_idx)

  let all_outs mem =
    List.flatten
    @@ List.init (Hashtbl.length mem.jump_table) (fun idx -> get_outs mem idx)

  let all_outs_unique mem =
    List.map (itov mem)
    @@ List.remove_duplicates
    @@ Hashtbl.fold (fun _ outs res -> outs @ res) mem.jump_table []

  (* List.init (Hashtbl.length mem.jump_table) (fun idx -> *)
  (*    let is = get_out_idxs mem idx in *)
  (*    Zlog.log_write @@ spf "all_outs_unique: get from %i" idx; *)
  (*    Zlog.log_write @@ spf "all_outs: %s" *)
  (*    @@ List.split_by_comma string_of_int is; *)
  (*    is) *)

  let all_valid_pairs mem =
    List.flatten
    @@ List.init (Hashtbl.length mem.jump_table) (fun idx ->
           List.map (fun outp -> (itov mem idx, outp)) @@ get_outs mem idx)

  let all mem = List.of_seq @@ VM.to_seq mem.datam

  type mopt = NewAdded | Old

  let add mem value =
    match VM.find_opt value mem.datam with
    | Some idx -> (mem, idx, Old)
    | None ->
        let idx = Hashtbl.length mem.datam_rev in
        let datam' = VM.add value idx mem.datam in
        Hashtbl.add mem.datam_rev idx value;
        Zlog.log_write @@ spf "datem_rev: add %i" idx;
        ({ mem with datam = datam' }, idx, NewAdded)

  let adds mem values =
    List.fold_left
      (fun (mem, n, o) v ->
        let mem, idx, opt = add mem v in
        match opt with
        | NewAdded -> (mem, idx :: n, o)
        | Old -> (mem, n, idx :: o))
      (mem, [], []) values

  let init () =
    {
      datam = VM.empty;
      datam_rev = Hashtbl.create 2000;
      jump_table = Hashtbl.create 2000;
    }

  let reg mem inp outs = Hashtbl.add mem.jump_table inp outs

  let filter mem f idxs =
    List.filter
      (fun idx ->
        let v = itov mem idx in
        let b = f v in
        if not b then
          (* Zlog.log_write @@ spf "filtered out %s" @@ Value.layout_l v; *)
          b
        else b)
      idxs

  let jump_layout mem idxs =
    List.fold_left
      (fun str i ->
        spf "%s; %s -> |%s|" str
          (Value.layout_l @@ itov mem i)
          (List.split_by_comma Value.layout_l @@ get_outs mem i))
      "" idxs
end

type generations = int list

let duplicate prev i = List.map (List.filter (fun j -> i == j)) prev

type t = { mem : Mem.t; gs : generations list }

type conds = {
  measure : Value.t list -> bool;
  sigma : Value.t list -> bool;
  client : Value.t list -> Value.t list option;
  phi : Value.t list -> bool;
  pre : Value.t list -> bool;
}

let mk_conds measure sigma client phi pre = { measure; sigma; client; phi; pre }

let init init_set =
  let mem, n, o = Mem.adds (Mem.init ()) init_set in
  { mem; gs = [ n @ o ] }

let flatten_raw { gs; _ } =
  match gs with [] -> [] | _ :: t -> List.remove_duplicates @@ List.flatten t

let layout { mem; gs } =
  let rec aux idx prev = function
    | [] -> prev
    | inps :: t ->
        aux (idx + 1)
          (spf "%s\n[iter %i]:\n%s" prev idx @@ Mem.jump_layout mem inps)
          t
  in
  aux 0 "" (List.rev gs)

let layout_raw { gs; _ } =
  let rec aux idx prev = function
    | [] -> prev
    | inps :: t ->
        aux (idx + 1)
          (spf "%s\n[iter %i]:\n%s" prev idx @@ IntList.to_string inps)
          t
  in
  aux 0 "" (List.rev gs)

let non_det_sampling_times = 3

(* TODO add non-det *)
let smart_eval mem f inp_idx =
  let n, outs = f @@ Mem.itov mem inp_idx in
  (* let _ = *)
  (*   List.iter (fun v -> Printf.printf "out: %s\n" (Value.layout_l v)) outs *)
  (* in *)
  let mem, newidxs, oldidxs = Mem.adds mem outs in
  Mem.reg mem inp_idx (newidxs @ oldidxs);
  (mem, n, newidxs @ oldidxs, newidxs)

let sampling_once mem f g =
  List.fold_left
    (fun (mem, expected_n, res, ns) inp_idx ->
      let mem, n, outs, newidxs = smart_eval mem f inp_idx in
      (mem, expected_n + n, res @ outs, ns @ newidxs))
    (mem, 0, [], []) g

let next_pool mem outs mode conds =
  match mode with
  | Config.SamplingCutOff ->
      Mem.filter mem (fun x -> conds.measure x && conds.pre x) outs
  | Config.CostPenalty -> Mem.filter mem conds.measure outs
  | Config.MeasureOnly -> Mem.filter mem conds.measure outs
  | Config.Correct ->
      Mem.filter mem
        (fun x ->
          match conds.client x with
          | None -> false
          | Some y ->
              (* let _ = *)
              (*   Printf.printf "%s --> %s\n" (Value.layout_l x) *)
              (*     (Value.layout_l y) *)
              (* in *)
              conds.measure x && conds.sigma x && not (conds.phi (x @ y)))
        outs

let generate mem gs mode conds f =
  let g = List.hd gs in
  let mem, _, outs, _ = sampling_once mem f g in
  let outs = next_pool mem outs mode conds in
  (mem, outs)

let mk_generation mode init_set conds f num =
  let cache = init init_set in
  let f =
    if Oplang.check_non_det f then fun inp ->
      ( non_det_sampling_times,
        List.filter_map (fun _ -> Oplang_interp.interp f inp)
        @@ List.init non_det_sampling_times (fun x -> x) )
    else fun inp ->
      ( 1,
        match Oplang_interp.interp f inp with
        | None -> []
        | Some output -> [ output ] )
  in
  let rec loop n cache =
    if n >= num then raise @@ failwith "never happen"
    else
      let mem, outs = generate cache.mem cache.gs mode conds f in
      (* let () = *)
      (*   Zlog.log_write @@ spf "outs =\n%s" *)
      (*   @@ List.split_by_comma *)
      (*        (fun x -> Value.layout_l @@ Mem.itov cache.mem x) *)
      (*        outs *)
      (* in *)
      if n >= num - 1 then cache
      else if List.length outs == 0 then
        (* early stop *)
        { mem; gs = List.init (num - n - 1) (fun _ -> []) @ cache.gs }
      else loop (n + 1) { mem; gs = outs :: cache.gs }
  in
  let scache = loop 0 cache in
  (* let () = *)
  (*   Zlog.log_write @@ spf "len(scache.gs) = %i" @@ List.length scache.gs *)
  (* in *)
  scache

let measure_conds measure =
  mk_conds measure
    (fun _ -> true)
    (fun _ -> None)
    (fun _ -> true)
    (fun _ -> true)

let mk_generation_measure_only measure init_set f num =
  let conds = measure_conds measure in
  let cache = init init_set in
  let f =
    if Oplang.check_non_det f then fun inp ->
      ( non_det_sampling_times,
        List.filter_map (fun _ -> Oplang_interp.interp f inp)
        @@ List.init non_det_sampling_times (fun x -> x) )
    else fun inp ->
      ( 1,
        match Oplang_interp.interp f inp with
        | None -> []
        | Some output -> [ output ] )
  in
  let rec loop n cache =
    if n >= num then cache
    else
      let mem, outs = generate cache.mem cache.gs Config.MeasureOnly conds f in
      if n >= num + 1 then cache
      else loop (n + 1) { mem; gs = outs :: cache.gs }
  in
  loop 0 cache

let max_pool_num = 100

let allow_stuck = true

let eval_sampling (init_set : Value.t list list) fs measure bound =
  let conds = measure_conds measure in
  let { mem; gs } = init init_set in
  let len = List.length fs in
  let f inp = (len, List.filter_map (fun f -> Oplang_interp.interp f inp) fs) in
  let res = ref [] in
  let rec aux mem num_none pool =
    let mem, expected_n, outs, _ = sampling_once mem f pool in
    (* let (mem, expected_n, outs, _), time = *)
    (*   Zlog.event_time_ "sampling_once" (fun () -> sampling_once mem f pool) *)
    (* in *)
    (* let _ = *)
    (*   Zlog.log_write *)
    (*   @@ spf "num_none:%i; pool:%i; time: %fs\n" num_none (List.length pool) *)
    (*        time *)
    (* in *)
    let num_none = num_none + expected_n - List.length outs in
    let samples = next_pool mem outs Config.MeasureOnly conds in
    if List.length samples == 0 then
      if allow_stuck then
        ( [],
          max num_none @@ (bound - List.length !res),
          List.map (Mem.itov mem) !res )
      else
        let () =
          Zlog.log_write @@ spf "data:\n%s\n"
          @@ List.split_by "\n" (fun x -> Value.layout_l @@ Mem.itov mem x) !res
        in
        raise @@ failwith "sampling get stuck"
    else
      let () = res := !res @ samples in
      let samples =
        if List.length samples > max_pool_num then
          List.sublist samples (0, max_pool_num)
        else samples
      in
      if num_none + List.length !res >= bound then
        ( List.map (Mem.itov mem) samples,
          num_none,
          List.map (Mem.itov mem) @@ List.sublist !res (0, bound) )
      else aux mem num_none samples
  in
  let pool, num_none, data = aux mem 0 @@ List.nth gs 0 in
  let () =
    if !Config.conf.show_samples_in_log then
      Zlog.log_write
      @@ spf "num_none = %i\ndata:\n%s\n" num_none
      @@ List.split_by "\n" Value.layout_l data
    else ()
  in
  (pool, num_none, data)

let pf_to_cpf (fs, f) =
  (None, f)
  :: List.map
       (fun (pre, f) ->
         if Specification.Spec.is_true pre then (None, f) else (Some pre, f))
       fs

let target_sampling conds (init_set : Value.t list list) pf bound =
  let open Specification in
  let cpfs = pf_to_cpf pf in
  let { mem; gs } = init init_set in
  let len = List.length cpfs in
  let f inp =
    ( len,
      List.filter_map
        (fun (pre, f) ->
          (* let _ = Printf.printf "input: %s\n" (Value.layout_l inp) in *)
          (* let _ = Printf.printf "%s\n" (Language.Oplang.layout f) in *)
          match pre with
          | None -> Oplang_interp.interp f inp
          | Some pre ->
              if Spec.eval pre inp then Oplang_interp.interp f inp else None)
        cpfs )
  in
  let res = ref [] in
  let rec aux mem num_none pool =
    let mem, expected_n, _, newidxs = sampling_once mem f pool in
    let num_none = num_none + expected_n - List.length newidxs in
    let samples = next_pool mem newidxs Config.Correct conds in
    let _ =
      Zlog.log_write
      @@ spf "num_none = %i\nlen(data):%i\ndata:\n%s\n" num_none
           (List.length samples)
      @@ List.split_by "\n" Value.layout_l (List.map (Mem.itov mem) samples)
    in
    if List.length samples == 0 then
      let () =
        Zlog.log_write @@ spf "data:\n%s\n"
        @@ List.split_by "\n" (fun x -> Value.layout_l @@ Mem.itov mem x) !res
      in
      raise @@ failwith "sampling get stuck; internal error"
    else
      let () = res := !res @ samples in
      if List.length !res >= bound then
        ( List.map (Mem.itov mem) samples,
          num_none,
          List.map (Mem.itov mem) @@ List.sublist !res (0, bound) )
      else aux mem num_none samples
  in
  let pool, num_none, data = aux mem 0 @@ List.nth gs 0 in
  let () =
    if !Config.conf.show_samples_in_log then
      Zlog.log_write
      @@ spf "num_none = %i\ndata:\n%s\n" num_none
      @@ List.split_by "\n" Value.layout_l data
    else ()
  in
  (pool, num_none, data)
