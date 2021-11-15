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

  let itov { datam_rev; _ } = Hashtbl.find datam_rev

  let vtoi { datam; _ } value = VM.find value datam

  let get_out_idxs mem inp_idx = Hashtbl.find mem.jump_table inp_idx

  let get_outs mem inp_idx =
    List.map (itov mem) (Hashtbl.find mem.jump_table inp_idx)

  let all_outs mem =
    List.flatten
    @@ List.init (Hashtbl.length mem.jump_table) (fun idx -> get_outs mem idx)

  let all_outs_unique mem =
    List.map (itov mem)
    @@ List.remove_duplicates @@ List.flatten
    @@ List.init (Hashtbl.length mem.jump_table) (fun idx ->
           get_out_idxs mem idx)

  let all_valid_pairs mem =
    List.flatten
    @@ List.init (Hashtbl.length mem.jump_table) (fun idx ->
           List.map (fun outp -> (itov mem idx, outp)) @@ get_outs mem idx)

  let all mem = List.of_seq @@ VM.to_seq mem.datam

  let add mem value =
    match VM.find_opt value mem.datam with
    | Some idx -> (mem, idx)
    | None ->
        let idx = Hashtbl.length mem.datam_rev in
        let datam' = VM.add value idx mem.datam in
        Hashtbl.add mem.datam_rev idx value;
        ({ mem with datam = datam' }, idx)

  let adds mem values =
    List.fold_left
      (fun (mem, res) v ->
        let mem, idx = add mem v in
        (mem, res @ [ idx ]))
      (mem, []) values

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
  let mem, idxs = Mem.adds (Mem.init ()) init_set in
  { mem; gs = [ idxs ] }

let layout { mem; gs } =
  let rec aux idx prev = function
    | [] -> prev
    | inps :: t ->
        aux (idx + 1)
          (spf "%s\n[iter %i]:\n%s" prev idx @@ Mem.jump_layout mem inps)
          t
  in
  aux 0 "" (List.rev gs)

let non_det_sampling_times = 3

(* TODO add non-det *)
let smart_eval mem f inp_idx =
  let n, outs = f @@ Mem.itov mem inp_idx in
  let mem, out_idxs = Mem.adds mem outs in
  Mem.reg mem inp_idx out_idxs;
  (mem, n, out_idxs)

let sampling_once mem f g =
  List.fold_left
    (fun (mem, expected_n, res) inp_idx ->
      let mem, n, outs = smart_eval mem f inp_idx in
      (mem, expected_n + n, res @ outs))
    (mem, 0, []) g

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
          | Some y -> conds.measure x && conds.sigma x && conds.phi (x @ y))
        outs

let generate mem gs mode conds f =
  let g = List.hd gs in
  let mem, _, outs = sampling_once mem f g in
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

let eval_sampling (init_set : Value.t list list) fs measure bound =
  let conds = measure_conds measure in
  let { mem; gs } = init init_set in
  let len = List.length fs in
  let f inp = (len, List.filter_map (fun f -> Oplang_interp.interp f inp) fs) in
  let res = ref [] in
  let rec aux mem num_none pool =
    let mem, expected_n, outs = sampling_once mem f pool in
    let num_none = num_none + expected_n - List.length outs in
    let samples = next_pool mem outs Config.MeasureOnly conds in
    if List.length samples == 0 then raise @@ failwith "sampling get stuck"
    else res := !res @ samples;
    if num_none + List.length !res >= bound then
      (num_none, List.map (Mem.itov mem) @@ List.sublist !res (0, bound))
    else aux mem num_none samples
  in
  let num_none, data = aux mem 0 @@ List.nth gs 0 in
  let () =
    Zlog.log_write
    @@ spf "num_none = %i\ndata:\n%s\n" num_none
    @@ List.split_by "\n" Value.layout_l data
  in
  (num_none, data)
