open Sexplib.Std
open Basic_dt

type t = { v_emb : (int, Value.t) Bihashtab.t; m : (int list, int) Hashtbl.t }
[@@deriving sexp]

type count_tab = int * (int list, (int * int) list) Hashtbl.t [@@deriving sexp]

let dirty2 t extract =
  let count0 = ref 0 in
  let count1 = ref 0 in
  let count2 = ref 0 in
  let count3 = ref 0 in
  let countmore = ref 0 in
  let count = function
    | Value.L l ->
        let n = List.length l in
        if n == 0 then count0 := !count0 + 1
        else if n == 1 then count1 := !count1 + 1
        else if n == 2 then count2 := !count2 + 1
        else if n == 3 then count3 := !count3 + 1
        else countmore := !countmore + 1
    | _ -> ()
  in
  let _ =
    Hashtbl.iter
      (fun inp_idxs _ ->
        let inp = List.map (Bihashtab.i_to_v t.v_emb) inp_idxs in
        count (extract inp))
      t.m
  in
  (!count0, !count1, !count2, !count3, !countmore)

let layout_dirty2 t =
  let a0, a1, a2, a3, amore = dirty2 t (fun l -> List.nth l 0) in
  let b0, b1, b2, b3, bmore = dirty2 t (fun l -> List.nth l 1) in
  spf
    "v_emb size: %i; m size: %i {0: %i; 1: %i; 2: %i; 3: %i; more: %i} {0: %i; \
     1: %i; 2: %i; 3: %i; more: %i}"
    (Bihashtab.length t.v_emb) (Hashtbl.length t.m) a0 a1 a2 a3 amore b0 b1 b2
    b3 bmore

let layout_dirty t =
  let count0 = ref 0 in
  let count1 = ref 0 in
  let count2 = ref 0 in
  let count3 = ref 0 in
  let countmore = ref 0 in
  let count = function
    | [ _; Value.T t ] ->
        let n = Tree.deep t in
        if n == 0 then count0 := !count0 + 1
        else if n == 1 then count1 := !count1 + 1
        else if n == 2 then count2 := !count2 + 1
        else if n == 3 then count3 := !count3 + 1
        else countmore := !countmore + 1
    | _ -> ()
  in
  let _ =
    Hashtbl.iter
      (fun inp_idxs _ ->
        let inp = List.map (Bihashtab.i_to_v t.v_emb) inp_idxs in
        count inp)
      t.m
  in
  spf "v_emb size: %i; m size: %i {0: %i; 1: %i; 2: %i; 3: %i; more: %i}"
    (Bihashtab.length t.v_emb) (Hashtbl.length t.m) !count0 !count1 !count2
    !count3 !countmore

let layout_dirty_rev t =
  let countrev = ref 0 in
  let count = function
    | [ _; Value.L l ] ->
        if IntList.eq l @@ List.rev l then countrev := !countrev + 1 else ()
    | _ -> ()
  in
  let _ =
    Hashtbl.iter
      (fun inp_idxs _ ->
        let inp = List.map (Bihashtab.i_to_v t.v_emb) inp_idxs in
        count inp)
      t.m
  in
  spf "v_emb size: %i; m size: %i {rev: %i}" (Bihashtab.length t.v_emb)
    (Hashtbl.length t.m) !countrev

let layout t =
  spf "v_emb size: %i; m size: %i" (Bihashtab.length t.v_emb)
    (Hashtbl.length t.m)

let init () = { v_emb = Bihashtab.init 20000; m = Hashtbl.create 20000 }

let list_map_opt f l =
  List.fold_left
    (fun r x ->
      match r with
      | None -> None
      | Some r -> ( match f x with None -> None | Some x -> Some (r @ [ x ])))
    (Some []) l

let mem t v =
  match list_map_opt (Bihashtab.v_to_i_opt t.v_emb) v with
  | None -> false
  | Some l -> Hashtbl.mem t.m l

let find_opt t v =
  match list_map_opt (Bihashtab.v_to_i_opt t.v_emb) v with
  | None -> None
  | Some l -> Hashtbl.find_opt t.m l

let add_opt t v iter_num =
  let v = List.map (Bihashtab.get_add t.v_emb) v in
  match Hashtbl.find_opt t.m v with
  | Some idx -> Some idx
  | None ->
      Hashtbl.add t.m v iter_num;
      None

let num_inps t = Hashtbl.length t.m

let count_init t = Hashtbl.create (num_inps t)

let count_add_replace count_tab v i =
  match Hashtbl.find_opt count_tab v with
  | Some c -> Hashtbl.replace count_tab v (i :: c)
  | None -> Hashtbl.add count_tab v [ i ]

let count_raw_bound bound f t =
  Hashtbl.fold
    (fun inp_idxs _ (total, n) ->
      if total >= bound then (total, n)
      else
        let inp = List.map (Bihashtab.i_to_v t.v_emb) inp_idxs in
        if f inp then (total + 1, n + 1) else (total + 1, n))
    t.m (0, 0)

let count_raw f t =
  Hashtbl.fold
    (fun inp_idxs _ n ->
      let inp = List.map (Bihashtab.i_to_v t.v_emb) inp_idxs in
      if f inp then n + 1 else n)
    t.m 0

let count count_tab (f, i) t =
  Hashtbl.fold
    (fun inp_idxs _ n ->
      let inp = List.map (Bihashtab.i_to_v t.v_emb) inp_idxs in
      if f inp then (
        count_add_replace count_tab inp_idxs i;
        n + 1)
      else n)
    t.m 0

let count_greater count_tab i =
  let rec aux = function [] -> false | h :: t -> i >= h || aux t in
  Hashtbl.fold (fun _ v n -> if aux v then n + 1 else n) count_tab 0

let count_all t range = List.map (count_greater t) range

let mk_count_tab t = Hashtbl.create (num_inps t)

let count_tab_add_pre t ct (i, num_step, pre) =
  Hashtbl.iter
    (fun inp_idxs _ ->
      let inp = List.map (Bihashtab.i_to_v t.v_emb) inp_idxs in
      if pre inp then
        match Hashtbl.find_opt ct inp_idxs with
        | None -> Hashtbl.add ct inp_idxs [ (i, num_step) ]
        | Some l -> Hashtbl.replace ct inp_idxs ((i, num_step) :: l)
      else ())
    t.m

let count_tab_analysis count_tab num_runs num_union idxs =
  (* let _ = *)
  (*   Printf.printf "num_runs: %i; num_union: %i; idxs: %s\n" num_runs num_union *)
  (*   @@ IntList.to_string idxs *)
  (* in *)
  let total = Hashtbl.create (num_runs * num_union * List.length idxs) in
  let _ =
    List.init num_runs (fun a ->
        List.init num_union (fun b ->
            List.map (fun c -> Hashtbl.add total (a, b, c) 0) idxs))
  in
  (* let _ = Printf.printf "? %i\n" @@ Hashtbl.find total (0, 0, 60) in *)
  let union_ rcd (run_idx, num_step) union_idx =
    match Hashtbl.find_opt rcd (run_idx, num_step) with
    | None -> Hashtbl.add rcd (run_idx, num_step) union_idx
    | Some n -> Hashtbl.replace rcd (run_idx, num_step) (min union_idx n)
  in
  let union rcd (idx, num_step) =
    let run_idx = idx / num_union in
    if run_idx >= num_runs then ()
    else
      let union_idx = idx mod num_union in
      List.iter
        (fun i ->
          if i >= num_step then union_ rcd (run_idx, i) union_idx else ())
        idxs
    (* union_ rcd (run_idx, num_step) union_idx *)
  in
  let update_ total (run_idx, union_idx, num_step) =
    match Hashtbl.find_opt total (run_idx, union_idx, num_step) with
    | None -> ()
    (* raise @@ failwith *)
    (* @@ spf "die: %i %i %i" run_idx union_idx num_step *)
    | Some n -> Hashtbl.replace total (run_idx, union_idx, num_step) (n + 1)
  in
  let update_from_union total rcd =
    Hashtbl.iter
      (fun (run_idx, num_step) min_union ->
        let rec aux union_idx =
          if union_idx >= num_union then ()
          else
            let () = update_ total (run_idx, union_idx, num_step) in
            aux (union_idx + 1)
        in
        aux min_union)
      rcd
  in
  let () =
    Hashtbl.iter
      (fun _ v ->
        let rcd = Hashtbl.create 1000 in
        List.iter (fun (idx, num_step) -> union rcd (idx, num_step)) v;
        update_from_union total rcd)
      count_tab
  in
  total

let res_to_list x = List.of_seq @@ Hashtbl.to_seq x

let get_inps t num =
  let l = List.of_seq @@ Hashtbl.to_seq_keys t.m in
  let l =
    if num > num_inps t then l (* raise @@ failwith "bad num ectx" *)
    else List.sublist l (0, num)
  in
  List.map (List.map (Bihashtab.i_to_v t.v_emb)) l

let filter t cond =
  let total = num_inps t in
  let () =
    Hashtbl.filter_map_inplace
      (fun inp_idxs v ->
        if cond (List.map (Bihashtab.i_to_v t.v_emb) inp_idxs) then Some v
        else None)
      t.m
  in
  let total' = num_inps t in
  let () = Zlog.log_write @@ spf "Data Filter: %i ---> %i" total total' in
  ()

let test () =
  let v1 = Value.L [ 2; 3; 2; 23; 5 ] in
  let v1' = Value.L [ 2; 3; 2; 23; 5 ] in
  let v2 = Value.T Tree.(Node (2, Leaf, Leaf)) in
  let v2' = Value.T Tree.(Node (2, Leaf, Leaf)) in
  let t = init () in
  let _ = add_opt t [ v1; v2 ] 0 in
  let _ =
    Printf.printf "exists? %b ~ %b | %b %b\n"
      (mem t [ v1; v2 ])
      (mem t [ v1'; v2' ])
      (mem t [ v1; v1 ])
      (mem t [ v1; v1' ])
  in
  ()
