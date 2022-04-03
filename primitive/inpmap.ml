open Sexplib.Std
open Basic_dt

type t = { v_emb : (int, Value.t) Bihashtab.t; m : (int list, int) Hashtbl.t }
[@@deriving sexp]

type count_tab = (int list, (int * int) list) Hashtbl.t [@@deriving sexp]

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
  (*   Printf.printf "num_runs: %i; num_union: %i\nidxs:%s\n" num_runs num_union *)
  (*     (IntList.to_string idxs) *)
  (* in *)
  let total =
    Array.init num_runs (fun _ ->
        Array.init num_union (fun _ -> Hashtbl.create 100))
  in
  let () =
    Array.iter
      (fun arr ->
        Array.iter
          (fun tab -> List.iter (fun x -> Hashtbl.add tab x 0) idxs)
          arr)
      total
  in
  let replace_map total x f =
    match Hashtbl.find_opt total x with
    | None -> raise @@ failwith "never happen"
    | Some y -> Hashtbl.replace total x (f y)
  in
  let union mat (idx, num_step) =
    let which_n = idx / num_union in
    if which_n >= num_runs then ()
    else
      let idx_in_union = idx mod num_union in
      (* let _ = Printf.printf "update %i.%i.%i\n" which_n idx_in_union num_step in *)
      Hashtbl.replace mat.(which_n).(idx_in_union) num_step ()
  in
  let update mat =
    Array.iteri
      (fun i arr ->
        Array.iteri
          (fun j tab ->
            Hashtbl.iter
              (fun num_steps _ ->
                replace_map total.(i).(j) num_steps (fun x -> x + 1))
              tab)
          arr)
      mat
  in
  let () =
    Hashtbl.iter
      (fun _ v ->
        let mat =
          Array.init num_runs (fun _ ->
              Array.init num_union (fun _ -> Hashtbl.create 100))
        in
        List.iter (fun (idx, num_step) -> union mat (idx, num_step)) v;
        update mat)
      count_tab
  in
  total

let count_result_to_json mat =
  let l =
    Array.fold_lefti
      (fun res i arr ->
        Array.fold_lefti
          (fun res j tab -> (i, j, List.of_seq @@ Hashtbl.to_seq tab) :: res)
          res arr)
      [] mat
  in
  `List
    (List.map
       (fun (i, j, x) ->
         `Assoc
           [
             ("run_idx", `Int i);
             ("num_unoin", `Int (j + 1));
             ( "stat",
               `List
                 (List.map
                    (fun (n, c) ->
                      `Assoc [ ("num_step", `Int n); ("count", `Int c) ])
                    x) );
           ])
       l)

let get_inps t num =
  if num > num_inps t then raise @@ failwith "bad num ectx"
  else
    let l = ref (0, []) in
    let () =
      Hashtbl.iter
        (fun k _ ->
          let n = fst !l in
          if n > num then () else l := (n, k :: snd !l))
        t.m
    in
    let l = snd @@ !l in
    List.map (List.map (Bihashtab.i_to_v t.v_emb)) l

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
