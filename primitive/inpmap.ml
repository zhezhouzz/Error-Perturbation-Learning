open Sexplib.Std
open Basic_dt

type t = { v_emb : (int, Value.t) Bihashtab.t; m : (int list, int) Hashtbl.t }
[@@deriving sexp]

type count_tab = (int list, int list) Hashtbl.t [@@deriving sexp]

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
