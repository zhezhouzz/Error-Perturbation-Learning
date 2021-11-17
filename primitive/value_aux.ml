module V = Value
open Basic_dt

module ValueVector = struct
  type t = V.t list

  let compare = List.compare V.compare
end

module ValueVectorMap = Map.Make (ValueVector)

module ValueSet = Set.Make (struct
  let compare = V.compare

  type t = V.t
end)

let remove_duplicates l =
  let s = ValueSet.add_seq (List.to_seq l) ValueSet.empty in
  List.of_seq @@ ValueSet.to_seq s

module ValueLSet = Set.Make (struct
  let compare = List.compare V.compare

  type t = V.t list
end)

let remove_duplicates_l l =
  let s = ValueLSet.add_seq (List.to_seq l) ValueLSet.empty in
  List.of_seq @@ ValueLSet.to_seq s

type label = L1 | L2 | L12

let intersection l1 l2 =
  let m = ValueVectorMap.empty in
  let m =
    List.fold_left
      (fun m v ->
        match ValueVectorMap.find_opt v m with
        | None -> ValueVectorMap.add v L1 m
        | Some _ -> m)
      m l1
  in
  let m =
    List.fold_left
      (fun m v ->
        match ValueVectorMap.find_opt v m with
        | None -> ValueVectorMap.add v L2 m
        | Some L1 -> ValueVectorMap.add v L12 m
        | Some _ -> m)
      m l2
  in
  ValueVectorMap.fold
    (fun v label (s1, s2, s12) ->
      match label with
      | L1 -> (v :: s1, s2, s12)
      | L2 -> (s1, v :: s2, s12)
      | L12 -> (s1, s2, v :: s12))
    m ([], [], [])

module ValueArrSet = Set.Make (struct
  let compare (arr1, idx1) (arr2, idx2) =
    List.compare V.compare arr1.(idx1) arr2.(idx2)

  type t = V.t list array * int
end)

let remove_duplicates_arr arr l =
  let s =
    ValueArrSet.add_seq
      (List.to_seq (List.map (fun idx -> (arr, idx)) l))
      ValueArrSet.empty
  in
  List.map snd @@ List.of_seq @@ ValueArrSet.to_seq s
