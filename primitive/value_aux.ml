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
