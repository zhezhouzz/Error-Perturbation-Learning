module V = Value;;
open Basic_dt;;
module ValueVector =
struct
  type t = V.t list
  let compare = List.compare V.compare
end

module ValueVectorMap = Map.Make(ValueVector);;
