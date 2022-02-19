module TreeTailCall = struct
  open Ztree.Tree

  let deep tree =
    let rec aux m = function
      | [] -> m
      | (depth, Leaf) :: t -> aux (max depth m) t
      | (depth, Node (_, l, r)) :: t ->
          aux m ((depth + 1, l) :: (depth + 1, r) :: t)
    in
    aux 0 [ (0, tree) ]
end

module LabeledTreeTailCall = struct
  open Zlabeled_tree.LabeledTree

  let deep tree =
    let rec aux m = function
      | [] -> m
      | (depth, Leaf) :: t -> aux (max depth m) t
      | (depth, Node (_, _, l, r)) :: t ->
          aux m ((depth + 1, l) :: (depth + 1, r) :: t)
    in
    let r = aux 0 [ (0, tree) ] in
    (* Printf.printf "deep: %i\n" r; *)
    r
end
