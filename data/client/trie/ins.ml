val ins : int -> Trie.tp -> int -> Trie.t -> Trie.t

let rec ins (default : int) (i : Trie.tp) (a : int) (m : Trie.t) =
  match m with
  | _ when Trie.leaf -> (
      match i with
      | _ when Trie.nil -> Trie.node a Trie.leaf Trie.leaf
      | _ when Trie.cons (z1 : int) (i1 : Trie.tp) ->
          if z1 > 0 then
            Trie.node default (ins default i1 a Trie.leaf) Trie.leaf
          else Trie.node default Trie.leaf (ins default i1 a Trie.leaf))
  | _ when Trie.node (y : int) (l : Trie.t) (r : Trie.t) -> (
      match i with
      | _ when Trie.nil -> Trie.node a l r
      | _ when Trie.cons (z2 : int) (i2 : Trie.tp) ->
          if z2 > 0 then Trie.node y (ins default i2 a r) l
          else Trie.node y l (ins default i2 a r))
