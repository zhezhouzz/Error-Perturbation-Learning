let pre (x : int) (l : List.t) (u : int) (v : int) =
  (not (mem l x)) && implies (ord l u v) (u < v)
