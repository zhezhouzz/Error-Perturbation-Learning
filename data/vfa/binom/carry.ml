val smash: Tree.t -> Tree.t -> Tree.t

let smash (t: Tree.t) (t_: Tree.t) =
  match t with
  | _ when Tree.Leaf -> Tree.Leaf
  | _ when Tree.Node x t1 t2 ->
    (match t2 with
     | _ when Tree.Leaf -> (match t_ with
         | _ when Tree.Leaf -> Tree.Leaf
         | _ when Tree.Node y t1_ t2_ ->(match t2_ with
             | _ when Tree.Leaf ->
               if x > y then Tree.Node x (Tree.Node y t1_ t1) Tree.Leaf
               else Tree.Node y (Tree.Node x t1 t1_) Tree.Leaf
             | _ when Tree.Node tmp0 tmp1 tmp2 -> Tree.Leaf))
     | _ when Tree.Node tmp0 tmp1 tmp2 -> Tree.Leaf)
