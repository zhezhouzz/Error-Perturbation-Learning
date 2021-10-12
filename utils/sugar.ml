let map2 f (a, b) = (f a, f b)
let map3 f (a, b, c) = (f a, f b, f c)
let map4 f (a, b, c, d) = (f a, f b, f c, f d)
let map5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)
let map6 f (a, b, c, d, e, g) = (f a, f b, f c, f d, f e, f g)
let map7 f (a, b, c, d, e, g, h) = (f a, f b, f c, f d, f e, f g, f h)

let opt_fmap (x: 'a option) (f: 'a -> 'b) : 'b option =
  match x with
  | None -> None
  | Some x -> Some (f x)

let opt_bind (x: 'a option) (f: 'a -> 'b option) : 'b option =
  match x with
  | None -> None
  | Some x -> f x

let bopt_false = function
  | Some b -> b
  | None -> false

let (let*) x f = opt_bind x f
let (let+) x f = opt_fmap x f
