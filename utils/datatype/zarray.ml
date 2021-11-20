module Array = struct
  include Array

  let fold_lefti f default arr =
    let _, r =
      fold_left (fun (idx, r) x -> (idx + 1, f r idx x)) (0, default) arr
    in
    r

  let mean_opt (f : 'a -> float) (l : 'a array) =
    if length l == 0 then None
    else
      let r = fold_left (fun sum x -> sum +. f x) 0.0 l in
      Some (r /. (float_of_int @@ length l))

  let meani_opt (f : int -> 'a -> float) (l : 'a array) =
    if length l == 0 then None
    else
      let r = fold_lefti (fun sum i x -> sum +. f i x) 0.0 l in
      Some (r /. (float_of_int @@ length l))

  let set_multi (f : int -> 'a) (i : int) (j : int) (arr : 'a array) =
    let rec aux idx =
      if idx >= j then () else set arr idx (f i);
      aux (idx + 1)
    in
    aux i
end

module Bitarray = struct
  type t = { len : int; buf : bytes }

  let create len x =
    let init = if x = true then '\255' else '\000' in
    let buf = Bytes.make ((len / 8) + 1) init in
    { len; buf }

  let get t i =
    let ch = int_of_char (Bytes.get t.buf @@ (i lsr 3)) in
    let mask = 1 lsl (i land 7) in
    ch land mask <> 0

  let set t i b =
    let index = i lsr 3 in
    let ch = int_of_char (Bytes.get t.buf index) in
    let mask = 1 lsl (i land 7) in
    let new_ch = if b then ch lor mask else ch land lnot mask in
    Bytes.set t.buf index @@ char_of_int new_ch
end
