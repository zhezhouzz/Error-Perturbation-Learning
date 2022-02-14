module IntList = Datatype.Zlist.IntList
module StrList = Datatype.Zlist.StrList
module List = Datatype.Zlist.List
module Tree = Datatype.Ztree.Tree
module LabeledTree = Datatype.Zlabeled_tree.LabeledTree
module IntSet = Datatype.Zset.IntSet
module IntMap = Datatype.Zmap.IntMap
module StrMap = Datatype.Zmap.StrMap
module IntListMap = Datatype.Zmap.IntListMap
module BitArray = Datatype.Zarray.Bitarray
module Array = Datatype.Zarray.Array
module TreeTailCall = Datatype.Ztailcall.TreeTailCall
module LabeledTreeTailCall = Datatype.Ztailcall.LabeledTreeTailCall
module BinomialHeap = Datatype.Zbinomialhp
module Pairinghp = Datatype.Zpairinghp
module Physicistsq = Datatype.Zphysicistsq
module Realtimeq = Datatype.Zrealtimeq
module Skewhp = Datatype.Zskewhp

let spf = Printf.sprintf

let rec fastexpt : int -> int -> int =
 fun b n ->
  if n = 0 then 1
  else
    let b2 = fastexpt b (n / 2) in
    if n mod 2 = 0 then b2 * b2 else b * b2 * b2

module Renaming = struct
  let universe_label = ref 0

  let name () =
    let n = Printf.sprintf "x!!%i" !universe_label in
    universe_label := !universe_label + 1;
    n

  let name_tab = Hashtbl.create 100

  open Printf

  let unique name =
    match Hashtbl.find_opt name_tab name with
    | Some n ->
        Hashtbl.replace name_tab name (n + 1);
        sprintf "%s%i" name (n + 1)
    | None ->
        Hashtbl.add name_tab name 0;
        sprintf "%s%i" name 0
end
