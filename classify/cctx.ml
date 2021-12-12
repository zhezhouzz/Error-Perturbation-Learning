open Primitive
open Basic_dt
module T = Tp
module V = Value
module F = Feature
module Fvtab = Hashtbl.Make (BitArray)

type vec = BitArray.t

type t = {
  args : T.tvar list;
  qv : T.tvar list;
  fset : F.set;
  fvtab : Label.label Fvtab.t;
}

(* TODO: reasonable initial table size *)
let mk_cctx args qv mps =
  { args; qv; fset = F.mk_set args qv mps; fvtab = Fvtab.create 10000 }

open Label

let layout_bool x = if x then "✓" else "𐄂"

let layout_vecs vecs =
  List.fold_lefti
    (fun table i (vec, label) ->
      let vec = List.split_by_comma layout_bool (BitArray.to_bool_list vec) in
      Printf.sprintf "%s\n%s: %s [%i]" table (layout_label label) vec i)
    "" vecs

let layout_fvctx { args; qv; fset; fvtab } =
  let args =
    Printf.sprintf "args: %s\n" @@ List.split_by_comma Tp.layouttvar args
  in
  let qv = Printf.sprintf "qv: %s\n" @@ List.split_by_comma Tp.layouttvar qv in
  let fset = Printf.sprintf "fset: %s\n" @@ F.layout_set fset in
  let labeled_vecs = layout_vecs @@ List.of_seq @@ Fvtab.to_seq fvtab in
  Printf.sprintf "%s%s%s%s\n" args qv fset labeled_vecs
