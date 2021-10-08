open Primitive;;
module T = Tp;;
module V = Value;;
module F = Feature;;

type t = {args: T.tvar list; qv: T.tvar list; fset: F.set;
            fvtab: (bool array, Label.label) Hashtbl.t}

(* TODO: reasonable initial table size *)
let mk_cctx args qv mps = {args = args; qv = qv; fset = F.mk_set args qv mps; fvtab = Hashtbl.create 10000}
