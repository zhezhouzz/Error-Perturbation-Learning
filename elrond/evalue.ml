open Basic_dt
open Sexplib.Std

type t =
  | L of int list
  | T of int Tree.t
  | I of int
  | B of bool
  | TI of (int, int) LabeledTree.t
  | TB of (int, bool) LabeledTree.t
  | NotADt
[@@deriving sexp]

module V = Primitive.Value

let to_value = function
  | L x -> V.L x
  | T x -> V.T x
  | I x -> V.I x
  | B x -> V.B x
  | TI x -> V.TI x
  | TB x -> V.TB x
  | NotADt -> raise @@ failwith "die"

let of_value = function
  | V.L x -> L x
  | V.T x -> T x
  | V.I x -> I x
  | V.B x -> B x
  | V.TI x -> TI x
  | V.TB x -> TB x
  | _ -> raise @@ failwith "die"

let to_value_l = List.map to_value

let of_value_l = List.map of_value

type nss = (string * t list list) list [@@deriving sexp]

let load_alpha filename =
  let x = nss_of_sexp @@ Sexplib.Sexp.load_sexp filename in
  List.map (fun (name, samples) -> (name, List.map to_value_l samples)) x

let save_alpha filename x =
  let x =
    List.map (fun (name, samples) -> (name, List.map of_value_l samples)) x
  in
  Sexplib.Sexp.save filename @@ sexp_of_nss x
