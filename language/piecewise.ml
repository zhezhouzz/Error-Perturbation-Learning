module Spec = Specification.Spec
module F = Oplang
module FInterp = Oplang_interp
open Basic_dt

type t = (Spec.t * F.t) list * F.t

let layout (cases, default) =
  let cases_str =
    List.fold_left
      (fun str (pre, f) ->
        spf "%sPre\n%s\nPerturbation\n%s\n" str (Spec.layout pre) (F.layout f))
      "" cases
  in
  spf "%sDefault\n%s\n" cases_str (F.layout default)

let eval (cases, default) inps =
  let rec loop = function
    | [] -> FInterp.interp default inps
    | (pre, f) :: cases ->
        if Spec.eval pre inps then FInterp.interp f inps else loop cases
  in
  loop cases
