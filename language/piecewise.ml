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

let eval_sampling (cases, default) inps nondet_num =
  let run f inps =
    if F.check_non_det f then
      List.init nondet_num (fun _ -> FInterp.interp f inps)
    else [ FInterp.interp f inps ]
  in
  let rec loop = function
    | [] -> run default inps
    | (pre, f) :: cases -> if Spec.eval pre inps then run f inps else loop cases
  in
  loop cases
