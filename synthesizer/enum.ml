open Primitive
open Basic_dt
open Sexplib.Std

type t = {
  optab : (int, string) Bihashtab.t;
  statement_num : int;
  state : int array;
  num_pfs : int ref;
}
[@@deriving sexp]

type ectx = {
  t : t;
  m : Inpmap.t;
  enum_max_argassigns : int option;
  iter_bound : int;
  tps : Tp.t list;
  i_err : Value.t list;
}
[@@deriving sexp]

type client = {
  sigma : Value.t list -> bool;
  c : Value.t list -> Value.t list option;
  phi : Value.t list -> bool;
}

let num_inps t = Inpmap.num_inps t.m

let layout_t t =
  let ops = Bihashtab.to_vs t.optab in
  spf "optab(%i): %s\nstatement_num: %i\nstate: %s\nnum_pfs: %i"
    (List.length ops) (StrList.to_string ops) t.statement_num
    (IntList.to_string @@ Array.to_list t.state)
    !(t.num_pfs)

let layout_e ectx =
  spf "%s\nlen(m): %i" (layout_t ectx.t) (Inpmap.num_inps ectx.m)

let init ?(enum_max_argassigns = None) ?(iter_bound = 1000) statement_num
    op_pool tps i_err =
  let optab = Bihashtab.init_with_vs op_pool in
  let state = Array.init statement_num (fun _ -> 0) in
  let m = Inpmap.init () in
  let _ = Inpmap.add_opt m i_err 0 in
  {
    t = { optab; statement_num; state; num_pfs = ref 0 };
    m;
    enum_max_argassigns;
    iter_bound;
    tps;
    i_err;
  }

let make_client sigma c phi = { sigma; c; phi }

open Language

let count_pf ectx client pf =
  (* let () = Zlog.log_write @@ Oplang.layout pf in *)
  let rec loop (i, inp) =
    if i >= ectx.iter_bound then i
    else
      match Oplang_interp.interp pf inp with
      | None -> i
      | Some inp' -> (
          if not (client.sigma inp') then i
          else
            match client.c inp with
            | None -> i
            | Some outp' -> (
                if client.phi (inp' @ outp') then i
                else
                  let i' = i + 1 in
                  match Inpmap.add_opt ectx.m inp' i' with
                  | None -> loop (i', inp')
                  | Some old -> if Int.equal i' old then i else loop (i', inp'))
          )
  in
  loop (0, ectx.i_err)

let count_pfs ectx client pfs =
  List.iter
    (fun pf ->
      let _ = count_pf ectx client pf in
      (* let _ = Zlog.log_write @@ spf "counted: %i" num in *)
      ())
    pfs

let explore_state client ectx =
  let ops =
    List.map (Bihashtab.i_to_v ectx.t.optab) @@ Array.to_list ectx.t.state
  in
  (* let _ = Zlog.log_write @@ spf "ops: %s" (StrList.to_string ops) in *)
  let res =
    Arg_solving.arg_assign ~max_solution:ectx.enum_max_argassigns ectx.tps ops
  in
  (* let res = *)
  (*   Zlog.event_ "arg_assign" (fun () -> *)
  (*       Arg_solving.arg_assign ~max_solution:ectx.enum_max_argassigns ectx.tps *)
  (*         ops) *)
  (* in *)
  match res with
  | None ->
      (* let _ = Zlog.log_write "none" in *)
      ()
  | Some (_, acache) ->
      let pfs =
        Arg_solving.unfold_cache (acache.solutions, acache.prog_with_holes)
      in
      let () = ectx.t.num_pfs := !(ectx.t.num_pfs) + List.length pfs in
      let _ = count_pfs ectx client pfs in
      (* let _ = Zlog.event_ "count_pfs" (fun () -> count_pfs ectx client pfs) in *)
      ()

let next_state_ t =
  let rec reset_full i =
    if i >= t.statement_num then ()
    else (
      t.state.(i) <- Bihashtab.length t.optab - 1;
      reset_full (i + 1))
  in
  let rec loop cur_i =
    if cur_i >= t.statement_num then (
      reset_full 0;
      false)
    else
      let cur_v' = t.state.(cur_i) + 1 in
      if cur_v' >= Bihashtab.length t.optab then (
        t.state.(cur_i) <- 0;
        loop (cur_i + 1))
      else (
        t.state.(cur_i) <- cur_v';
        true)
  in
  loop 0

(* Stack overflow!! *)
let counting_upper_bound = 50000

let next_state ectx =
  let n = num_inps ectx in
  let () = Zlog.log_write @@ spf "counted:%i\n" n in
  if n > counting_upper_bound then false else next_state_ ectx.t

let run client ectx =
  let f = explore_state client in
  let rec loop () =
    (* Zlog.log_write *)
    (* @@ spf "explore_state: [%s]" *)
    (*      (List.split_by_comma (Bihashtab.i_to_v ectx.t.optab) *)
    (*      @@ Array.to_list ectx.t.state); *)
    f ectx;
    if next_state ectx then loop () else ()
  in
  let () = loop () in
  (* let () = Inpmap.filter ectx.m client.sigma in *)
  let _ = Zlog.log_write (layout_e ectx) in
  ()

let save ectx filename = Sexplib.Sexp.save filename (sexp_of_ectx ectx)

let load filename = ectx_of_sexp @@ Sexplib.Sexp.load_sexp filename

let count_init ectx = Inpmap.count_init ectx.m

let count_in_pre_raw_bound ectx bound pre =
  Inpmap.count_raw_bound bound pre ectx.m

let count_in_pre_raw ectx pre = Inpmap.count_raw pre ectx.m

let count_in_pre ct ectx pre = Inpmap.count ct pre ectx.m

let count_all ct = Inpmap.count_all ct

let test client =
  let op_pool = [ "cons"; "top"; "tail"; "append" ] in
  let statement_num = 4 in
  let ectx =
    init statement_num op_pool [ Tp.IntList; Tp.IntList ]
      [ Value.L [ 1; 2 ]; Value.L [ 3; 4 ] ]
  in
  run client ectx
