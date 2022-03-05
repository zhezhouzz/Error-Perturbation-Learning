module E = Synthesizer.Env
open LoopInvGen
module PV = Value
module V = Primitive.Value
module T = Primitive.Tp

let prim_v_to_pie_v = function
  | V.I x -> PV.Int x
  | V.L x -> PV.List (INT, List.map (fun x -> PV.Int x) x)
  | _ -> raise @@ failwith "PIE does not imp"

let pie_v_to_prim_v = function
  | PV.Int x -> V.I x
  | PV.List (INT, x) ->
      V.L
        (List.map
           (fun x ->
             match x with
             | PV.Int x -> x
             | _ -> raise @@ failwith "PIE does not imp")
           x)
  | _ -> raise @@ failwith "PIE does not imp"

exception PrimException

let dummy_v = V.I 2932

let pie_res_to_res = function
  | Ok v -> Some [ V.B true; pie_v_to_prim_v v ]
  | Error _ -> Some [ V.B false; dummy_v ]

let res_to_pie_res = function
  | Some [ V.B false; _ ] -> Error PrimException
  | Some [ V.B true; v ] -> Ok (prim_v_to_pie_v v)
  | _ -> raise @@ failwith "should not happen"

let prim_fun_to_pie_fun (f : V.t list -> V.t list option) inp =
  res_to_pie_res @@ f (List.map pie_v_to_prim_v inp)

let pie_fun_to_prim_fun (f : Value.t list -> Value.t) inp =
  try Some [ V.B true; pie_v_to_prim_v @@ f (List.map prim_v_to_pie_v inp) ]
  with _ -> Some [ V.B false; dummy_v ]

let tp_to_pie_tp = function
  | T.Int -> Type.INT
  | T.IntList -> Type.(LIST INT)
  | _ -> raise @@ failwith "PIE does not imp"

let pie_tp_to_tp = function
  | Type.INT -> T.Int
  | Type.(LIST INT) -> T.IntList
  | _ -> raise @@ failwith "PIE does not imp"

open Specification

let prepost_to_pie_args (pre, _) =
  List.map (fun (tp, name) -> (name, tp_to_pie_tp tp)) pre.Spec.args

let args_to_pie_args args =
  List.map (fun (tp, name) -> (name, tp_to_pie_tp tp)) args

open Basic_dt

let post_to_pie_post post pie_inp pie_res =
  match pie_res_to_res pie_res with
  | None -> false
  | Some res ->
      let inp = List.map pie_v_to_prim_v pie_inp in
      post (res @ inp)

let pie_post_to_post pie_post inp =
  match List.last_destruct_opt inp with
  | None -> raise @@ failwith "should not happen"
  | Some (inp, outp) -> (
      match List.last_destruct_opt inp with
      | Some (_, V.B false) -> false
      | Some (inp, V.B true) ->
          pie_post (List.map prim_v_to_pie_v inp) (Ok (prim_v_to_pie_v outp))
      | _ -> raise @@ failwith "should not happen")

let cnf_to_prim_pre cnf =
  let open CNF in
  let reduction_lit input = function
    | Pos (feature, _) -> feature input
    | Neg (feature, _) -> not @@ feature input
  in
  let reduction_clause input l = List.exists (reduction_lit input) l in
  let reduction_cnf input cnf = List.for_all (reduction_clause input) cnf in
  fun input -> reduction_cnf (List.map prim_v_to_pie_v input) cnf

let cnf_normalize a =
  let open CNF in
  let concat l = List.fold_left ( ^ ) "" l in
  let a =
    List.map
      (List.map (function
        | Pos (_, str) -> "T" ^ str
        | Neg (_, str) -> "F" ^ str))
      a
  in
  let a = List.map (List.sort compare) a in
  let a = List.sort (fun a b -> compare (concat a) (concat b)) a in
  a

let cnf_eq a b =
  let lit_eq = String.equal in
  let clause_eq a b = List.eq lit_eq a b in
  let cnf_eq a b = List.eq clause_eq a b in
  cnf_eq a b

type pie_bench = {
  name : string;
  args : T.tvar list;
  client : PV.t list -> PV.t;
  post : PV.t list -> (PV.t, exn) result -> bool;
  features : ((PV.t list -> bool) * string) list;
  i_g : V.t list;
  i_err : V.t list;
  op_pool : string list;
  p_size : int;
  sampling_rounds : int;
  ans : string list list list;
}

let pool =
  [
    "cons";
    "append";
    "top";
    "tail";
    "list_destruct";
    "list_len";
    "plus1";
    "minus1";
    "const0";
    "const1";
  ]

let pie_settings =
  [
    {
      name = "list_nth";
      args = [ (T.IntList, "l"); (T.Int, "n") ];
      client =
        (fun [@warning "-8"] [ Value.List (INT, l); Value.Int n ] ->
          let x = List.nth l n in
          x);
      post =
        (fun [@warning "-8"] [ Value.List _; Value.Int _ ] res ->
          match res with Ok _ -> true | Error _ -> false);
      features =
        [
          (* ( (fun [@warning "-8"] [ Value.List _; Value.Int n ] -> 0 <= n), *)
          (*   "0 <= n" ); *)
          ( (fun [@warning "-8"] [ Value.List _; Value.Int n ] -> 0 == n),
            "0 == n" );
          ((fun [@warning "-8"] [ Value.List _; Value.Int n ] -> 0 < n), "0 < n");
          ( (fun [@warning "-8"] [ Value.List (INT, l); Value.Int n ] ->
              List.length l > n),
            "len(l) > n" );
          ( (fun [@warning "-8"] [ Value.List (INT, l); Value.Int n ] ->
              List.exists
                (fun y ->
                  match y with Value.Int y when n == y -> true | _ -> false)
                l),
            "mem(l, n)" );
          ( (fun [@warning "-8"] [ Value.List (INT, l); Value.Int _ ] ->
              List.length l == 0),
            "len(l) == 0" );
          ( (fun [@warning "-8"] [ Value.List (INT, l); Value.Int n ] ->
              List.length l >= n),
            "len(l) >= n" );
          ( (fun [@warning "-8"] [ Value.List (INT, l); Value.Int n ] ->
              List.length l == n),
            "len(l) == n" );
          ( (fun [@warning "-8"] [ Value.List (INT, l); Value.Int _ ] ->
              List.check_list_unique PV.equal l),
            "unique(l)" );
        ];
      i_g = [ V.L [ 0 ]; V.I 0 ];
      i_err = [ V.L [ -1; 1; 2; 5; 7; 10; 12; 13 ]; V.I 9 ];
      op_pool = pool;
      p_size = 3;
      sampling_rounds = 16;
      (* ans = [ [ "T0 <= n" ]; [ "Tlen(l) > n" ] ]; *)
      ans = [ [ [ "T0 < n"; "T0 == n" ]; [ "Tlen(l) > n" ] ] ];
    };
    {
      name = "list_rev";
      args = [ (T.IntList, "l") ];
      client =
        (fun [@warning "-8"] [ Value.List (INT, l) ] ->
          let x = List.rev l in
          Value.List (INT, x));
      post =
        (fun [@warning "-8"] [ Value.List (INT, l) ] res ->
          match res with
          | Ok (Value.List (INT, l')) -> not @@ List.eq Value.equal l l'
          | _ -> false);
      features =
        [
          ( (fun [@warning "-8"] [ Value.List (INT, l) ] -> List.length l == 0),
            "len(l) == 0" );
          ( (fun [@warning "-8"] [ Value.List (INT, l) ] -> List.length l <= 1),
            "len(l) <= 1" );
          ( (fun [@warning "-8"] [ Value.List (INT, l) ] ->
              match (List.destruct_opt l, List.last_destruct_opt l) with
              | Some (h, _), Some (_, la) -> Value.equal h la
              | _, _ -> false),
            "hd(l) == last(l)" );
          ( (fun [@warning "-8"] [ Value.List (INT, l) ] ->
              List.eq Value.equal l (List.rev l)),
            "l == rev(l)" );
          ( (fun [@warning "-8"] [ Value.List (INT, l) ] ->
              List.check_list_unique PV.equal l),
            "unique(l)" );
        ];
      i_g = [ V.L [ 0 ] ];
      i_err = [ V.L [ 1; 1 ] ];
      op_pool = pool;
      p_size = 3;
      sampling_rounds = 8;
      ans = [ [ [ "Fl == rev(l)" ] ] ];
    };
    {
      name = "list_append";
      args = [ (T.IntList, "l1"); (T.IntList, "l2") ];
      client =
        (fun [@warning "-8"] [ Value.List (INT, l1); Value.List (INT, l2) ] ->
          let x = l1 @ l2 in
          Value.List (INT, x));
      post =
        (fun [@warning "-8"] [ Value.List (INT, _); Value.List (INT, _) ] res ->
          match res with
          | Ok (Value.List (INT, l')) -> List.length l' == 0
          | _ -> false);
      features =
        [
          ( (fun [@warning "-8"] [ Value.List (INT, l1); _ ] ->
              List.length l1 == 0),
            "len(l1) == 0" );
          ( (fun [@warning "-8"] [ _; Value.List (INT, l2) ] ->
              List.length l2 == 0),
            "len(l2) == 0" );
          ( (fun [@warning "-8"] [ Value.List (INT, l1); _ ] ->
              List.length l1 <= 1),
            "len(l1) <= 1" );
          ( (fun [@warning "-8"] [ _; Value.List (INT, l2) ] ->
              List.length l2 <= 1),
            "len(l2) <= 1" );
          ( (fun [@warning "-8"] [ Value.List (INT, l1); Value.List (INT, l2) ] ->
              List.eq Value.equal l1 l2),
            "l1 == l2" );
          (* ( (fun [@warning "-8"] [ Value.List (INT, l1); _ ] -> *)
          (*     List.check_list_unique PV.equal l1), *)
          (*   "unique(l1)" ); *)
          (* ( (fun [@warning "-8"] [ _; Value.List (INT, l2) ] -> *)
          (*     List.check_list_unique PV.equal l2), *)
          (*   "unique(l2)" ); *)
        ];
      i_g = [ V.L []; V.L [] ];
      i_err = [ V.L [ 0 ]; V.L [ 0 ] ];
      op_pool = pool;
      p_size = 3;
      sampling_rounds = 16;
      ans =
        [
          [ [ "Tlen(l1) == 0" ]; [ "Tlen(l2) == 0" ] ];
          [ [ "Tl1 == l2" ]; [ "Tlen(l1) == 0" ] ];
          [ [ "Tl1 == l2" ]; [ "Tlen(l2) == 0" ] ];
        ];
    };
  ]

let setting_decode client_name =
  match
    List.find_opt (fun s -> String.equal s.name client_name) pie_settings
  with
  | None ->
      raise @@ failwith
      @@ Printf.sprintf "cannot find the setting of benchmark %s" client_name
  | Some s -> s

let setting_decode_to_cond client_name =
  let s = setting_decode client_name in
  let tps = List.map fst s.args in
  ( tps,
    fun x ->
      match pie_fun_to_prim_fun s.client x with
      | None -> false
      | Some outp -> pie_post_to_post s.post (x @ outp) )

let setting_decode_to_env client_name i_err =
  let s = setting_decode client_name in
  let tps = List.map fst s.args in
  let i_err = match i_err with None -> s.i_err | Some x -> x in
  (* let _ = *)
  (*   Printf.printf "i_err: %s ~> (%s) \n" (V.layout_l i_err) *)
  (*     (match pie_fun_to_prim_fun s.client i_err with *)
  (*     | None -> "none" *)
  (*     | Some y -> V.layout_l y) *)
  (* in *)
  let _ =
    if (snd @@ setting_decode_to_cond client_name) i_err then
      Printf.printf "bad!!!\n"
    else ()
  in
  E.
    {
      sigma_raw = Spec.dummy_pre tps;
      sigma = (fun _ -> true);
      client = (fun _ x -> ([], pie_fun_to_prim_fun s.client x));
      library_inspector = Language.Bblib.dummy_inspector;
      phi = pie_post_to_post s.post;
      measure_cond = Primitive.Measure.mk_measure_cond s.i_err;
      tps;
      op_pool = s.op_pool;
      preds = [];
      i_err;
      i_err_non_trivial_info = [];
      init_sampling_set = [ s.i_err ];
      sampling_rounds = s.sampling_rounds;
      p_size = s.p_size;
      cur_p = None;
    }

let pie client_name data =
  let s = setting_decode client_name in
  let job =
    Job.create_unlabeled ~f:s.client ~args:(args_to_pie_args s.args)
      ~post:s.post ~features:s.features
      (List.map (List.map prim_v_to_pie_v) (s.i_g :: data))
  in
  let result, _ =
    PIE.learnPreCond job
      ~config:{ PIE.Config.default with disable_synth = true }
  in
  match result with
  | None ->
      let pre_correct = List.exists (fun a -> cnf_eq [] a) s.ans in
      ((fun _ -> false), [], pre_correct, "false")
  | Some pred ->
      let prim_pred = cnf_to_prim_pre pred in
      (* let data = *)
      (*   [ *)
      (*     [ V.L [ 7; 8 ]; V.I (-1) ]; *)
      (*     [ V.L [ 7; 8 ]; V.I 0 ]; *)
      (*     [ V.L [ 7; 8 ]; V.I 1 ]; *)
      (*     [ V.L [ 7; 8 ]; V.I 2 ]; *)
      (*     [ V.L [ 7; 8 ]; V.I 3 ]; *)
      (*     [ V.L [ 7; 8 ]; V.I 7 ]; *)
      (*     [ V.L [ 7; 8 ]; V.I 9 ]; *)
      (*   ] *)
      (* in *)
      (* let () = *)
      (*   List.iter *)
      (*     (fun v -> *)
      (*       Printf.printf "inp: %s ~> %b\n" (V.layout_l v) (prim_pred v)) *)
      (*     data *)
      (* in *)
      let pre_correct =
        List.exists (fun a -> cnf_eq (cnf_normalize pred) a) s.ans
      in
      (* let _ = *)
      (*   Zlog.log_write *)
      (*   @@ Printf.sprintf "pie pre: - %b ~ %s | %s\n" pre_correct *)
      (*        (List.to_string (List.to_string (fun x -> x)) (cnf_normalize pred)) *)
      (*        (CNF.to_string pred ~stringify:snd) *)
      (* in *)
      (prim_pred, pred, pre_correct, CNF.to_string pred ~stringify:snd)
