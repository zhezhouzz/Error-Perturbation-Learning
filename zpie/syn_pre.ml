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

let prim_fun_to_pie_fun (f : V.t list -> V.t list option) inp =
  match f (List.map pie_v_to_prim_v inp) with
  | None -> raise PrimException
  | Some [ x ] -> prim_v_to_pie_v x
  | _ -> raise @@ failwith "should not happen"

let pie_fun_to_prim_fun (f : Value.t list -> Value.t) inp =
  try Some [ pie_v_to_prim_v @@ f (List.map prim_v_to_pie_v inp) ]
  with _ -> None

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

let post_to_pie_post post inp res =
  match res with
  | Ok v -> Spec.eval post (List.map pie_v_to_prim_v @@ inp @ [ v ])
  | Error _ -> false

open Basic_dt

let pie_post_to_post post inp =
  match List.last_destruct_opt inp with
  | None -> raise @@ failwith "should not happen"
  | Some (inp, outp) ->
      post (List.map prim_v_to_pie_v inp) (Ok (prim_v_to_pie_v outp))

let cnf_to_prim_pre cnf =
  let open CNF in
  let reduction_lit input = function
    | Pos (feature, _) -> feature input
    | Neg (feature, _) -> not @@ feature input
  in
  let reduction_clause input l = List.exists (reduction_lit input) l in
  let reduction_cnf input cnf = List.for_all (reduction_clause input) cnf in
  fun input -> reduction_cnf (List.map prim_v_to_pie_v input) cnf

type pie_bench = {
  name : string;
  args : T.tvar list;
  client : PV.t list -> PV.t;
  post : PV.t list -> (PV.t, exn) result -> bool;
  features : ((PV.t list -> bool) * string) list;
  i_err : V.t list;
  op_pool : string list;
  p_size : int;
  sampling_rounds : int;
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
          (* let () = Printf.printf "len(l): %i; n:%i\n" (List.length l) n in *)
          x);
      post =
        (fun [@warning "-8"] [ Value.List _; Value.Int _ ] res ->
          match res with Ok _ -> true | Error _ -> false);
      features =
        [
          ( (fun [@warning "-8"] [ Value.List _; Value.Int n ] -> 0 <= n),
            "0 <= n" );
          ( (fun [@warning "-8"] [ Value.List _; Value.Int n ] -> 1 <= n),
            "1 <= n" );
          ( (fun [@warning "-8"] [ Value.List (INT, l); Value.Int n ] ->
              List.length l > n),
            "len(l) > n" );
          ( (fun [@warning "-8"] [ Value.List (INT, l); Value.Int n ] ->
              List.exists
                (fun y ->
                  match y with Value.Int y when n == y -> true | _ -> false)
                l),
            "mem(l, n)" );
          ( (fun [@warning "-8"] [ Value.List (INT, l); Value.Int n ] ->
              List.length l >= n),
            "len(l) >= n" );
        ];
      i_err = [ V.L [ -1; 1; 2; 5; 7; 10; 12; 13 ]; V.I 9 ];
      op_pool = pool;
      p_size = 3;
      sampling_rounds = 16;
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

let setting_decode_to_env client_name =
  let s = setting_decode client_name in
  let tps = List.map fst s.args in
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
      i_err = s.i_err;
      i_err_non_trivial_info = [];
      init_sampling_set = [ s.i_err ];
      sampling_rounds = s.sampling_rounds;
      p_size = s.p_size;
      cur_p = None;
    }

let pie client_name (g, b) =
  let s = setting_decode client_name in
  let job =
    Job.create_unlabeled ~f:s.client ~args:(args_to_pie_args s.args)
      ~post:s.post ~features:s.features
      (List.map (List.map prim_v_to_pie_v) (g @ b))
  in
  let result, _ =
    PIE.learnPreCond job
      ~config:{ PIE.Config.default with disable_synth = true }
  in
  match result with
  | None -> ((fun _ -> false), "false")
  | Some pred -> (cnf_to_prim_pre pred, CNF.to_string pred ~stringify:snd)
