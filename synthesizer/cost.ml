open Primitive
open Sampling
module V = Value
open Basic_dt

let alpha_in_pre_is_err = 0.05

let alpha_out_pre_is_err = 0.6

let alpha_out_pre_not_err = 0.7

let alpha_none = 1.0

let cost_valid_iter (sigma : V.t list -> bool)
    (prog : V.t list -> V.t list option) (phi : V.t list -> bool) m jump_entry =
  let num = Array.length jump_entry in
  let r =
    Array.fold_left
      (fun r j ->
        match j with
        | None -> alpha_none
        | Some j ->
            let v = Hashtbl.find m j in
            let delta =
              match prog v with
              | None -> alpha_none
              | Some v' ->
                  if phi (v @ v') then alpha_out_pre_not_err
                  else if sigma v then alpha_in_pre_is_err
                  else alpha_out_pre_is_err
            in
            r +. delta)
      0.0 jump_entry
  in
  r /. float_of_int num

let identity_panalty len = 1.5 *. float_of_int len

let duplicate_panalty = 2.0

let duplicate_level gh i j =
  if i == j then identity_panalty @@ List.length gh
  else if j < i then
    let dup_times x =
      fst
      @@ List.fold_left
           (fun (dup_times, gidx) gtotal ->
             if gidx < gtotal then (dup_times + 1, gidx) else (dup_times, gidx))
           (0, x) gh
    in
    let dup_times = dup_times j in
    let _ = Zlog.log_write @@ Printf.sprintf "dup_times:%i" dup_times in
    float_of_int @@ dup_times
  else 1.0

let delta_non_trival = 1.5

let diff_invocation_record r1 r2 =
  let r =
    try List.combine r1 r2
    with _ -> raise @@ failwith "diff_invocation_record"
  in
  List.fold_left (fun sum (n1, n2) -> sum + (abs @@ (n1 - n2))) 0 r

let non_trival r1 r2 =
  let diff = float_of_int @@ diff_invocation_record r1 r2 in
  let k = delta_non_trival /. (delta_non_trival +. diff) in
  (* let _ = Zlog.log_write @@ Printf.sprintf "|<%s> - <%s>| = %f\n" *)
  (*     (List.split_by_comma string_of_int r1) (List.split_by_comma string_of_int r2) k in *)
  k

(* 1. iteration times: 1/(1 + mean(abs(c_i - c_i'))) within [0.5, 1] *)
(* 2. relative difference comparison: C(n,2) / (Sigma ((c_i - c-j) != (c_i' - c_j')) + C(n,2)) *)
let non_trival_v2 r1 r2 =
  let r =
    try List.combine r1 r2
    with _ -> raise @@ failwith "diff_invocation_record_v2"
  in
  let a = List.mean (fun (n1, n2) -> float_of_int @@ abs @@ (n1 - n2)) r in
  let a = max 0.2 (2.0 /. (2.0 +. a)) in
  let b1, b2 =
    Sugar.map2 (fun l -> List.map (fun (x, y) -> x - y) @@ List.c_n_2 l) (r1, r2)
  in
  let b =
    try List.combine b1 b2
    with _ -> raise @@ failwith "diff_invocation_record_v2"
  in
  let b = List.map (fun (b1, b2) -> b1 == b2) b in
  let c_b_2_num = float_of_int @@ List.length b in
  let b =
    max 0.3
      (List.fold_left (fun sum x -> if x then sum +. 1.0 else sum) 0.0 b
      /. c_b_2_num)
  in
  let result = a *. b in
  (* let _ = Zlog.log_write @@ Printf.sprintf "|<%s> - <%s>| = %f * %f = %f" *)
  (*     (List.split_by_comma string_of_int r1) (List.split_by_comma string_of_int r2) a b result in *)
  result

let no_new_output_panalty = 4.5

let cost_weighted_valid_iter (sigma : V.t list -> bool)
    (prog : V.t list -> int list * V.t list option) (phi : V.t list -> bool)
    (i_err_non_trivial_info : Env.non_trivial_info) gh m jump_entry =
  let f i jopt =
    match jopt with
    | [] -> alpha_none
    | js ->
        let one j =
          let k_dupliate = duplicate_level gh i j in
          let v = Hashtbl.find m j in
          let delta =
            let invocation_record, result = prog v in
            let alpha =
              match result with
              | None -> alpha_none
              | Some v' ->
                  if phi (v @ v') then alpha_out_pre_not_err
                  else if sigma v then
                    let k_non_trivial =
                      non_trival_v2 i_err_non_trivial_info invocation_record
                    in
                    k_non_trivial *. alpha_in_pre_is_err
                  else alpha_out_pre_is_err
            in
            alpha
          in
          delta *. k_dupliate
        in
        List.mean one js
  in
  Array.meani f jump_entry

let cost_duplicate_iter jump_entry =
  let bound = Array.length jump_entry in
  let r =
    Array.fold_left
      (fun r j ->
        match j with
        | None -> 0.0
        | Some j -> if j >= bound then r else 1.0 +. r)
      0.0 jump_entry
  in
  r /. float_of_int (Array.length jump_entry)

let k_duplicate = 0.5

let k_valid = 0.5

let cal_cost (sigma : V.t list -> bool)
    (prog : V.t list -> int list * V.t list option) (phi : V.t list -> bool)
    (i_err_non_trivial_info : Env.non_trivial_info) (cache : cache) =
  let sum =
    List.fold_lefti
      (fun sum i j ->
        (* let cost_valid = cost_valid_iter sigma prog phi cache.datam_rev j in *)
        (* let cost_duplicate = cost_duplicate_iter j in *)
        (* let () = Zlog.log_write (Printf.sprintf "cost_valid[%i]: %f" i cost_valid) in *)
        (* let () = Zlog.log_write (Printf.sprintf "cost_duplicate[%i]: %f" i cost_duplicate) in *)
        let cost =
          cost_weighted_valid_iter sigma prog phi i_err_non_trivial_info
            cache.generation_hierarchy_rev cache.datam_rev j
        in
        let k_no_new gh i =
          if i == 0 then 1.0
          else
            try
              if List.nth gh i == List.nth gh (i - 1) then no_new_output_panalty
              else 1.0
            with _ ->
              raise
              @@ failwith
                   (Printf.sprintf "never happen in cost([%s]:%i)"
                      (List.split_by_comma string_of_int gh)
                      i)
        in
        let cost = k_no_new cache.generation_hierarchy_rev i *. cost in
        let () = Zlog.log_write (Printf.sprintf "cost[%i]: %f" i cost) in
        sum +. cost)
      0.0 cache.jump_table
  in
  sum /. float_of_int (List.length cache.jump_table)

(* TODO: Try to reduce the probability of [unused;unused;unused;unused] *)
let cost (env : Env.t) =
  let open Env in
  Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
    (fun () ->
      match env.cur_p with
      | None ->
          raise
          @@ failwith
               (spf "[%s:%i] the env is not initialized" __FILE__ __LINE__)
      | Some cur_p ->
          let () =
            Zlog.log_write
              (Printf.sprintf "[%s:%i] prog(non-det: %b):\n%s\n" __FILE__
                 __LINE__
                 (Language.Oplang.check_non_det cur_p.prog)
                 (Language.Oplang.layout cur_p.prog))
          in
          let scache =
            Sampling.biased_cost_sampling
              (fun _ -> true)
              env.tps env.init_sampling_set cur_p.prog env.sampling_rounds
          in
          (* let () = Zlog.log_write (Printf.sprintf "sample cache:\n%s\n" (Sampling.cache_layout scache)) in *)
          let cost =
            cal_cost env.sigma
              (env.client env.library_inspector)
              env.phi env.i_err_non_trivial_info scache
          in
          let () = Zlog.log_write (Printf.sprintf "cost = %f\n" cost) in
          cost)

let biased_cost (bias : V.t list -> bool) (env : Env.t) =
  let open Env in
  Zlog.event_ (Printf.sprintf "%s:%i[%s]-%s" __FILE__ __LINE__ __FUNCTION__ "")
    (fun () ->
      match env.cur_p with
      | None ->
          raise
          @@ failwith
               (spf "[%s:%i] the env is not initialized" __FILE__ __LINE__)
      | Some cur_p ->
          let () =
            Zlog.log_write
              (Printf.sprintf "[%s:%i] prog(non-det: %b):\n%s\n" __FILE__
                 __LINE__
                 (Language.Oplang.check_non_det cur_p.prog)
                 (Language.Oplang.layout cur_p.prog))
          in
          let scache =
            Sampling.biased_cost_sampling bias env.tps env.init_sampling_set
              cur_p.prog env.sampling_rounds
          in
          let () =
            Zlog.log_write
              (Printf.sprintf "sample cache:\n%s\n"
                 (Sampling.cache_layout scache))
          in
          let cost =
            cal_cost env.sigma
              (env.client env.library_inspector)
              env.phi env.i_err_non_trivial_info scache
          in
          let () = Zlog.log_write (Printf.sprintf "cost = %f\n" cost) in
          cost)

let test (env : Env.t) =
  let open Language.Oplang in
  let randomcos =
    {
      fin = [ (Tp.IntList, 0); (Tp.IntList, 1) ];
      body =
        [
          { op = "random_int"; args = []; res = [ (Tp.Int, 2) ] };
          { op = "random_int"; args = []; res = [ (Tp.Int, 3) ] };
          { op = "random_int"; args = []; res = [ (Tp.Int, 4) ] };
          {
            op = "cons";
            args = [ (Tp.Int, 2); (Tp.IntList, 0) ];
            res = [ (Tp.IntList, 5) ];
          };
        ];
      fout = [ (Tp.IntList, 5); (Tp.IntList, 1) ];
    }
  in
  let exchange =
    {
      fin = [ (Tp.IntList, 0); (Tp.IntList, 1) ];
      body =
        [
          { op = "random_int"; args = []; res = [ (Tp.Int, 2) ] };
          { op = "random_int"; args = []; res = [ (Tp.Int, 3) ] };
          { op = "random_int"; args = []; res = [ (Tp.Int, 4) ] };
          { op = "random_int"; args = []; res = [ (Tp.Int, 5) ] };
        ];
      fout = [ (Tp.IntList, 1); (Tp.IntList, 0) ];
    }
  in
  let cons0 =
    {
      fin = [ (Tp.IntList, 0); (Tp.IntList, 1) ];
      body =
        [
          { op = "const0"; args = []; res = [ (Tp.Int, 2) ] };
          {
            op = "cons";
            args = [ (Tp.Int, 2); (Tp.IntList, 0) ];
            res = [ (Tp.IntList, 3) ];
          };
          { op = "random_int"; args = []; res = [ (Tp.Int, 4) ] };
          { op = "random_int"; args = []; res = [ (Tp.Int, 5) ] };
        ];
      fout = [ (Tp.IntList, 1); (Tp.IntList, 3) ];
    }
  in
  let min_minus1_cons_cons =
    {
      fin = [ (Tp.IntList, 0); (Tp.IntList, 1) ];
      body =
        [
          { op = "min"; args = [ (Tp.IntList, 0) ]; res = [ (Tp.Int, 2) ] };
          { op = "minus1"; args = [ (Tp.Int, 2) ]; res = [ (Tp.Int, 3) ] };
          {
            op = "cons";
            args = [ (Tp.Int, 3); (Tp.Int, 0) ];
            res = [ (Tp.IntList, 4) ];
          };
          {
            op = "cons";
            args = [ (Tp.Int, 3); (Tp.Int, 1) ];
            res = [ (Tp.IntList, 5) ];
          };
        ];
      fout = [ (Tp.IntList, 4); (Tp.IntList, 5) ];
    }
  in
  let max_plus1_append =
    {
      fin = [ (Tp.IntList, 0); (Tp.IntList, 1) ];
      body =
        [
          { op = "max"; args = [ (Tp.IntList, 1) ]; res = [ (Tp.Int, 2) ] };
          { op = "plus1"; args = [ (Tp.Int, 2) ]; res = [ (Tp.Int, 3) ] };
          {
            op = "append";
            args = [ (Tp.Int, 1); (Tp.Int, 3) ];
            res = [ (Tp.IntList, 4) ];
          };
          { op = "random_int"; args = []; res = [ (Tp.Int, 5) ] };
        ];
      fout = [ (Tp.IntList, 4); (Tp.IntList, 0) ];
    }
  in
  let top_plus1_cons_cons =
    {
      fin = [ (Tp.IntList, 0); (Tp.IntList, 1) ];
      body =
        [
          { op = "top"; args = [ (Tp.IntList, 0) ]; res = [ (Tp.Int, 2) ] };
          { op = "plus1"; args = [ (Tp.Int, 2) ]; res = [ (Tp.Int, 3) ] };
          {
            op = "cons";
            args = [ (Tp.Int, 3); (Tp.Int, 1) ];
            res = [ (Tp.IntList, 4) ];
          };
          {
            op = "cons";
            args = [ (Tp.Int, 3); (Tp.Int, 4) ];
            res = [ (Tp.IntList, 5) ];
          };
        ];
      fout = [ (Tp.IntList, 5); (Tp.IntList, 5) ];
    }
  in
  let cost_ f =
    Zlog.log_write
      (Printf.sprintf "prog(non-det: %b):\n%s\n"
         (Language.Oplang.check_non_det f)
         (Language.Oplang.layout f));
    let scache =
      cost_sampling_ [ Tp.IntList; Tp.IntList ] env.init_sampling_set f
        env.sampling_rounds
    in
    let () =
      Zlog.log_write
        (Printf.sprintf "sample cache:\n%s\n" (Sampling.cache_layout scache))
    in
    let cost =
      cal_cost env.sigma
        (env.client env.library_inspector)
        env.phi env.i_err_non_trivial_info scache
    in
    let () = Zlog.log_write (Printf.sprintf "cost = %f\n" cost) in
    cost
  in
  let print name prog =
    let _ =
      Printf.printf "\\begin{lstlisting}\n%s:\n%s\\end{lstlisting}\n" name
      @@ layout prog
    in
    let _ = Printf.printf "cost %s: %f\n\n" name @@ cost_ prog in
    ()
  in
  let _ = print "randomcos" randomcos in
  let _ = print "exchange" exchange in
  let _ = print "cons0" cons0 in
  let _ = print "min_minus1_cons_cons" min_minus1_cons_cons in
  let _ = print "max_plus1_append" max_plus1_append in
  let _ = print "top_plus1_cons_cons" top_plus1_cons_cons in
  ()
