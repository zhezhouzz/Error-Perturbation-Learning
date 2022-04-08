open Primitive
module V = Value
open Basic_dt
module S = Sampling.Scache

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

let duplicate_level prev i j =
  (* let () = Zlog.log_write @@ spf "dupl: i: %i v.s. j: %i" i j in *)
  if i == j then Some (identity_panalty @@ (List.length prev + 1))
  else if j < i then
    (* let () = Zlog.log_write @@ spf "%i == %i" i j in *)
    let dup = S.duplicate prev j in
    let dup_times =
      List.fold_left (fun sum x -> sum +. x) 0.0
      @@ List.map (fun (dtimes, total) ->
             float_of_int (List.length dtimes)
             /. float_of_int (List.length total))
      @@ List.combine dup prev
    in
    (* let () = Zlog.log_write @@ Printf.sprintf "dup_times:%f" dup_times in *)
    Some dup_times
  else None

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
  match r1 with
  | [] -> 1.0
  | _ ->
      let r =
        try List.combine r1 r2
        with _ -> raise @@ failwith "diff_invocation_record_v2"
      in
      let a =
        match
          List.mean_opt (fun (n1, n2) -> float_of_int @@ abs @@ (n1 - n2)) r
        with
        | Some a -> a
        | None -> 0.0
      in
      let a = max 0.2 (2.0 /. (2.0 +. a)) in
      let b1, b2 =
        Sugar.map2
          (fun l -> List.map (fun (x, y) -> x - y) @@ List.c_n_2 l)
          (r1, r2)
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

let bias_penalty = 2.0

let empty_generation_penalty = 1.0

let cost_count_error _ (sigma : V.t list -> bool)
    (prog : V.t list -> int list * V.t list option) (phi : V.t list -> bool) _
    prev g mem =
  let penalty = 1.0 in
  let prev = List.flatten prev in
  let f i =
    if List.exists (fun x -> Int.equal x i) prev then penalty
    else
      match S.Mem.get_out_idxs mem i with
      | [] -> penalty
      | js ->
          let len = List.length js in
          let js' =
            List.filter
              (fun j ->
                let v = S.Mem.itov mem j in
                let _, result = prog v in
                match result with
                | None -> true
                | Some v' ->
                    if sigma v && not (phi (v @ v')) then false else true)
              js
          in
          float_of_int (List.length js') /. float_of_int len *. penalty
  in
  let c = match List.mean_opt f g with Some c -> c | None -> penalty in
  c

let cost_weighted_valid_iter (bias : V.t list -> bool)
    (sigma : V.t list -> bool) (prog : V.t list -> int list * V.t list option)
    (phi : V.t list -> bool) (i_err_non_trivial_info : Env.non_trivial_info)
    prev g mem =
  let k_bias_penalty =
    match !Config.conf.bias_method with
    | Config.SamplingCutOff | Config.Correct | Config.MeasureOnly ->
        fun _ -> 1.0
    | Config.CostPenalty ->
        fun v ->
          let p = if bias v then 1.0 else bias_penalty in
          (* let () = *)
          (*   Zlog.log_write @@ spf "v:%s; bias_penalty: %f\n" (V.layout_l v) p *)
          (* in *)
          p
  in
  let no_new = ref true in
  let f i =
    match S.Mem.get_out_idxs mem i with
    | [] -> alpha_none
    | js ->
        let one j =
          let k_dupliate =
            match duplicate_level prev i j with
            | Some k -> k
            | None ->
                no_new := false;
                1.0
          in
          (* let () = Zlog.log_write @@ spf "\tk_dupliate: %f" k_dupliate in *)
          let v = S.Mem.itov mem j in
          let delta =
            let invocation_record, result = prog v in
            let alpha =
              match result with
              | None -> alpha_none
              | Some v' ->
                  (* let () = *)
                  (*   Zlog.log_write *)
                  (*   @@ spf "v:%s; in sigma: %b; in phi: %b\n" (V.layout_l v) *)
                  (*        (sigma v) *)
                  (*        (phi (v @ v')) *)
                  (* in *)
                  if phi (v @ v') then alpha_out_pre_not_err
                  else if sigma v then
                    let k_non_trivial =
                      non_trival_v2 i_err_non_trivial_info invocation_record
                    in
                    (* let k_non_trivial = 1.0 in *)
                    (* let () = *)
                    (*   Zlog.log_write *)
                    (*   @@ spf *)
                    (* "\tk_non_trivial:%f k_bias_penalty:%f \ *)
                       (*         alpha_in_pre_is_err:%f" *)
                    (*        k_non_trivial (k_bias_penalty v) alpha_in_pre_is_err *)
                    (* in *)
                    k_non_trivial *. k_bias_penalty v *. alpha_in_pre_is_err
                  else alpha_out_pre_is_err
            in
            (* let () = Zlog.log_write @@ spf "\talpha: %f" alpha in *)
            alpha
          in
          (* let () = *)
          (*   Zlog.log_write @@ spf "\tdelta: %f; k_dupliate: %f" delta k_dupliate *)
          (* in *)
          delta *. k_dupliate
        in
        List.mean_exn one js
  in
  let c =
    match List.mean_opt f g with
    | Some c -> c
    | None -> empty_generation_penalty
  in
  let c = if !no_new then no_new_output_panalty *. c else c in
  (* let () = *)
  (*   Zlog.log_write *)
  (*   @@ spf "\tno_new_output_panalty(%b): %f; c: %f" !no_new *)
  (*        no_new_output_panalty c *)
  (* in *)
  c

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

let cal_cost (conds : S.conds) prog
    (i_err_non_trivial_info : Env.non_trivial_info) (cache : S.t) pf =
  let rec aux sum = function
    | [] -> sum
    | g :: prev ->
        let cost =
          Config.(
            match !conf.cost_function_version with
            | VWeighted ->
                cost_weighted_valid_iter conds.pre conds.sigma prog conds.phi
                  i_err_non_trivial_info prev g cache.mem
            | VCountErrors ->
                cost_count_error conds.pre conds.sigma prog conds.phi
                  i_err_non_trivial_info prev g cache.mem)
        in
        (* let () = *)
        (*   Zlog.log_write *)
        (*   @@ spf "g: %s ==* %f" (List.split_by_comma string_of_int g) cost *)
        (* in *)
        (* let () = *)
        (*   Zlog.log_write *)
        (*   @@ spf "g: %s" *)
        (*        (List.split_by_comma *)
        (*           (fun i -> V.layout_l @@ S.Mem.itov cache.mem i) *)
        (*           g) *)
        (* in *)
        (* let () = Zlog.log_write (Printf.sprintf "cost-: %f" cost) in *)
        aux (sum +. cost) prev
  in
  let c = aux 0.0 cache.gs /. float_of_int (List.length cache.gs) in
  (* let c = *)
  (*   if c < 0.1 then c -. (0.05 *. Language.Oplang_ana.insteresting pf) else c *)
  (* in *)
  c

let biased_cost_ if_free bias (env : Env.t) prog =
  let conds =
    S.mk_conds
      (if if_free then fun _ -> true else env.measure_cond)
      env.sigma
      (fun v -> snd @@ env.client env.library_inspector v)
      env.phi bias
  in
  (* let () = Zlog.time_tick_init () in *)
  let scache =
    S.mk_generation !Config.conf.bias_method env.init_sampling_set conds prog
      env.sampling_rounds
  in
  (* let () = Zlog.time_tick 0.002 in *)
  (* let () = *)
  (*   Zlog.log_write *)
  (*     (Printf.sprintf "sample cache:\n%s\n" *)
  (*        (Sampling.cache_layout scache)) *)
  (* in *)
  let cost =
    cal_cost conds
      (fun v -> env.client env.library_inspector v)
      env.i_err_non_trivial_info scache prog
  in
  let () = Zlog.log_write (Printf.sprintf "cost = %f" cost) in
  cost

let biased_cost ?(if_free = false) bias (env : Env.t) =
  let prog =
    match env.cur_p with
    | None ->
        raise
        @@ failwith (spf "[%s:%i] the env is not initialized" __FILE__ __LINE__)
    | Some cur_p -> cur_p.prog
  in
  biased_cost_ if_free bias env prog

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
    let conds =
      S.mk_conds
        (Measure.mk_measure_cond env.i_err)
        env.sigma
        (fun v -> snd @@ env.client env.library_inspector v)
        env.phi
        (fun _ -> true)
    in
    let scache =
      S.mk_generation !Config.conf.bias_method env.init_sampling_set conds f
        env.sampling_rounds
    in
    (* let () = *)
    (*   Zlog.log_write *)
    (*     (Printf.sprintf "sample cache:\n%s\n" *)
    (*        (Sampling.cache_layout scache)) *)
    (* in *)
    let cost =
      cal_cost conds
        (fun v -> env.client env.library_inspector v)
        env.i_err_non_trivial_info scache f
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
