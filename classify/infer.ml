open Primitive
module T = Tp
module V = Value
module F = Feature
open Basic_dt

let infer_ ctx =
  (* let () = Zlog.log_write @@ spf "fvctx:\n%s\n" @@ Cctx.layout_fvctx ctx in *)
  let dt, _ = Dtree.classify ctx in
  let body = Specification.Simplify.simplify_ite @@ Dtree.to_prop dt in
  let _ =
    Zlog.log_write
    @@ Printf.sprintf "spec: %s\n"
         (Specification.Prop.pretty_layout_prop
         @@ Specification.Simplify.simplify_ite body)
  in
  (* let body = Specification.Simplify.simplify_ite @@ Dtree.to_prop dt in *)
  (* let _ = raise @@ failwith "end" in *)
  Specification.Spec.{ args = ctx.Cctx.args; qv = ctx.Cctx.qv; body }

let pn_spec_infer ctx pos_values neg_values =
  if List.length neg_values == 0 then
    Specification.Spec.
      {
        args = ctx.Cctx.args;
        qv = ctx.Cctx.qv;
        body = Specification.Specast.True;
      }
  else if List.length pos_values == 0 then (
    Zlog.log_write ~log_level:LWarning
      "the precondition is false, prob is wrong";
    Specification.Spec.
      {
        args = ctx.Cctx.args;
        qv = ctx.Cctx.qv;
        body = Specification.Specast.(Not True);
      })
  else (
    Gather.pos_gather ctx pos_values;
    Gather.neg_gather ctx neg_values;
    infer_ ctx)

let spec_infer ctx (data : 'a list) (to_values : 'a -> V.t list)
    (judge : 'a -> bool) =
  let _ =
    if List.length data == 0 then
      raise @@ failwith "no data in precondition inference"
    else ()
  in
  let pos_values, neg_values =
    Sugar.map2 (List.map fst)
    @@ List.partition snd
    @@ List.map (fun d -> (to_values d, judge d)) data
  in
  pn_spec_infer ctx pos_values neg_values

let max_loop_num = 10

module E = Sampling.Engine
module Spec = Specification.Spec

let spec_infer_once_multi_engine ~cctx ~pos_engine ~neg_engines ~pos_filter
    ~neg_filter num_sampling =
  let infer_counter = ref 0 in
  let pos_counter = ref 0 in
  let neg_counter = ref 0 in
  let pe = pos_engine in
  let nes = neg_engines in
  let _, pos_values =
    Zlog.event_ (spf "pos_values: %i(%i)" !pos_counter !infer_counter)
      (fun () -> E.sampling_num pos_filter num_sampling pe)
  in
  let () =
    if List.length pos_values == 0 then raise @@ failwith "bad pos engine"
    else ()
  in
  let () =
    Zlog.event_
      (spf "pos_gather[%i]: %i(%i)" (List.length pos_values) !pos_counter
         !infer_counter)
      (fun () -> Gather.pos_gather cctx pos_values)
  in
  let n' = (num_sampling / List.length nes) + 1 in
  let neg_values =
    Zlog.event_ (spf "neg_values: %i(%i)" !neg_counter !infer_counter)
      (fun () ->
        List.concat
        @@ List.map (fun ne -> snd @@ E.sampling_num neg_filter n' ne) nes)
  in
  let () =
    if List.length neg_values == 0 then raise @@ failwith "neg pos engine"
    else ()
  in
  let () =
    Zlog.event_
      (spf "neg_gather[%i]: %i(%i)" (List.length neg_values) !neg_counter
         !infer_counter)
      (fun () -> Gather.neg_gather cctx neg_values)
  in
  let spec = infer_ cctx in
  let () =
    Zlog.log_write
    @@ Printf.sprintf "refined spec: %s\n"
         (Specification.Prop.pretty_layout_prop
         @@ Specification.Simplify.simplify_ite spec.Specification.Spec.body)
  in
  spec

let spec_infer_once ~cctx ~pos_engine ~neg_engine ~pos_filter ~neg_filter
    ~init_body num_sampling =
  let infer_counter = ref 0 in
  let pos_counter = ref 0 in
  let neg_counter = ref 0 in
  let pe = pos_engine in
  let ne = neg_engine in
  let _, pos_values =
    Zlog.event_ (spf "pos_values: %i(%i)" !pos_counter !infer_counter)
      (fun () -> E.sampling_num pos_filter num_sampling pe)
  in
  let () =
    if List.length pos_values == 0 then raise @@ failwith "bad pos engine"
    else ()
  in
  let () =
    Zlog.event_
      (spf "pos_gather[%i]: %i(%i)" (List.length pos_values) !pos_counter
         !infer_counter)
      (fun () -> Gather.pos_gather cctx pos_values)
  in
  let _, neg_values =
    Zlog.event_ (spf "neg_values: %i(%i)" !neg_counter !infer_counter)
      (fun () -> E.sampling_num neg_filter num_sampling ne)
  in
  let () =
    if List.length neg_values == 0 then raise @@ failwith "neg pos engine"
    else ()
  in
  let () =
    Zlog.event_
      (spf "neg_gather[%i]: %i(%i)" (List.length neg_values) !neg_counter
         !infer_counter)
      (fun () -> Gather.neg_gather cctx neg_values)
  in
  let spec = infer_ cctx in
  let () =
    Zlog.log_write
    @@ Printf.sprintf "refined spec: %s\n"
         (Specification.Prop.pretty_layout_prop
         @@ Specification.Simplify.simplify_ite spec.Specification.Spec.body)
  in
  spec

let spec_refine_loop ~cctx ~pos_engine ~neg_engine ~pos_filter ~neg_filter
    ~init_body num_sampling =
  let with_init_body spec =
    let open Spec in
    {
      args = spec.args;
      qv = spec.qv;
      body = Specification.Specast.(And [ spec.body; init_body ]);
    }
  in
  let infer_counter = ref 0 in
  let pos_counter = ref 0 in
  let neg_counter = ref 0 in
  let do_infer cctx =
    Zlog.event_ (spf "infer: %i(%i)" !neg_counter !infer_counter) (fun () ->
        infer_counter := !infer_counter + 1;
        (* let _ = *)
        (*   if !infer_counter >= 3 then raise @@ failwith (spf "infer end") *)
        (*   else () *)
        (* in *)
        infer_ cctx)
  in
  let rec neg_loop (candidate, ne) =
    if !neg_counter >= max_loop_num then
      raise @@ failwith "over max inference loop bound"
    else
      (* let _ = *)
      (*   if !infer_counter == 2 then raise @@ failwith (spf "neg start") else () *)
      (* in *)
      let ne, neg_values =
        Zlog.event_ (spf "neg_values: %i(%i)" !neg_counter !infer_counter)
          (fun () ->
            let ne, neg_values = E.sampling_num neg_filter num_sampling ne in
            ( ne,
              List.filter
                (fun v -> Specification.Spec.eval (with_init_body candidate) v)
                neg_values ))
      in
      (* let _ = *)
      (*   if !neg_counter == 2 then *)
      (*     raise @@ failwith (spf "neg end:%i" (List.length neg_values)) *)
      (*   else () *)
      (* in *)
      if List.length neg_values > 0 then (
        let () =
          Zlog.event_
            (spf "neg_gather[%i]: %i(%i)" (List.length neg_values) !neg_counter
               !infer_counter)
            (fun () -> Gather.neg_gather cctx neg_values)
        in
        let candiate' = do_infer cctx in
        let _ = raise @@ failwith "end" in
        neg_counter := !neg_counter + 1;
        neg_loop (candiate', ne))
      else (candidate, ne)
  in
  let rec pos_loop (candidate, pe, ne) =
    if !pos_counter >= max_loop_num then
      raise @@ failwith "over max inference loop bound"
    else
      let pe, pos_values =
        Zlog.event_ (spf "pos_values: %i(%i)" !pos_counter !infer_counter)
          (fun () ->
            let pe, pos_values = E.sampling_num pos_filter num_sampling pe in
            ( pe,
              List.filter
                (fun v ->
                  not @@ Specification.Spec.eval (with_init_body candidate) v)
                pos_values ))
      in
      (* let _ = *)
      (*   if !pos_counter == 2 then *)
      (*     raise @@ failwith (spf "pos end:%i" (List.length pos_values)) *)
      (*   else () *)
      (* in *)
      if List.length pos_values > 0 then (
        (* let _ = *)
        (*   if !pos_counter == 1 then *)
        (*     raise *)
        (*     @@ failwith (spf "pos gather start:%i" (List.length pos_values)) *)
        (*   else () *)
        (* in *)
        let () =
          Zlog.event_
            (spf "pos_gather[%i]: %i(%i)" (List.length pos_values) !pos_counter
               !infer_counter)
            (fun () -> Gather.pos_gather cctx pos_values)
        in
        (* let _ = *)
        (*   if !pos_counter == 1 then *)
        (*     raise @@ failwith (spf "pos gather end:%i" (List.length pos_values)) *)
        (*   else () *)
        (* in *)
        let candiate' = do_infer cctx in
        let () =
          Zlog.log_write @@ spf "pos candiate: %s" @@ Spec.layout candiate'
        in
        let _ =
          if !pos_counter == 1 then
            raise @@ failwith (spf "pos infer end:%i" (List.length pos_values))
          else ()
        in
        pos_counter := !pos_counter + 1;
        pos_loop (candiate', pe, ne))
      else (
        neg_counter := 0;
        let candidate, ne = neg_loop (candidate, ne) in
        let () =
          Zlog.log_write @@ spf "neg candiate: %s" @@ Spec.layout candidate
        in
        (* let _ = *)
        (*   raise @@ failwith (spf "neg end with %i iterations" !neg_counter) *)
        (* in *)
        if !neg_counter == 0 then candidate
        else (
          pos_counter := !pos_counter + 1;
          pos_loop (candidate, pe, ne)))
  in
  let spec =
    with_init_body
    @@ pos_loop
         ( Specification.Spec.
             {
               args = cctx.Cctx.args;
               qv = cctx.Cctx.qv;
               body = Specification.Specast.(Not True);
             },
           pos_engine,
           neg_engine )
  in
  let _ = Zlog.log_write @@ spf "cctx:\n%s" @@ Cctx.layout_fvctx cctx in
  let () =
    Zlog.log_write
    @@ Printf.sprintf "refined spec: %s\n"
         (Specification.Prop.pretty_layout_prop
         @@ Specification.Simplify.simplify_ite spec.Specification.Spec.body)
  in
  spec
