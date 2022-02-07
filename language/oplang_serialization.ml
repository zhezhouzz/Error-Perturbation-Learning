open Primitive
open Basic_dt

(* Serrialization Format | Graph Structure *)
type rawpf = string option list

type encoding = {
  encode_opopt : int StrMap.t;
  decode_opopt : string IntMap.t;
  encode_rawpf : (int list, int) Hashtbl.t;
  decode_rawpf : (int, int list) Hashtbl.t;
}

let init_encoding ops =
  let ops = List.sort compare ops in
  let () = Printf.printf "ops: %s\n" @@ List.to_string (fun x -> x) ops in
  {
    decode_opopt =
      IntMap.from_kv_list @@ List.mapi (fun idx x -> (idx + 1, x)) ops;
    encode_opopt =
      StrMap.from_kv_list @@ List.mapi (fun idx x -> (x, idx + 1)) ops;
    encode_rawpf = Hashtbl.create 10000;
    decode_rawpf = Hashtbl.create 10000;
  }

let opopt_to_i { encode_opopt; _ } = function
  | None -> 0
  | Some op -> StrMap.find "opopt_to_il" encode_opopt op

let i_to_opopt { decode_opopt; _ } = function
  | 0 -> None
  | x -> Some (IntMap.find "il_to_opopt" decode_opopt x)

let ipf_to_int encoding ipf = Hashtbl.find encoding.encode_rawpf ipf

let int_to_ipf encoding i = Hashtbl.find encoding.decode_rawpf i

let rawpf_to_int encoding rawpf =
  Hashtbl.find encoding.encode_rawpf @@ List.map (opopt_to_i encoding) rawpf

let int_to_rawpf encoding i =
  List.map (i_to_opopt encoding) @@ Hashtbl.find encoding.decode_rawpf i

let update_encoding e fps =
  let fps = List.map (fun fp -> List.map (opopt_to_i e) fp) fps in
  (* let () = *)
  (*   List.iter (fun fp -> Printf.printf "~ [%s]\n" @@ IntList.to_string fp) fps *)
  (* in *)
  Hashtbl.add_seq e.encode_rawpf
  @@ List.to_seq
  @@ List.mapi (fun idx x -> (x, idx)) fps;
  Hashtbl.add_seq e.decode_rawpf
  @@ List.to_seq
  @@ List.mapi (fun idx x -> (idx, x)) fps

type pf_graph = {
  ops_encoding : encoding;
  vertices : (int IntMap.t list * Oplang.t) IntMap.t;
  edges : int list IntMap.t;
  num_e : int list IntMap.t;
}

let init_pf_graph ops =
  {
    ops_encoding = init_encoding ops;
    vertices = IntMap.empty;
    edges = IntMap.empty;
    num_e = IntMap.empty;
  }

let reg_vertices pf_graph l =
  let fps, aas = List.split l in
  let () = update_encoding pf_graph.ops_encoding fps in
  let aas = List.mapi (fun i x -> (i, x)) aas in
  { pf_graph with vertices = IntMap.from_kv_list aas }

let moti_layout_stat pf_graph =
  let num_v = IntMap.cardinal pf_graph.vertices in
  let stat =
    IntMap.fold
      (fun _ nums stat ->
        (* let _ = Printf.printf "<%s>\n" @@ IntList.to_string nums in *)
        ( List.length nums,
          match IntList.max_opt nums with None -> 0 | Some x -> x )
        :: stat)
      pf_graph.num_e []
  in
  spf "details:\n%s\nnum of vertices: %i\n"
    (List.split_by "\n" (fun (len, m) -> spf "num:%i --> max: %i" len m) stat)
    num_v

(* PF Enumeration *)

let enumerate (tps : Tp.t list) (theta : string list) (len : int) =
  let enum_max_argassigns = 100 in
  let enumerate_ops (theta : string list) (len : int) =
    let theta = None :: List.map (fun x -> Some x) theta in
    List.choose_n theta len
  in
  List.filter_map
    (fun ops ->
      let ops' = List.filter_map (fun x -> x) ops in
      match
        Arg_solving.arg_assign ~max_solution:enum_max_argassigns tps ops'
      with
      | None -> None
      | Some (_, acache) ->
          Some (ops, (acache.solutions, acache.prog_with_holes)))
    (enumerate_ops theta len)

(* make edges *)

let make_edges_ encoding one =
  let ipf = int_to_ipf encoding one in
  let ipf_arr = Array.of_list ipf in
  let n = StrMap.cardinal encoding.encode_opopt in
  let default_nl = List.init n (fun x -> x) in
  let others i = List.filter (fun x -> x != i) default_nl in
  let replace idx =
    let orginal = ipf_arr.(idx) in
    let neighb =
      List.filter_map
        (fun x ->
          let () = ipf_arr.(idx) <- x in
          let l = Array.to_list ipf_arr in
          (* let () = Printf.printf "[%s]\n" @@ IntList.to_string l in *)
          try Some (ipf_to_int encoding l) with _ -> None)
        (others orginal)
    in
    ipf_arr.(idx) <- orginal;
    neighb
  in
  List.remove_duplicates @@ List.flatten
  @@ List.mapi (fun idx _ -> replace idx) ipf

let make_edges pf_graph =
  {
    pf_graph with
    edges =
      IntMap.from_kv_list
      @@ List.init (Hashtbl.length pf_graph.ops_encoding.encode_rawpf)
           (fun one -> (one, make_edges_ pf_graph.ops_encoding one));
  }

let make_num pf_graph count =
  {
    pf_graph with
    num_e =
      IntMap.from_kv_list
      @@ List.init (Hashtbl.length pf_graph.ops_encoding.encode_rawpf)
           (fun one ->
             let pfs =
               Arg_solving.unfold_cache
                 (IntMap.find "make_num" pf_graph.vertices one)
             in
             (one, List.map count pfs));
  }

(* Serrialization Read/Load | Graph Read/Load*)
open Yojson.Basic

let i_save i = `Int i

let i_load =
  let open Util in
  to_int

let str_save i = `String i

let str_load =
  let open Util in
  to_string

let l_save elm_save l = `List (List.map elm_save l)

let l_load elm_load json =
  let open Util in
  List.map elm_load @@ to_list json

let il_save = l_save i_save

let il_load = l_load i_load

let kvlist_save k_save v_save l =
  `List (List.map (fun (k, v) -> `Assoc [ ("k", k_save k); ("v", v_save v) ]) l)

let kvlist_load k_load v_load json =
  let open Util in
  List.map (fun j -> (k_load @@ member "k" j, v_load @@ member "v" j))
  @@ to_list json

let ope_save { encode_opopt; encode_rawpf; _ } =
  `Assoc
    [
      ( "encode_opopt",
        kvlist_save str_save i_save @@ StrMap.to_kv_list encode_opopt );
      ( "encode_rawpf",
        kvlist_save il_save i_save @@ List.of_seq @@ Hashtbl.to_seq encode_rawpf
      );
    ]

let ope_load json =
  let open Util in
  let kvl_encode_opopt =
    json |> member "encode_opopt" |> kvlist_load str_load i_load
  in
  let kvl_encode_rawpf =
    json |> member "encode_rawpf" |> kvlist_load il_load i_load
  in
  {
    encode_opopt = StrMap.from_kv_list kvl_encode_opopt;
    decode_opopt =
      IntMap.from_kv_list @@ List.map (fun (a, b) -> (b, a)) kvl_encode_opopt;
    encode_rawpf = Hashtbl.of_seq @@ List.to_seq kvl_encode_rawpf;
    decode_rawpf =
      Hashtbl.of_seq @@ List.to_seq
      @@ List.map (fun (a, b) -> (b, a)) kvl_encode_rawpf;
  }

let v_save m =
  kvlist_save i_save
    (fun (l, c) ->
      `Assoc
        [
          ( "argassigns",
            `List
              (List.map
                 (fun m ->
                   kvlist_save (fun x -> `Int x) (fun x -> `Int x)
                   @@ IntMap.to_kv_list m)
                 l) );
          ("oplang", `String (Oplang.layout c));
        ])
    (IntMap.to_kv_list m)

let v_load parse_string json =
  let open Util in
  IntMap.from_kv_list
  @@ kvlist_load i_load
       (fun json ->
         ( List.map (fun x -> IntMap.from_kv_list @@ kvlist_load to_int to_int x)
           @@ to_list @@ member "argassigns" json,
           let x = to_string @@ member "oplang" json in
           (* let _ = Printf.printf "%s\nend\n" x in *)
           parse_string x ))
       json

let e_save m = kvlist_save i_save il_save (IntMap.to_kv_list m)

let e_load json = IntMap.from_kv_list @@ kvlist_load i_load il_load json

let n_save m = kvlist_save i_save il_save (IntMap.to_kv_list m)

let n_load json = IntMap.from_kv_list @@ kvlist_load i_load il_load json

let pf_graph_save { ops_encoding; vertices; edges; num_e } =
  `Assoc
    [
      ("ops_encoding", ope_save ops_encoding);
      ("vertices", v_save vertices);
      ("edges", e_save edges);
      ("num_e", n_save num_e);
    ]

let pf_graph_load p json =
  let open Util in
  {
    ops_encoding = json |> member "ops_encoding" |> ope_load;
    vertices = json |> member "vertices" |> v_load p;
    edges = json |> member "edges" |> e_load;
    num_e = json |> member "num_e" |> n_load;
  }
