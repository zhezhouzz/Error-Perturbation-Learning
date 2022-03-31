open Primitive
open Basic_dt

module Bihashtab = struct
  type ('a, 'b) t = {
    tab : ('a, 'b) Hashtbl.t;
    tab_rev : ('b, 'a) Hashtbl.t;
    a_to_string : 'a -> string;
    b_to_string : 'b -> string;
    a_of_string : string -> 'a;
    b_of_string : string -> 'b;
  }

  let init size a_to_string b_to_string a_of_string b_of_string =
    {
      tab = Hashtbl.create size;
      tab_rev = Hashtbl.create size;
      a_to_string;
      b_to_string;
      a_of_string;
      b_of_string;
    }

  let split_id = "$"

  let batch_size = 1000

  let save tag t filename =
    let oc = open_out filename in
    Printf.fprintf "%s\n" tag;
    Hashtbl.iter
      (fun (a, b) ->
        Printf.fprintf oc "%s%s%s\n" (t.a_to_string a) split_id
          (t.b_to_string b))
      t.tab;
    close_out oc

  let load filename =
    let ic = open_in filename in
    try
      let line = input_line ic in
      match tag with
      | "int_string" ->
      print_endline line;
      flush stdout;
      close_in ic
    with e ->
      close_in_noerr ic;
      raise e
end

(* Serrialization Format | Graph Structure *)
type rawpf = string option list

type ('a, 'b) bihashtbl = ('a, 'b) Hashtbl.t * ('b, 'a) Hashtbl.t

let save _

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
