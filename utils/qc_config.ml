type size = SmallNat | SizeBound of int

type element = SmallUnsign | LowUpper of int * int

type tree_fq = { fq_leaf : int; fq_node : int }

type quick_check_config = {
  int_conf : element;
  list_conf : element * size;
  tree_conf : element * size * tree_fq;
  treei_conf : element * size * tree_fq;
  treeb_conf : element * size * tree_fq;
  binomialhp_conf : element * size;
  pairinghp_conf : element * size;
  physicistsq_conf : element * size;
  realtimeq_conf : element * size;
  skewhp_conf : element * size;
}

open Yojson.Basic.Util

let parse_size j =
  match to_string j with
  | "SmallNat" -> SmallNat
  | n -> SizeBound (int_of_string n)

let parse_element j =
  match to_string j with
  | "SmallUnsign" -> SmallUnsign
  | lu -> (
      match String.split_on_char ',' lu with
      | [ l; u ] -> LowUpper (int_of_string l, int_of_string u)
      | _ -> raise @@ failwith "parse_size")

let parse_tree_fq j =
  {
    fq_leaf = j |> member "fq_leaf" |> to_int;
    fq_node = j |> member "fq_node" |> to_int;
  }

let parse_int j = j |> member "element" |> parse_element

let parse_list j =
  (j |> member "element" |> parse_element, j |> member "size" |> parse_size)

let parse_tree j =
  ( j |> member "element" |> parse_element,
    j |> member "size" |> parse_size,
    j |> member "fq" |> parse_tree_fq )

let load_config fname =
  let j =
    try Yojson.Basic.from_file fname
    with _ ->
      raise @@ failwith (Printf.sprintf "cannot find json file(%s)" fname)
  in
  try
    {
      int_conf = j |> member "int_conf" |> parse_int;
      list_conf = j |> member "list_conf" |> parse_list;
      tree_conf = j |> member "tree_conf" |> parse_tree;
      treei_conf = j |> member "treei_conf" |> parse_tree;
      treeb_conf = j |> member "treeb_conf" |> parse_tree;
      binomialhp_conf = j |> member "binomialhp_conf" |> parse_list;
      pairinghp_conf = j |> member "pairinghp_conf" |> parse_list;
      physicistsq_conf = j |> member "physicistsq_conf" |> parse_list;
      realtimeq_conf = j |> member "realtimeq_conf" |> parse_list;
      skewhp_conf = j |> member "skewhp_conf" |> parse_list;
    }
  with _ -> raise @@ failwith "cannot load config::quich check"
