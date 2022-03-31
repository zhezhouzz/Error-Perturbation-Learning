(* open Basic_dt *)
open Sexplib.Std

type ('a, 'b) t = { tab : ('a, 'b) Hashtbl.t; tab_rev : ('b, 'a) Hashtbl.t }
[@@deriving sexp]

let init size = { tab = Hashtbl.create size; tab_rev = Hashtbl.create size }

let i_s_t_save t filename =
  Sexplib.Sexp.save filename @@ sexp_of_t sexp_of_int sexp_of_string t

let i_s_t_load filename =
  t_of_sexp int_of_sexp string_of_sexp @@ Sexplib.Sexp.load_sexp filename

let i_v_t_save t filename =
  Sexplib.Sexp.save filename @@ sexp_of_t sexp_of_int Value.sexp_of_t t

let i_v_t_load filename =
  t_of_sexp int_of_sexp Value.t_of_sexp @@ Sexplib.Sexp.load_sexp filename

let length t = Hashtbl.length t.tab

let mem_i t i = Hashtbl.mem t.tab i

let mem_v t v = Hashtbl.mem t.tab_rev v

let add_v_ t v =
  let i = Hashtbl.length t.tab in
  Hashtbl.add t.tab i v;
  Hashtbl.add t.tab_rev v i;
  i

let add_v t v = if mem_v t v then None else Some (add_v_ t v)

let init_with_vs vs =
  let tab = init (List.length vs) in
  let _ = List.map (fun v -> add_v_ tab v) vs in
  tab

let get_add t v =
  match Hashtbl.find_opt t.tab_rev v with Some n -> n | None -> add_v_ t v

let i_to_v t i = Hashtbl.find t.tab i

let v_to_i t v = Hashtbl.find t.tab_rev v

let i_to_v_opt t i = Hashtbl.find_opt t.tab i

let v_to_i_opt t v = Hashtbl.find_opt t.tab_rev v

let to_vs t = Hashtbl.fold (fun _ v res -> v :: res) t.tab []
