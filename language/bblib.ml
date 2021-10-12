open Primitive;;

open Basic_dt;;
module V = Value;;
type inspector = {names: string list;
                  m: (string, int * (V.t list -> (V.t list) option)) Hashtbl.t}

let modules =
  let open Imp in
  let list_module =
    ("List",
     ["is_empty", is_empty;
      "push", cons;
      "top", top;
      "tail", tail;
      "nil_rev", nil_rev;
      "cons_rev", cons_rev]
    ) in
  [list_module]


let invocation_inspector_init module_name =
  let imodules = List.map (fun name -> match List.find_opt (fun (name', _) -> String.equal name name') modules with
      | Some x -> x
      | None -> raise @@ failwith (spf "unknown module name %s" name)) module_name in
  let m = Hashtbl.create 100 in
  let l = List.flatten @@ List.map (fun (module_name, interfaces) ->
      List.map (fun (name, imp) -> spf "%s.%s" module_name name, (0, imp)) interfaces
    ) imodules in
  let names = List.map fst l in
  Hashtbl.add_seq m @@ List.to_seq l;
  {names; m}


let invocation_inspector_clear {m; _} =
  Hashtbl.filter_map_inplace (fun _ (_, imp) -> Some (0, imp)) m

let invocation_inspector_call {m; _} name input =
  (* let _ = Printf.printf "do call(%s)\n" name in *)
  match Hashtbl.find_opt m name with
  | None -> raise @@ failwith (Printf.sprintf "invocation_inspector_call: cannot find the name(%s)" name)
  | Some (i, imp) ->
    let result = imp input in
    (* let _ = Printf.printf "i = %i\n" i in *)
    Hashtbl.replace m name (i + 1, imp); result

let invocation_inspector_rev_call inspector name input =
  let rev_name = spf "%s_rev" name in
  invocation_inspector_call inspector rev_name input

let invocation_inspector_stat {names; m} =
  try List.map (fun name -> fst @@ Hashtbl.find m name) names with
  | _ -> raise @@ failwith "never happen: the invocation inspector has inconsistent names"

(* let eval libname interface_name values = *)
