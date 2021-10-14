open Primitive;;

open Basic_dt;;
module V = Value;;
type libf = {module_name: string; interface_name: string;
             calling_num: int; imp: V.t list -> (V.t list) option;
             itps: Tp.t list; otps: Tp.t list}
type inspector = {names: string list;
                  m: (string, libf) Hashtbl.t}
let make_interfaces imp_name =
  let imps = List.filter (fun imp -> String.equal imp_name imp.Imp.imp_name) Imp.table in
  match imps with
  | [] -> raise @@ failwith (spf "lib(%s) cannot find imp..." imp_name)
  | [imp] -> imp.Imp.imp_itps, imp.Imp.imp_otps, imp.Imp.imp_exec
  | _ -> raise @@ failwith (spf "poly lib(%s) do not impelemented" imp_name)
let modules =
  let list_module =
    ("List",
     ["is_empty", "is_empty";
      "cons", "cons";
      "top", "top";
      "tail", "tail";
      "nil_rev", "nil_rev";
      "cons_rev", "cons_rev"]
    ) in
  [list_module]


let invocation_inspector_init module_name =
  let imodules = List.map (fun name -> match List.find_opt (fun (name', _) -> String.equal name name') modules with
      | Some x -> x
      | None -> raise @@ failwith (spf "unknown module name %s" name)) module_name in
  let m = Hashtbl.create 100 in
  let l = List.flatten @@ List.map (fun (module_name, interfaces) ->
      List.map (fun (interface_name, imp_name) ->
          let whole_name = spf "%s.%s" module_name interface_name in
          let itps, otps, imp = make_interfaces imp_name in
          (whole_name, {module_name; interface_name; itps; otps; imp; calling_num = 0})) interfaces
    ) imodules in
  let names = List.map fst l in
  Hashtbl.add_seq m @@ List.to_seq l;
  {names; m}


let invocation_inspector_clear {m; _} =
  Hashtbl.filter_map_inplace (fun _ stat -> Some {stat with calling_num = 0}) m

let invocation_inspector_call {m; _} name input =
  (* let _ = Printf.printf "do call(%s)\n" name in *)
  match Hashtbl.find_opt m name with
  | None -> raise @@ failwith (Printf.sprintf "invocation_inspector_call: cannot find the name(%s)" name)
  | Some stat ->
    let result = stat.imp input in
    (* let _ = Printf.printf "i = %i\n" i in *)
    Hashtbl.replace m name {stat with calling_num = stat.calling_num + 1}; result

let invocation_inspector_rev_call inspector name input =
  let rev_name = spf "%s_rev" name in
  invocation_inspector_call inspector rev_name input

let invocation_inspector_stat {names; m} =
  try List.map (fun name -> let stat = Hashtbl.find m name in stat.calling_num) names with
  | _ -> raise @@ failwith "never happen: the invocation inspector has inconsistent names"


let merge inspector input =
  let _ = invocation_inspector_clear inspector in
  let call name = invocation_inspector_call inspector name in
  let open Sugar in
  let rec aux (input: V.t list): (V.t list) option =
    match input with
    | [l1; l2] ->
      let* l1_empty = call "is_empty" [l1] in
      (match l1_empty with
       | [V.B true] -> Some [l2]
       | [V.B false] ->
         let* l2_empty = call "is_empty" [l2] in
         (match l2_empty with
          | [V.B true] -> Some [l1]
          | [V.B false] ->
            let* h1 = call "top" [l1] in
            let* h2 = call "top" [l2] in
            let* t1 = call "tail" [l1] in
            let* t2 = call "tail" [l2] in
            if h1 < h2
            then
              let* tmp = aux (t1 @ t2) in
              let* tmp = call "cons" (tmp @ h2) in
              let* tmp = call "cons" (tmp @ h1) in
              Some tmp
            else if h1 > h2
            then
              let* tmp = aux (t1 @ t2) in
              let* tmp = call "cons" (tmp @ h1) in
              let* tmp = call "cons" (tmp @ h2) in
              Some tmp
            else
              let* tmp = aux (t1 @ t2) in
              let* tmp = call "cons" (tmp @ h1) in
              Some tmp
          | _ -> raise @@ failwith "runtime client program(merge) error")
       | _ -> raise @@ failwith "runtime client program(merge) error")
    | _ -> raise @@ failwith "runtime client program(merge) error"
  in
  let result = aux input in
  let stat = invocation_inspector_stat inspector in
  (* let _ = Printf.printf "stat: %s\n" (List.split_by_comma string_of_int stat) in *)
  stat, result
