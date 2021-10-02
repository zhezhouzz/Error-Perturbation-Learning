open Basic_dt;;
module V = Value;;
type inspector = {names: string list;
                  m: (string, int * (V.t list -> (V.t list) option)) Hashtbl.t}
let invocation_inspector_init (interefaces: (string * (V.t list -> (V.t list) option)) list) =
  let names = List.map fst interefaces in
  let m = Hashtbl.create (List.length interefaces) in
  let _ = Hashtbl.add_seq m @@ List.to_seq @@ List.map (fun (name, imp) -> name, (0, imp)) interefaces in
  {names; m}

let invocation_inspector_clear {m; _} =
  Hashtbl.filter_map_inplace (fun _ (_, imp) -> Some (0, imp)) m

let invocation_inspector_call {m; _} name input =
  match Hashtbl.find_opt m name with
  | None -> raise @@ failwith (Printf.sprintf "invocation_inspector_call: cannot find the name(%s)" name)
  | Some (i, imp) ->
    let result = imp input in
    Hashtbl.replace m name (i + 1, imp); result

let invocation_inspector_stat {names; m} =
  try List.map (fun name -> fst @@ Hashtbl.find m name) names with
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
  let stat = invocation_inspector_stat inspector in
  stat, aux input
