open Sexplib.Std

type t =
  | Unit
  | Bool
  | Int
  | Nat
  | IntList
  | IntBoolList
  | BoolIntBoolList
  | IntTree
  | IntTreeI
  | IntTreeB
  (* | IfcInstr *)
  (* | IfcInstrList *)
  | Uninterp of string
[@@deriving sexp]

type tvar = t * string

let layout = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Nat -> "nat"
  | IntList -> "int list"
  | IntBoolList -> "(int * bool) list"
  | BoolIntBoolList -> "(bool * int * bool) list"
  | IntTree -> "int tree"
  | IntTreeI -> "int treei"
  | IntTreeB -> "int treeb"
  (* | IfcInstr -> "instr" *)
  (* | IfcInstrList -> "instr list" *)
  | Uninterp name -> name

let of_string = function
  | "unit" -> Unit
  | "bool" -> Bool
  | "int" -> Int
  | "nat" -> Nat
  | "int list" -> IntList
  | "(int * bool) list" -> IntBoolList
  | "(bool * int * bool) list" -> BoolIntBoolList
  | "int tree" -> IntTree
  | "int treei" -> IntTreeI
  | "int treeb" -> IntTreeB
  (* | "instr" -> IfcInstr *)
  (* | "instr list" -> IfcInstrList *)
  | "binomialhp" -> Uninterp "binomialhp"
  | "binomialt" -> Uninterp "binomialt"
  | "pairinghp" -> Uninterp "pairinghp"
  | "pairingl" -> Uninterp "pairingl"
  | "physicistsq" -> Uninterp "physicistsq"
  | "realtimeq" -> Uninterp "realtimeq"
  | "skewhp" -> Uninterp "skewhp"
  | "skewt" -> Uninterp "skewt"
  | _ as tp -> failwith (Printf.sprintf "unknown type name(%s)" tp)

let layout_l = Basic_dt.List.split_by_comma layout

let compare t1 t2 =
  (* let conding = function *)
  (*   | Unit -> 0 *)
  (*   | Bool -> 1 *)
  (*   | Int -> 2 *)
  (*   | IntList -> 3 *)
  (*   | IntTree -> 4 *)
  (*   | IntTreeI -> 5 *)
  (*   | IntTreeB -> 6 *)
  (*   | IfcInstr -> 7 *)
  (*   | IfcInstrList -> 8 *)
  (*   | IntBoolList -> 9 *)
  (*   | BoolIntBoolList -> 10 *)
  (* in *)
  compare (layout t1) (layout t2)

let compare_nat_is_int t1 t2 =
  match (t1, t2) with Nat, Int -> 0 | Int, Nat -> 0 | _ -> compare t1 t2

let compare_tvar (t1, name1) (t2, name2) =
  let c1 = compare t1 t2 in
  if c1 == 0 then String.compare name1 name2 else c1

let compare_tvarl l1 l2 = List.compare compare_tvar l1 l2

let layouttvar (t, name) = layout t ^ ":" ^ name

let tavrs_to_tps (l : tvar list) = List.map (fun (tp, _) -> tp) l

let is_dt = function
  | Unit -> false
  | Int -> false
  | Nat -> false
  | Bool -> false
  | IntList -> true
  | IntTree -> true
  | IntTreeI -> true
  | IntTreeB -> true
  (* | IfcInstr -> false *)
  (* | IfcInstrList -> true *)
  | IntBoolList -> true
  | BoolIntBoolList -> true
  | Uninterp _ -> true

let eq_tp_ = function
  | Unit, Unit -> true
  | Int, Int -> true
  | Nat, Nat -> true
  | Bool, Bool -> true
  | IntList, IntList -> true
  | IntTree, IntTree -> true
  | IntTreeI, IntTreeI -> true
  | IntTreeB, IntTreeB -> true
  (* | IfcInstr, IfcInstr -> true *)
  (* | IfcInstrList, IfcInstrList -> true *)
  | IntBoolList, IntBoolList -> true
  | BoolIntBoolList, BoolIntBoolList -> true
  | Uninterp a, Uninterp b when String.equal a b -> true
  | _ -> false

let eq a b = eq_tp_ (a, b)

let tps_eq a b = try List.for_all2 eq a b with _ -> false

let tvar_eq (tp1, name1) (tp2, name2) = eq tp1 tp2 && String.equal name1 name2

module TvarVectorSet = Set.Make (struct
  let compare = List.compare compare_tvar

  type t = tvar list
end)

module TvarMap = Map.Make (struct
  let compare = compare_tvar

  type t = tvar
end)

let remove_duplicates_tvars l =
  let s = TvarVectorSet.add_seq (List.to_seq l) TvarVectorSet.empty in
  List.of_seq @@ TvarVectorSet.to_seq s

module Naming = struct
  open Basic_dt

  let make_name tp =
    let name =
      match tp with
      | Unit -> Renaming.unique "u"
      | Int -> Renaming.unique "x"
      | Nat -> Renaming.unique "n"
      | IntList -> Renaming.unique "l"
      | IntTree | IntTreeI | IntTreeB -> Renaming.unique "tr"
      | Bool -> Renaming.unique "b"
      (* | IfcInstr -> Renaming.unique "instr" *)
      (* | IfcInstrList -> Renaming.unique "instrl" *)
      | IntBoolList -> Renaming.unique "ibl"
      | BoolIntBoolList -> Renaming.unique "bibl"
      | Uninterp x -> Renaming.unique x
    in
    (tp, name)

  type tp_counter = {
    unitnum : int;
    boolnum : int;
    intnum : int;
    ilistnum : int;
    itreenum : int;
    itreeinum : int;
    itreebnum : int;
    instrnum : int;
    instrlnum : int;
    iblnum : int;
    biblnum : int;
  }

  let make_counter () =
    {
      unitnum = 0;
      boolnum = 0;
      intnum = 0;
      ilistnum = 0;
      itreenum = 0;
      itreeinum = 0;
      itreebnum = 0;
      instrnum = 0;
      instrlnum = 0;
      iblnum = 0;
      biblnum = 0;
    }

  let counter_set counter tp =
    let name s i = Printf.sprintf "%s_%i" s i in
    match tp with
    | Unit ->
        ( name "u" counter.unitnum,
          { counter with unitnum = counter.unitnum + 1 } )
    | Bool ->
        ( name "b" counter.boolnum,
          { counter with boolnum = counter.boolnum + 1 } )
    | Int ->
        (name "i" counter.intnum, { counter with intnum = counter.intnum + 1 })
    | Nat ->
        (name "n" counter.intnum, { counter with intnum = counter.intnum + 1 })
    | IntList ->
        ( name "il" counter.ilistnum,
          { counter with ilistnum = counter.ilistnum + 1 } )
    | IntTree ->
        ( name "it" counter.itreenum,
          { counter with itreenum = counter.itreenum + 1 } )
    | IntTreeI ->
        ( name "iti" counter.itreeinum,
          { counter with itreeinum = counter.itreeinum + 1 } )
    | IntTreeB ->
        ( name "itb" counter.itreebnum,
          { counter with itreebnum = counter.itreebnum + 1 } )
    | IntBoolList ->
        ( name "instr" counter.iblnum,
          { counter with iblnum = counter.iblnum + 1 } )
    | BoolIntBoolList ->
        ( name "instr" counter.biblnum,
          { counter with biblnum = counter.biblnum + 1 } )
    | Uninterp name_ ->
        ( name name_ counter.biblnum,
          { counter with biblnum = counter.biblnum + 1 } )

  let universal_counter = ref (make_counter ())

  let universal_auto_name tp =
    let name, counter = counter_set !universal_counter tp in
    let _ = universal_counter := counter in
    name

  let auto_name tps =
    let res, _ =
      List.fold_left
        (fun (r, counter) tp ->
          let name, counter = counter_set counter tp in
          (r @ [ name ], counter))
        ([], make_counter ())
        tps
    in
    res
end

(* open Yojson.Basic
 * let encode = function
 *   | Bool -> `String "B"
 *   | Int -> `String "I"
 *   | IntList -> `String "IL"
 *   | IntTree -> `String "IT"
 *   | IntTreeI -> `String "ITI"
 *   | IntTreeB -> `String "ITB"
 * let decode json =
 *   let open Util in
 *   let tp = to_string json in
 *   if String.equal "B" tp then Bool
 *   else if String.equal "I" tp then Int
 *   else if String.equal "IL" tp then IntList
 *   else if String.equal "IT" tp then IntTree
 *   else if String.equal "ITI" tp then IntTreeI
 *   else if String.equal "ITB" tp then IntTreeB
 *   else raise @@  "Lit.Tree::decode wrong type"
 *
 * let tvar_encode (tp, name) =
 *   `Assoc ["t", `String "tpv";
 *           "tp", encode tp;
 *           "n", `String name]
 *
 * let tvar_decode json =
 *   let open Util in
 *   let treetp = json |> member "t" |> to_string in
 *   if String.equal "tpv" treetp then
 *     let tp = json |> member "tp" |> decode in
 *     let name = json |> member "n" |> to_string in
 *     (tp, name)
 *   else raise @@ InterExn (Printf.sprintf "%s::decode wrong type" "tvar") *)
