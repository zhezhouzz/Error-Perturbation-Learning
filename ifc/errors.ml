open Machine
open Rules
open Instructions

let errors =
  [
    ( A_LE (L_Join (v_1, v_pc), v_3),
      Some (L_Join (v_pc, L_Join (v_1, v_2))),
      v_pc );
    (A_LE (v_pc, v_3), Some (L_Join (v_pc, L_Join (v_1, v_2))), v_pc);
    (A_LE (v_1, v_3), Some (L_Join (v_pc, L_Join (v_1, v_2))), v_pc);
    (A_LE (L_Join (v_1, v_pc), v_3), None, v_pc);
    (A_LE (L_Join (v_1, v_pc), v_3), Some (L_Join (v_pc, v_1)), v_pc);
    (A_LE (L_Join (v_1, v_pc), v_3), Some (L_Join (v_pc, v_2)), v_pc);
    (A_LE (L_Join (v_1, v_pc), v_3), Some (L_Join (v_pc, v_2)), v_pc);
  ]

let error_tables =
  let mk (a, b, c) = { allow = a; labRes = b; labResPC = c } in
  List.map
    (fun e op -> match op with OpStore -> mk e | _ -> default_table op)
    errors
