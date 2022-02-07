%{
    (* open Past *)
    (* open Grammar *)
    %}
(* tokens *)
%token EOF LPAR RPAR LBRACK RBRACK STAR
(* keywords *)
%token SEMICOLON COLON COMMA EQ IN OUT
%token <string> IDENT
%token <string> STRING

(* start symbol *)
%start <Past.prog> prog_eof
%on_error_reduce statements
%%

prog_eof:
  | IN COLON input=args_tuple LBRACK RBRACK OUT COLON output=args_tuple ; EOF { (input, [], output) }
  | IN COLON input=args_tuple LBRACK body=statements RBRACK OUT COLON output=args_tuple ; EOF { (input, body, output) }
;
statements:
  | s1=statement SEMICOLON s2=statements {s1 :: s2}
  | s1=statement {[s1]}
;
statement:
  | at1=args_tuple EQ op=IDENT at2=args_tuple {($startpos, at1, op, at2)}
;
args_tuple:
  | LPAR RPAR {[]}
  | LPAR a=args RPAR {a}
;
args:
  | a1=arg COMMA a2=args {a1 :: a2}
  | a=arg {[a]}
;
arg:
  | x=IDENT COLON t=tp {($startpos, x, t)}
tp:
  | x=IDENT {[x], []}
  | LPAR t=tptuple RPAR {t, []}
  | x=tp y=IDENT {let elem, cs = x in elem, cs @ [y]}
tptuple:
  | x=IDENT {[x]}
  | x=IDENT STAR y=tptuple {x :: y}
;
%%
