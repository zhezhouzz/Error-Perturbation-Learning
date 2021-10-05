%{
    (* open Past *)
    %}
(* tokens *)
%token EOF LPAR RPAR LBRACK RBRACK
(* keywords *)
%token SEMICOLON COLON COMMA EQ IN OUT
%token <string> IDENT
%token <string> STRING

(* start symbol *)
%start <Past.prog> prog_eof

%%

prog_eof:
  | IN COLON input=args LBRACK body=statements RBRACK OUT COLON output=args ; EOF { (input, body, output) }
;
statements:
  | s1=statement SEMICOLON s2=statements {s1 :: s2}
  | s1=statement {[s1]}
;
statement:
  | LPAR RPAR EQ op=IDENT LPAR RPAR {($startpos, [], op, [])}
  | LPAR res=args RPAR EQ op=IDENT LPAR RPAR {($startpos, res, op, [])}
  | LPAR res=args RPAR EQ op=IDENT LPAR a=args RPAR {($startpos, res, op, a)}
;
args:
  | a1=arg COMMA a2=args {a1 :: a2}
  | a=arg {[a]}
;
arg:
  | LPAR x=IDENT COLON t=tp RPAR {($startpos, x, t)}
tp:
  | x=IDENT {[x]}
  | x=tp y=IDENT {x @ [y]}
;
%%
