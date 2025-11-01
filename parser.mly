%{
open Ast
%}

%token <int> INT
%token <string> IDENT
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token EQUALS
%token LEQ
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token EOF

/* precedence/associativity per spec */
%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES 

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | x = IDENT { Var x }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LET; x = IDENT; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | LPAREN; e=expr; RPAREN { e } 
  ;
