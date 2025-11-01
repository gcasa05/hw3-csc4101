{
open Parser
}

let white = [' ' '\t' '\r' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+

(* a simple, friendly identifier rule; letters/underscore start, then letters/digits/underscore/' *)
let idstart = ['a'-'z' 'A'-'Z' '_']
let idchar = idstart | ['0'-'9' '\'']

rule read = parse
  | white { read lexbuf }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | "=" { EQUALS }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | idstart idchar* { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
