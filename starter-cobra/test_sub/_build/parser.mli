type token =
  | NUM of (int)
  | ID of (string)
  | ADD1
  | SUB1
  | LPAREN
  | RPAREN
  | LET
  | IN
  | EQUAL
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | IF
  | COLON
  | ELSECOLON
  | EOF
  | PRINT
  | PRINTSTACK
  | TRUE
  | FALSE
  | ISBOOL
  | ISNUM
  | EQEQ
  | LESS
  | GREATER
  | LESSEQ
  | GREATEREQ
  | AND
  | OR
  | NOT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Lexing.position * Lexing.position) Types.expr
