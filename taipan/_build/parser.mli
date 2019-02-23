type token =
  | NUM of (int)
  | ID of (string)
  | TYID of (string)
  | DEF
  | ANDDEF
  | ADD1
  | SUB1
  | LPARENSPACE
  | LPARENNOSPACE
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
  | LESSSPACE
  | LESSNOSPACE
  | GREATER
  | LESSEQ
  | GREATEREQ
  | AND
  | OR
  | NOT
  | THINARROW

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Lexing.position * Lexing.position) Exprs.program
