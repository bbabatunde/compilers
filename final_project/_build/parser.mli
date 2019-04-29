type token =
  | NUM of (int)
  | ID of (string)
  | TYID of (string)
  | DEF
  | ANDDEF
  | CLASS
  | EXTENDS
  | FIELDS
  | METHODS
  | NEW
  | THIS
  | ADD1
  | SUB1
  | LPARENSPACE
  | LPARENNOSPACE
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | LET
  | IN
  | OF
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
  | COLONEQ
  | SEMI
  | DOT
  | NIL
  | TYPE
  | LAMBDA
  | BEGIN
  | END
  | REC
  | UNDERSCORE

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Lexing.position * Lexing.position) Exprs.program
