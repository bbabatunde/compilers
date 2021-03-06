{
  open Lexing
  open Parser
  open Printf
}

let dec_digit = ['0'-'9']
let signed_int = dec_digit+ | ('-' dec_digit+)

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let tyident = "'"['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let blank = [' ' '\t']+

let space = [' ' '\t' '\n']+

rule token = parse
  | '#' [^ '\n']+ { token lexbuf }
  | blank "(" { LPARENSPACE }
  | '\n' "(" { LPARENSPACE }
  | blank "<=" { LESSEQ }
  | '\n' "<=" { LESSEQ }
  | blank "<" { LESSSPACE }
  | '\n' "<" { LESSSPACE }
  | blank { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | signed_int as x { NUM (int_of_string x) }
  | "->" { THINARROW }
  | ":" { COLON }
  | "def" { DEF }
  | "and" { ANDDEF }
  | "print" { PRINT }
  | "printStack" { PRINTSTACK }
  | "true" { TRUE }
  | "false" { FALSE }
  | "isbool" { ISBOOL }
  | "isnum" { ISNUM }
  | "add1" { ADD1 }
  | "sub1" { SUB1 }
  | "if" { IF }
  | ":" { COLON }
  | "else:" { ELSECOLON }
  | "let" { LET }
  | "in" { IN }
  | "=" { EQUAL }
  | "," { COMMA }
  | "(" { LPARENNOSPACE }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "==" { EQEQ }
  | "<" { LESSNOSPACE }
  | ">" { GREATER }
  | "<=" { LESSEQ }
  | ">=" { GREATEREQ }
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  | tyident as x { TYID x }
  | ident as x { ID x }
  | eof { EOF }
  | _ as c { failwith (sprintf "Unrecognized character: %c" c) }

