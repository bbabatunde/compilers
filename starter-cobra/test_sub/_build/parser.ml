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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Types

# 40 "parser.ml"
let yytransl_const = [|
  259 (* ADD1 *);
  260 (* SUB1 *);
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* LET *);
  264 (* IN *);
  265 (* EQUAL *);
  266 (* COMMA *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* IF *);
  271 (* COLON *);
  272 (* ELSECOLON *);
    0 (* EOF *);
  273 (* PRINT *);
  274 (* PRINTSTACK *);
  275 (* TRUE *);
  276 (* FALSE *);
  277 (* ISBOOL *);
  278 (* ISNUM *);
  279 (* EQEQ *);
  280 (* LESS *);
  281 (* GREATER *);
  282 (* LESSEQ *);
  283 (* GREATEREQ *);
  284 (* AND *);
  285 (* OR *);
  286 (* NOT *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\004\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\005\000\005\000\005\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\005\000\004\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\004\000\006\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\026\000\004\000\005\000\000\000\000\000\
\000\000\007\000\010\000\002\000\003\000\008\000\009\000\006\000\
\031\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\015\000\016\000\017\000\024\000\022\000\020\000\
\023\000\021\000\018\000\019\000\000\000\027\000\000\000\013\000\
\000\000\000\000\012\000\028\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\024\000\020\000\021\000"

let yysindex = "\002\000\
\006\255\000\000\000\000\000\000\000\000\000\000\006\255\002\255\
\006\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\255\006\000\048\255\008\255\003\255\007\255\
\001\255\006\255\000\000\036\255\036\255\036\255\036\255\036\255\
\036\255\036\255\036\255\036\255\036\255\000\000\006\255\006\255\
\006\255\011\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\255\000\000\005\255\000\000\
\002\255\006\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\228\255\249\255\050\000"

let yytablesize = 273
let yytable = "\022\000\
\029\000\025\000\001\000\023\000\026\000\027\000\003\000\004\000\
\005\000\006\000\007\000\039\000\008\000\038\000\040\000\041\000\
\056\000\011\000\042\000\009\000\058\000\057\000\010\000\011\000\
\012\000\013\000\014\000\015\000\059\000\000\000\000\000\053\000\
\054\000\055\000\000\000\016\000\003\000\004\000\005\000\006\000\
\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\060\000\000\000\010\000\011\000\012\000\013\000\
\014\000\015\000\028\000\029\000\030\000\000\000\000\000\000\000\
\000\000\016\000\000\000\000\000\000\000\000\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\000\000\
\029\000\000\000\029\000\000\000\000\000\000\000\000\000\029\000\
\029\000"

let yycheck = "\007\000\
\000\000\009\000\001\000\002\001\005\001\000\000\001\001\002\001\
\003\001\004\001\005\001\009\001\007\001\006\001\008\001\015\001\
\006\001\008\001\026\000\014\001\016\001\010\001\017\001\018\001\
\019\001\020\001\021\001\022\001\057\000\255\255\255\255\039\000\
\040\000\041\000\255\255\030\001\001\001\002\001\003\001\004\001\
\005\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\058\000\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\011\001\012\001\013\001\255\255\255\255\255\255\
\255\255\030\001\255\255\255\255\255\255\255\255\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\006\001\255\255\
\008\001\255\255\010\001\255\255\255\255\255\255\255\255\015\001\
\016\001"

let yynames_const = "\
  ADD1\000\
  SUB1\000\
  LPAREN\000\
  RPAREN\000\
  LET\000\
  IN\000\
  EQUAL\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  IF\000\
  COLON\000\
  ELSECOLON\000\
  EOF\000\
  PRINT\000\
  PRINTSTACK\000\
  TRUE\000\
  FALSE\000\
  ISBOOL\000\
  ISNUM\000\
  EQEQ\000\
  LESS\000\
  GREATER\000\
  LESSEQ\000\
  GREATEREQ\000\
  AND\000\
  OR\000\
  NOT\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 20 "parser.mly"
        ( ENumber(_1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 245 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 21 "parser.mly"
         ( EBool(true, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 251 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "parser.mly"
          ( EBool(false, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 257 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
         ( Add1 )
# 263 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
         ( Sub1 )
# 269 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "parser.mly"
        ( Not )
# 275 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
          ( Print )
# 281 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
           ( IsBool )
# 287 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
          ( IsNum )
# 293 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "parser.mly"
               ( PrintStack )
# 299 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                  ( [(_1, _3, (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1))] )
# 307 "parser.ml"
               : 'binds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'binds) in
    Obj.repr(
# 35 "parser.mly"
                              ( (_1, _3, (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1))::_5 )
# 316 "parser.ml"
               : 'binds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'prim1) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                             ( EPrim1(_1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 324 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                       ( _2 )
# 331 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 40 "parser.mly"
                               ( EPrim2(Plus, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 339 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 41 "parser.mly"
                                ( EPrim2(Minus, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 347 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 42 "parser.mly"
                                ( EPrim2(Times, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 355 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 43 "parser.mly"
                              ( EPrim2(And, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 363 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 44 "parser.mly"
                             ( EPrim2(Or, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 371 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 45 "parser.mly"
                                  ( EPrim2(Greater, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 379 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 46 "parser.mly"
                                    ( EPrim2(GreaterEq, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 387 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 47 "parser.mly"
                               ( EPrim2(Less, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 395 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 48 "parser.mly"
                                 ( EPrim2(LessEq, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 403 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 49 "parser.mly"
                               ( EPrim2(Eq, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 411 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 50 "parser.mly"
          ( _1 )
# 418 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
       ( EId(_1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 425 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'binds) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                      ( ELet(_2, _4, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 433 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                                      ( EIf(_2, _4, _6, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 442 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 56 "parser.mly"
               ( _1 )
# 449 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
             ( _1 )
# 456 "parser.ml"
               : (Lexing.position * Lexing.position) Types.expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Lexing.position * Lexing.position) Types.expr)
;;
