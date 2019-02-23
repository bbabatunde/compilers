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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Exprs

# 46 "parser.ml"
let yytransl_const = [|
  260 (* DEF *);
  261 (* ANDDEF *);
  262 (* ADD1 *);
  263 (* SUB1 *);
  264 (* LPARENSPACE *);
  265 (* LPARENNOSPACE *);
  266 (* RPAREN *);
  267 (* LET *);
  268 (* IN *);
  269 (* EQUAL *);
  270 (* COMMA *);
  271 (* PLUS *);
  272 (* MINUS *);
  273 (* TIMES *);
  274 (* IF *);
  275 (* COLON *);
  276 (* ELSECOLON *);
    0 (* EOF *);
  277 (* PRINT *);
  278 (* PRINTSTACK *);
  279 (* TRUE *);
  280 (* FALSE *);
  281 (* ISBOOL *);
  282 (* ISNUM *);
  283 (* EQEQ *);
  284 (* LESSSPACE *);
  285 (* LESSNOSPACE *);
  286 (* GREATER *);
  287 (* LESSEQ *);
  288 (* GREATEREQ *);
  289 (* AND *);
  290 (* OR *);
  291 (* NOT *);
  292 (* THINARROW *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
  259 (* TYID *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\004\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\006\000\006\000\
\006\000\008\000\008\000\009\000\009\000\009\000\009\000\009\000\
\011\000\011\000\013\000\012\000\012\000\005\000\005\000\010\000\
\010\000\010\000\010\000\014\000\014\000\015\000\015\000\016\000\
\016\000\001\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\005\000\004\000\004\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\001\000\004\000\006\000\
\001\000\001\000\003\000\006\000\008\000\012\000\007\000\009\000\
\000\000\003\000\001\000\001\000\003\000\001\000\003\000\001\000\
\001\000\005\000\005\000\001\000\003\000\001\000\003\000\001\000\
\002\000\005\000\003\000\004\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\000\000\000\000\004\000\005\000\000\000\
\000\000\000\000\000\000\007\000\010\000\002\000\003\000\008\000\
\009\000\006\000\062\000\029\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\061\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\057\000\000\000\015\000\000\000\000\000\000\000\000\000\
\016\000\017\000\000\000\000\000\000\000\000\000\000\000\048\000\
\043\000\000\000\000\000\000\000\049\000\018\000\019\000\020\000\
\028\000\025\000\026\000\023\000\027\000\024\000\021\000\022\000\
\055\000\000\000\059\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\047\000\031\000\000\000\000\000\013\000\000\000\
\000\000\000\000\060\000\000\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\058\000\036\000\000\000\045\000\000\000\000\000\042\000\000\000\
\012\000\032\000\053\000\000\000\000\000\000\000\039\000\000\000\
\000\000\051\000\050\000\037\000\000\000\000\000\040\000\000\000\
\000\000\000\000\038\000"

let yydgoto = "\002\000\
\019\000\020\000\021\000\032\000\087\000\053\000\023\000\054\000\
\024\000\096\000\090\000\088\000\069\000\097\000\025\000\026\000"

let yysindex = "\013\000\
\088\255\000\000\000\000\008\255\016\255\000\000\000\000\180\255\
\180\255\017\255\180\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\015\255\020\000\222\255\023\255\
\027\255\180\255\120\255\000\255\025\255\032\255\022\255\034\255\
\030\255\029\255\180\255\042\255\000\000\031\255\031\255\031\255\
\031\255\031\255\031\255\031\255\031\255\031\255\031\255\031\255\
\027\255\000\000\030\000\000\000\044\255\052\255\011\255\060\255\
\000\000\000\000\042\255\180\255\180\255\180\255\054\255\000\000\
\000\000\042\255\042\255\065\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\042\255\000\000\180\255\000\000\242\254\053\255\058\255\
\055\255\041\255\000\000\000\000\059\255\056\255\000\000\061\255\
\036\255\038\255\000\000\077\000\000\000\180\255\042\255\017\255\
\243\254\060\255\071\255\017\255\180\255\042\255\042\255\042\255\
\000\000\000\000\062\255\000\000\180\255\042\255\000\000\017\255\
\000\000\000\000\000\000\072\255\073\255\180\255\000\000\065\255\
\076\255\000\000\000\000\000\000\180\255\051\255\000\000\042\255\
\069\255\180\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\000\150\255\
\210\255\000\000\000\000\000\000\000\000\000\000\002\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\081\255\000\000\000\000\063\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\092\255\000\000\
\000\000\000\000\000\000\000\000\086\255\000\000\000\000\068\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\063\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\253\255\248\255\255\255\034\001\023\000\
\000\000\223\255\009\000\163\255\000\000\193\255\068\000\093\000"

let yytablesize = 338
let yytable = "\022\000\
\030\000\033\000\068\000\098\000\102\000\117\000\029\000\030\000\
\055\000\034\000\116\000\046\000\031\000\001\000\046\000\046\000\
\027\000\028\000\031\000\037\000\086\000\103\000\118\000\035\000\
\051\000\091\000\129\000\049\000\056\000\083\000\005\000\003\000\
\004\000\063\000\057\000\033\000\006\000\007\000\008\000\009\000\
\059\000\058\000\061\000\064\000\065\000\060\000\123\000\062\000\
\100\000\066\000\067\000\012\000\013\000\014\000\015\000\016\000\
\017\000\084\000\092\000\093\000\094\000\085\000\089\000\095\000\
\099\000\018\000\104\000\105\000\106\000\115\000\107\000\111\000\
\108\000\112\000\110\000\109\000\113\000\124\000\125\000\120\000\
\126\000\130\000\131\000\133\000\128\000\134\000\136\000\138\000\
\003\000\004\000\034\000\005\000\041\000\006\000\007\000\008\000\
\009\000\011\000\010\000\033\000\114\000\044\000\137\000\052\000\
\121\000\011\000\101\000\122\000\012\000\013\000\014\000\015\000\
\016\000\017\000\119\000\127\000\081\000\050\000\000\000\000\000\
\003\000\004\000\018\000\000\000\132\000\006\000\007\000\008\000\
\009\000\052\000\010\000\135\000\000\000\000\000\000\000\000\000\
\139\000\011\000\000\000\000\000\012\000\013\000\014\000\015\000\
\016\000\017\000\000\000\000\000\000\000\000\000\054\000\054\000\
\000\000\054\000\018\000\054\000\054\000\054\000\054\000\000\000\
\054\000\000\000\000\000\000\000\000\000\000\000\000\000\054\000\
\000\000\000\000\054\000\054\000\054\000\054\000\054\000\054\000\
\000\000\000\000\000\000\000\000\003\000\004\000\000\000\000\000\
\054\000\006\000\007\000\008\000\009\000\000\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\
\012\000\013\000\014\000\015\000\016\000\017\000\000\000\000\000\
\000\000\000\000\056\000\056\000\000\000\000\000\018\000\056\000\
\056\000\056\000\056\000\000\000\056\000\000\000\000\000\000\000\
\000\000\000\000\000\000\056\000\000\000\000\000\056\000\056\000\
\056\000\056\000\056\000\056\000\038\000\039\000\040\000\000\000\
\000\000\000\000\000\000\000\000\056\000\000\000\000\000\000\000\
\041\000\042\000\043\000\044\000\045\000\046\000\047\000\048\000\
\000\000\030\000\030\000\000\000\030\000\030\000\030\000\030\000\
\030\000\000\000\030\000\030\000\030\000\000\000\030\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\033\000\033\000\036\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\082\000\033\000\000\000\000\000\000\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000"

let yycheck = "\001\000\
\000\000\010\000\036\000\067\000\019\001\019\001\008\000\009\000\
\009\001\011\000\104\000\010\001\002\001\001\000\013\001\014\001\
\009\001\002\001\002\001\000\000\010\001\036\001\036\001\009\001\
\026\000\059\000\120\000\005\001\029\001\000\000\004\001\001\001\
\002\001\035\000\010\001\000\000\006\001\007\001\008\001\009\001\
\019\001\010\001\013\001\002\001\003\001\012\001\110\000\019\001\
\082\000\008\001\009\001\021\001\022\001\023\001\024\001\025\001\
\026\001\014\001\060\000\061\000\062\000\010\001\003\001\010\001\
\000\000\035\001\014\001\010\001\014\001\103\000\030\001\036\001\
\014\001\036\001\014\001\020\001\000\000\111\000\112\000\009\001\
\019\001\010\001\010\001\019\001\118\000\010\001\036\001\019\001\
\001\001\002\001\010\001\004\001\030\001\006\001\007\001\008\001\
\009\001\012\001\011\001\108\000\102\000\010\001\136\000\036\001\
\108\000\018\001\084\000\109\000\021\001\022\001\023\001\024\001\
\025\001\026\001\106\000\117\000\049\000\025\000\255\255\255\255\
\001\001\002\001\035\001\255\255\126\000\006\001\007\001\008\001\
\009\001\010\001\011\001\133\000\255\255\255\255\255\255\255\255\
\138\000\018\001\255\255\255\255\021\001\022\001\023\001\024\001\
\025\001\026\001\255\255\255\255\255\255\255\255\001\001\002\001\
\255\255\004\001\035\001\006\001\007\001\008\001\009\001\255\255\
\011\001\255\255\255\255\255\255\255\255\255\255\255\255\018\001\
\255\255\255\255\021\001\022\001\023\001\024\001\025\001\026\001\
\255\255\255\255\255\255\255\255\001\001\002\001\255\255\255\255\
\035\001\006\001\007\001\008\001\009\001\255\255\011\001\255\255\
\255\255\255\255\255\255\255\255\255\255\018\001\255\255\255\255\
\021\001\022\001\023\001\024\001\025\001\026\001\255\255\255\255\
\255\255\255\255\001\001\002\001\255\255\255\255\035\001\006\001\
\007\001\008\001\009\001\255\255\011\001\255\255\255\255\255\255\
\255\255\255\255\255\255\018\001\255\255\255\255\021\001\022\001\
\023\001\024\001\025\001\026\001\015\001\016\001\017\001\255\255\
\255\255\255\255\255\255\255\255\035\001\255\255\255\255\255\255\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\255\255\001\001\002\001\255\255\004\001\005\001\006\001\007\001\
\008\001\255\255\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\001\001\002\001\019\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\019\001\014\001\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\035\001\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000"

let yynames_const = "\
  DEF\000\
  ANDDEF\000\
  ADD1\000\
  SUB1\000\
  LPARENSPACE\000\
  LPARENNOSPACE\000\
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
  LESSSPACE\000\
  LESSNOSPACE\000\
  GREATER\000\
  LESSEQ\000\
  GREATEREQ\000\
  AND\000\
  OR\000\
  NOT\000\
  THINARROW\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  TYID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 20 "parser.mly"
        ( ENumber(_1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 319 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 21 "parser.mly"
         ( EBool(true, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 325 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "parser.mly"
          ( EBool(false, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 331 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
         ( Add1 )
# 337 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
         ( Sub1 )
# 343 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "parser.mly"
        ( Not )
# 349 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
          ( Print )
# 355 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
           ( IsBool )
# 361 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
          ( IsNum )
# 367 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "parser.mly"
               ( PrintStack )
# 373 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bind) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                    ( [(_1, _3, (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1))] )
# 381 "parser.ml"
               : 'binds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'bind) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'binds) in
    Obj.repr(
# 35 "parser.mly"
                                ( (_1, _3, (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1))::_5 )
# 390 "parser.ml"
               : 'binds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'prim1) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                                    ( EPrim1(_1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 398 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 39 "parser.mly"
                                  ( EApp(_1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 406 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 40 "parser.mly"
                            ( EApp(_1, [], (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 413 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                            ( _2 )
# 420 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                              ( _2 )
# 427 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 43 "parser.mly"
                               ( EPrim2(Plus, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 435 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 44 "parser.mly"
                                ( EPrim2(Minus, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 443 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 45 "parser.mly"
                                ( EPrim2(Times, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 451 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 46 "parser.mly"
                              ( EPrim2(And, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 459 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 47 "parser.mly"
                             ( EPrim2(Or, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 467 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 48 "parser.mly"
                                  ( EPrim2(Greater, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 475 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 49 "parser.mly"
                                    ( EPrim2(GreaterEq, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 483 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 50 "parser.mly"
                                    ( EPrim2(Less, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 491 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 51 "parser.mly"
                                      ( EPrim2(Less, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 499 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 52 "parser.mly"
                                 ( EPrim2(LessEq, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 507 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 53 "parser.mly"
                               ( EPrim2(Eq, _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 515 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 54 "parser.mly"
          ( _1 )
# 522 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
       ( EId(_1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 529 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'binds) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                      ( ELet(_2, _4, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 537 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                                      ( EIf(_2, _4, _6, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 546 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 60 "parser.mly"
               ( _1 )
# 553 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
         ( [_1] )
# 560 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 64 "parser.mly"
                     ( _1::_3 )
# 568 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
    ( let arg_pos = Parsing.rhs_start_pos 3, Parsing.rhs_end_pos 4 in
      DFun(_2, [], SForall([], TyArr([], TyBlank arg_pos, arg_pos), arg_pos), _6, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 577 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
    (
      let typ_pos = (Parsing.rhs_start_pos 6, Parsing.rhs_end_pos 6) in
      DFun(_2, [], SForall([], TyArr([], _6, typ_pos), typ_pos), _8, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 588 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 8 : 'tyids) in
    let _7 = (Parsing.peek_val __caml_parser_env 5 : 'ids) in
    let _10 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _12 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
    (
      let arg_names = List.map (fun (name, _, a) -> (name, a)) _7 in
      let arg_types = List.map (fun (_, typ, _) -> typ) _7 in
      let arrow_pos = (Parsing.rhs_start_pos 6, Parsing.rhs_end_pos 10) in
      let typ_pos = (Parsing.rhs_start_pos 3, Parsing.rhs_end_pos 10) in
      DFun(_2, arg_names, SForall(_4, TyArr(arg_types, _10, arrow_pos), typ_pos), _12, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()))
    )
# 605 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ids) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
    (
      let arg_names = List.map (fun (name, _, a) -> (name, a)) _4 in
      let arg_types = List.map (fun (_, typ, _) -> typ) _4 in
      let typ_pos = (Parsing.rhs_start_pos 3, Parsing.rhs_end_pos 5) in
      let arr_typ = SForall([], TyArr(arg_types, TyBlank(typ_pos), typ_pos), typ_pos) in
      DFun(_2, arg_names, arr_typ, _7, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()))
    )
# 620 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'ids) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
    (
      let arg_names = List.map (fun (name, _, a) -> (name, a)) _4 in
      let arg_types = List.map (fun (_, typ, _) -> typ) _4 in
      let typ_pos = (Parsing.rhs_start_pos 3, Parsing.rhs_end_pos 7) in
      DFun(_2, arg_names, SForall([], TyArr(arg_types, _7, typ_pos), typ_pos), _9, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()))
    )
# 635 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
    ( [] )
# 641 "parser.ml"
               : 'tyids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tyids) in
    Obj.repr(
# 100 "parser.mly"
                     ( _1::_3 )
# 649 "parser.ml"
               : 'tyids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "parser.mly"
            ( TyVar(_1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 656 "parser.ml"
               : 'tyid))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bind) in
    Obj.repr(
# 105 "parser.mly"
         ( [_1] )
# 663 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bind) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ids) in
    Obj.repr(
# 106 "parser.mly"
                   ( _1::_3 )
# 671 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
       ( (_1, TyBlank(Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()), (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 678 "parser.ml"
               : 'bind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 110 "parser.mly"
                 ( (_1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 686 "parser.ml"
               : 'bind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
       ( TyCon(_1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 693 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tyid) in
    Obj.repr(
# 114 "parser.mly"
         ( _1 )
# 700 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 115 "parser.mly"
                                            ( TyArr(_2, _4, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 708 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 116 "parser.mly"
                                          ( TyArr(_2, _4, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 716 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 118 "parser.mly"
        ( [_1] )
# 723 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 119 "parser.mly"
                   ( _1::_3 )
# 731 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 122 "parser.mly"
         ( [_1] )
# 738 "parser.ml"
               : 'declgroup))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'declgroup) in
    Obj.repr(
# 123 "parser.mly"
                          ( _1::_3 )
# 746 "parser.ml"
               : 'declgroup))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declgroup) in
    Obj.repr(
# 126 "parser.mly"
              ( [_1] )
# 753 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declgroup) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 127 "parser.mly"
                    ( _1::_2 )
# 761 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 130 "parser.mly"
                             ( Program(_1, _2, _4, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 770 "parser.ml"
               : (Lexing.position * Lexing.position) Exprs.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                   ( Program(_1, _2, TyBlank(Parsing.symbol_end_pos(), Parsing.symbol_end_pos()), (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 778 "parser.ml"
               : (Lexing.position * Lexing.position) Exprs.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 132 "parser.mly"
                       ( Program([], _1, _3, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 786 "parser.ml"
               : (Lexing.position * Lexing.position) Exprs.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
             ( Program([], _1, TyBlank(Parsing.symbol_end_pos(), Parsing.symbol_end_pos()), (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())) )
# 793 "parser.ml"
               : (Lexing.position * Lexing.position) Exprs.program))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Lexing.position * Lexing.position) Exprs.program)
;;
