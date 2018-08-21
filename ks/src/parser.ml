type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | NEWLINE
  | COLON
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | RECV
  | SPAWN
  | SEND
  | FUNCDEF
  | LITERAL of (int)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
 open Ast 
# 47 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* NEWLINE *);
  264 (* COLON *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* ASSIGN *);
  270 (* NOT *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* LT *);
  274 (* LEQ *);
  275 (* GT *);
  276 (* GEQ *);
  277 (* TRUE *);
  278 (* FALSE *);
  279 (* AND *);
  280 (* OR *);
  281 (* RETURN *);
  282 (* IF *);
  283 (* ELSE *);
  284 (* FOR *);
  285 (* WHILE *);
  286 (* INT *);
  287 (* BOOL *);
  288 (* VOID *);
  289 (* RECV *);
  290 (* SPAWN *);
  291 (* SEND *);
  292 (* FUNCDEF *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  293 (* LITERAL *);
  294 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\005\000\005\000\008\000\
\008\000\006\000\006\000\006\000\003\000\009\000\009\000\009\000\
\007\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\003\000\000\000\004\000\003\000\
\003\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\002\000\002\000\003\000\004\000\003\000\005\000\
\007\000\005\000\005\000\002\000\003\000\002\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\051\000\000\000\010\000\011\000\012\000\000\000\
\001\000\003\000\004\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\000\000\014\000\005\000\000\000\000\000\017\000\
\000\000\000\000\019\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\018\000\000\000\034\000\000\000\000\000\
\035\000\036\000\000\000\000\000\000\000\000\000\046\000\044\000\
\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\039\000\015\000\000\000\000\000\000\000\045\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\038\000\000\000\000\000\042\000\043\000\000\000\000\000\
\041\000"

let yydgoto = "\002\000\
\003\000\004\000\010\000\046\000\017\000\012\000\029\000\019\000\
\030\000\047\000\080\000\081\000"

let yysindex = "\019\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\222\254\
\000\000\000\000\000\000\247\254\040\255\051\255\240\254\000\000\
\055\255\021\255\067\255\081\255\000\000\240\254\240\254\056\255\
\085\255\000\000\091\255\000\000\000\000\041\255\070\255\000\000\
\070\255\070\255\000\000\000\000\070\255\095\255\099\255\100\255\
\074\255\074\255\077\255\000\000\011\255\000\000\079\000\143\255\
\000\000\000\000\097\000\070\255\070\255\070\255\000\000\000\000\
\074\255\070\255\070\255\000\000\070\255\070\255\070\255\070\255\
\070\255\070\255\070\255\070\255\070\255\070\255\070\255\070\255\
\000\000\000\000\161\255\179\255\197\255\000\000\113\000\108\255\
\113\255\113\000\036\255\036\255\000\000\000\000\156\000\156\000\
\218\255\218\255\218\255\218\255\144\000\129\000\091\255\091\255\
\091\255\000\000\070\255\093\255\000\000\000\000\113\000\091\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\124\255\000\000\
\000\000\000\000\130\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\106\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\135\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\082\255\000\000\
\136\255\080\255\216\255\235\255\000\000\000\000\002\255\004\000\
\254\255\023\000\042\000\061\000\058\255\047\255\000\000\000\000\
\000\000\000\000\000\000\125\255\000\000\000\000\087\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\003\000\000\000\034\000\171\255\000\000\
\000\000\225\255\000\000\000\000"

let yytablesize = 432
let yytable = "\048\000\
\009\000\049\000\050\000\013\000\026\000\051\000\011\000\026\000\
\026\000\100\000\101\000\102\000\058\000\005\000\006\000\007\000\
\026\000\026\000\105\000\001\000\075\000\076\000\077\000\059\000\
\026\000\026\000\079\000\082\000\014\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\015\000\031\000\055\000\056\000\032\000\063\000\064\000\
\018\000\033\000\033\000\016\000\033\000\033\000\034\000\024\000\
\025\000\020\000\021\000\078\000\032\000\035\000\036\000\032\000\
\032\000\037\000\038\000\103\000\039\000\040\000\033\000\031\000\
\022\000\041\000\042\000\043\000\008\000\044\000\045\000\033\000\
\032\000\032\000\037\000\034\000\049\000\037\000\037\000\049\000\
\023\000\050\000\035\000\036\000\050\000\026\000\028\000\038\000\
\052\000\039\000\040\000\027\000\053\000\054\000\041\000\042\000\
\043\000\008\000\044\000\045\000\021\000\008\000\098\000\021\000\
\021\000\057\000\021\000\021\000\021\000\021\000\099\000\104\000\
\021\000\021\000\021\000\021\000\021\000\021\000\006\000\040\000\
\021\000\021\000\040\000\040\000\007\000\040\000\040\000\040\000\
\040\000\047\000\048\000\040\000\040\000\040\000\040\000\040\000\
\040\000\073\000\000\000\040\000\040\000\000\000\000\000\061\000\
\062\000\063\000\064\000\000\000\000\000\065\000\066\000\067\000\
\068\000\069\000\070\000\095\000\000\000\071\000\072\000\000\000\
\000\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\096\000\000\000\071\000\
\072\000\000\000\000\000\061\000\062\000\063\000\064\000\000\000\
\000\000\065\000\066\000\067\000\068\000\069\000\070\000\097\000\
\000\000\071\000\072\000\000\000\000\000\061\000\062\000\063\000\
\064\000\000\000\000\000\065\000\066\000\067\000\068\000\069\000\
\070\000\000\000\022\000\071\000\072\000\022\000\022\000\000\000\
\022\000\022\000\061\000\062\000\063\000\064\000\022\000\022\000\
\022\000\022\000\022\000\022\000\000\000\023\000\022\000\022\000\
\023\000\023\000\000\000\023\000\023\000\000\000\000\000\000\000\
\000\000\023\000\023\000\023\000\023\000\023\000\023\000\000\000\
\028\000\023\000\023\000\028\000\028\000\000\000\027\000\000\000\
\000\000\027\000\027\000\000\000\028\000\028\000\028\000\028\000\
\028\000\028\000\027\000\027\000\028\000\028\000\000\000\000\000\
\000\000\029\000\027\000\027\000\029\000\029\000\005\000\006\000\
\007\000\000\000\000\000\000\000\008\000\029\000\029\000\029\000\
\029\000\029\000\029\000\000\000\030\000\029\000\029\000\030\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\030\000\030\000\030\000\030\000\030\000\000\000\031\000\
\030\000\030\000\031\000\031\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\031\000\031\000\031\000\031\000\
\031\000\000\000\000\000\031\000\031\000\060\000\000\000\061\000\
\062\000\063\000\064\000\000\000\000\000\065\000\066\000\067\000\
\068\000\069\000\070\000\000\000\000\000\071\000\072\000\074\000\
\000\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\000\000\000\000\071\000\
\072\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\000\000\000\000\071\000\
\072\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\000\000\000\000\071\000\
\061\000\062\000\063\000\064\000\000\000\000\000\065\000\066\000\
\067\000\068\000\069\000\070\000\061\000\062\000\063\000\064\000\
\000\000\000\000\000\000\000\000\067\000\068\000\069\000\070\000"

let yycheck = "\031\000\
\000\000\033\000\034\000\038\001\003\001\037\000\004\000\006\001\
\007\001\095\000\096\000\097\000\002\001\030\001\031\001\032\001\
\015\001\016\001\104\000\001\000\052\000\053\000\054\000\013\001\
\023\001\024\001\058\000\059\000\038\001\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\002\001\002\001\041\000\042\000\005\001\011\001\012\001\
\015\000\003\001\010\001\001\001\006\001\007\001\014\001\022\000\
\023\000\003\001\038\001\057\000\003\001\021\001\022\001\006\001\
\007\001\025\001\026\001\099\000\028\001\029\001\024\001\002\001\
\006\001\033\001\034\001\035\001\036\001\037\001\038\001\010\001\
\023\001\024\001\003\001\014\001\003\001\006\001\007\001\006\001\
\008\001\003\001\021\001\022\001\006\001\038\001\004\001\026\001\
\002\001\028\001\029\001\015\001\002\001\002\001\033\001\034\001\
\035\001\036\001\037\001\038\001\003\001\036\001\003\001\006\001\
\007\001\037\001\009\001\010\001\011\001\012\001\006\001\027\001\
\015\001\016\001\017\001\018\001\019\001\020\001\003\001\003\001\
\023\001\024\001\006\001\007\001\003\001\009\001\010\001\011\001\
\012\001\003\001\003\001\015\001\016\001\017\001\018\001\019\001\
\020\001\003\001\255\255\023\001\024\001\255\255\255\255\009\001\
\010\001\011\001\012\001\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\003\001\255\255\023\001\024\001\255\255\
\255\255\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\003\001\255\255\023\001\
\024\001\255\255\255\255\009\001\010\001\011\001\012\001\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\003\001\
\255\255\023\001\024\001\255\255\255\255\009\001\010\001\011\001\
\012\001\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\003\001\023\001\024\001\006\001\007\001\255\255\
\009\001\010\001\009\001\010\001\011\001\012\001\015\001\016\001\
\017\001\018\001\019\001\020\001\255\255\003\001\023\001\024\001\
\006\001\007\001\255\255\009\001\010\001\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\003\001\023\001\024\001\006\001\007\001\255\255\003\001\255\255\
\255\255\006\001\007\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\015\001\016\001\023\001\024\001\255\255\255\255\
\255\255\003\001\023\001\024\001\006\001\007\001\030\001\031\001\
\032\001\255\255\255\255\255\255\036\001\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\003\001\023\001\024\001\006\001\
\007\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\003\001\
\023\001\024\001\006\001\007\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\255\255\023\001\024\001\007\001\255\255\009\001\
\010\001\011\001\012\001\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\023\001\024\001\007\001\
\255\255\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\024\001\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\024\001\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\009\001\010\001\011\001\012\001\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  NEWLINE\000\
  COLON\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  VOID\000\
  RECV\000\
  SPAWN\000\
  SEND\000\
  FUNCDEF\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 30 "parser.mly"
            ( _1 )
# 332 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                ([], [])
# 338 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 38 "parser.mly"
                ((_2 :: fst _1), snd _1)
# 346 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 39 "parser.mly"
                    (fst _1, (_2 :: snd _1))
# 354 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr_list) in
    Obj.repr(
# 42 "parser.mly"
                                                                ( { 
    ftype = _7;
    fname = _2;
    formals = _4;
    body = _9;
  } )
# 369 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( [] )
# 375 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 51 "parser.mly"
                 ( List.rev _1 )
# 382 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
         ( [(_1, _2)] )
# 390 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                              ( (_3, _4) :: _1)
# 399 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
      ( Int )
# 405 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
         ( Bool )
# 411 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
         ( Void )
# 417 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 63 "parser.mly"
              ( (_1, _2) )
# 425 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                ([])
# 431 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                  ( Return(_3) :: _1)
# 439 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                           (_2 :: _1)
# 447 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 71 "parser.mly"
                          ( ExprBlock(_2) )
# 454 "parser.ml"
               : 'b_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
                     ( Literal(_1) )
# 461 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                     ( BoolLit(true) )
# 467 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                     ( BoolLit(false) )
# 473 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                     ( Id(_1) )
# 480 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Binop(_1, Mul,  _3) )
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 520 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 528 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 536 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                     ( Binop(_1, And,   _3) )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                     ( Binop(_1, Or,    _3) )
# 576 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 90 "parser.mly"
                    ( Func(_1) )
# 583 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                         ( Unop(Neg, _2) )
# 590 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                     ( Unop(Not, _2) )
# 597 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                     ( Assign(_1, _3) )
# 605 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 94 "parser.mly"
                                 ( Call(_1, _3) )
# 613 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                       ( _2 )
# 620 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr_list) in
    Obj.repr(
# 96 "parser.mly"
                                                   ( If(_3, _5, ExprBlock([])) )
# 628 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr_list) in
    Obj.repr(
# 97 "parser.mly"
                                                       ( If(_3, _5, _7) )
# 637 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr_list) in
    Obj.repr(
# 98 "parser.mly"
                                       ( For(_3, _5) )
# 645 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr_list) in
    Obj.repr(
# 99 "parser.mly"
                                         ( While(_3, _5) )
# 653 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 100 "parser.mly"
                    ( Spawn(_2) )
# 660 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 101 "parser.mly"
                           ( Send(_2, _3) )
# 668 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 102 "parser.mly"
                   ( Receive(_2) )
# 675 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                  ( [] )
# 681 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 106 "parser.mly"
                  ( List.rev _1 )
# 688 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                            ( [_1] )
# 695 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                            ( _3 :: _1 )
# 703 "parser.ml"
               : 'actuals_list))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
