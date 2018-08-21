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
\008\000\006\000\006\000\006\000\009\000\009\000\003\000\010\000\
\010\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\011\000\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\000\000\002\000\003\000\000\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\002\000\002\000\003\000\004\000\003\000\005\000\
\007\000\005\000\005\000\002\000\002\000\003\000\002\000\003\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\053\000\000\000\010\000\011\000\012\000\000\000\
\001\000\003\000\004\000\000\000\000\000\000\000\000\000\015\000\
\000\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\
\000\000\009\000\000\000\000\000\000\000\000\000\000\000\019\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\000\000\034\000\000\000\000\000\000\000\035\000\036\000\
\000\000\000\000\000\000\000\000\047\000\045\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\039\000\048\000\000\000\
\000\000\000\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\025\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\038\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\003\000\004\000\010\000\043\000\017\000\012\000\044\000\019\000\
\000\000\000\000\077\000\078\000"

let yysindex = "\009\000\
\000\000\000\000\000\000\169\000\000\000\000\000\000\000\232\254\
\000\000\000\000\000\000\014\255\055\255\057\255\052\255\000\000\
\066\255\038\255\072\255\078\255\000\000\052\255\052\255\049\255\
\075\255\000\000\071\255\071\255\071\255\071\255\071\255\000\000\
\000\000\071\255\087\255\089\255\092\255\059\255\059\255\061\255\
\000\000\254\254\000\000\137\001\107\255\189\255\000\000\000\000\
\044\255\071\255\071\255\071\255\000\000\000\000\059\255\071\255\
\071\255\071\255\071\255\071\255\071\255\071\255\071\255\071\255\
\071\255\071\255\071\255\071\255\071\255\000\000\000\000\126\255\
\144\255\167\255\000\000\044\255\098\255\096\255\044\255\020\255\
\020\255\000\000\000\000\137\001\137\001\249\254\249\254\249\254\
\249\254\007\255\226\255\071\255\071\255\071\255\000\000\071\255\
\205\255\044\255\044\255\044\255\071\255\044\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\100\255\000\000\
\000\000\000\000\109\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\180\000\000\000\000\000\000\000\000\000\
\029\001\000\000\000\000\000\000\000\000\000\000\000\000\110\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\255\000\000\111\255\045\001\033\000\
\065\000\000\000\000\000\203\000\226\000\088\000\111\000\134\000\
\157\000\240\000\022\001\000\000\000\000\000\000\000\000\000\000\
\109\001\061\001\077\001\074\255\000\000\100\001"

let yygindex = "\000\000\
\000\000\000\000\000\000\011\000\000\000\254\255\234\255\000\000\
\000\000\000\000\000\000\000\000"

let yytablesize = 669
let yytable = "\056\000\
\021\000\058\000\059\000\060\000\061\000\045\000\046\000\047\000\
\048\000\001\000\057\000\049\000\018\000\013\000\011\000\058\000\
\059\000\060\000\061\000\024\000\025\000\062\000\063\000\064\000\
\065\000\066\000\067\000\072\000\073\000\074\000\060\000\061\000\
\022\000\076\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\051\000\
\053\000\054\000\051\000\014\000\058\000\059\000\060\000\061\000\
\015\000\016\000\062\000\063\000\064\000\065\000\066\000\067\000\
\023\000\075\000\068\000\069\000\020\000\097\000\098\000\099\000\
\028\000\100\000\029\000\021\000\052\000\022\000\102\000\052\000\
\030\000\005\000\006\000\007\000\031\000\023\000\026\000\028\000\
\050\000\027\000\051\000\032\000\033\000\052\000\008\000\034\000\
\035\000\055\000\036\000\037\000\095\000\096\000\006\000\038\000\
\039\000\040\000\008\000\041\000\042\000\070\000\029\000\007\000\
\049\000\050\000\000\000\058\000\059\000\060\000\061\000\000\000\
\000\000\062\000\063\000\064\000\065\000\066\000\067\000\000\000\
\092\000\068\000\069\000\000\000\000\000\030\000\058\000\059\000\
\060\000\061\000\000\000\000\000\062\000\063\000\064\000\065\000\
\066\000\067\000\093\000\000\000\068\000\069\000\000\000\000\000\
\058\000\059\000\060\000\061\000\031\000\000\000\062\000\063\000\
\064\000\065\000\066\000\067\000\000\000\000\000\068\000\069\000\
\009\000\094\000\000\000\000\000\000\000\000\000\000\000\058\000\
\059\000\060\000\061\000\005\000\000\000\062\000\063\000\064\000\
\065\000\066\000\067\000\000\000\000\000\068\000\069\000\000\000\
\000\000\071\000\000\000\000\000\000\000\058\000\059\000\060\000\
\061\000\000\000\026\000\062\000\063\000\064\000\065\000\066\000\
\067\000\000\000\000\000\068\000\069\000\058\000\059\000\060\000\
\061\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\027\000\000\000\068\000\069\000\000\000\000\000\101\000\
\000\000\000\000\058\000\059\000\060\000\061\000\000\000\032\000\
\062\000\063\000\064\000\065\000\066\000\067\000\000\000\000\000\
\068\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\000\000\021\000\021\000\000\000\
\000\000\021\000\021\000\021\000\021\000\000\000\000\000\021\000\
\021\000\021\000\021\000\021\000\021\000\033\000\000\000\021\000\
\021\000\000\000\000\000\021\000\044\000\000\000\021\000\021\000\
\021\000\000\000\000\000\022\000\021\000\022\000\022\000\000\000\
\000\000\022\000\022\000\000\000\037\000\000\000\000\000\022\000\
\022\000\022\000\022\000\022\000\022\000\000\000\000\000\022\000\
\022\000\000\000\000\000\022\000\042\000\000\000\022\000\022\000\
\022\000\000\000\000\000\023\000\022\000\023\000\023\000\000\000\
\000\000\023\000\023\000\000\000\043\000\000\000\000\000\023\000\
\023\000\023\000\023\000\023\000\023\000\000\000\000\000\023\000\
\023\000\000\000\028\000\023\000\028\000\028\000\023\000\023\000\
\023\000\000\000\000\000\041\000\023\000\000\000\028\000\028\000\
\028\000\028\000\028\000\028\000\040\000\000\000\028\000\028\000\
\000\000\029\000\028\000\029\000\029\000\028\000\028\000\028\000\
\000\000\000\000\000\000\028\000\000\000\029\000\029\000\029\000\
\029\000\029\000\029\000\000\000\000\000\029\000\029\000\000\000\
\030\000\029\000\030\000\030\000\029\000\029\000\029\000\000\000\
\000\000\000\000\029\000\000\000\030\000\030\000\030\000\030\000\
\030\000\030\000\000\000\000\000\030\000\030\000\000\000\031\000\
\030\000\031\000\031\000\030\000\030\000\030\000\000\000\000\000\
\000\000\030\000\000\000\031\000\031\000\031\000\031\000\031\000\
\031\000\000\000\000\000\031\000\031\000\000\000\005\000\031\000\
\005\000\005\000\031\000\031\000\031\000\000\000\000\000\000\000\
\031\000\000\000\005\000\005\000\000\000\000\000\005\000\006\000\
\007\000\000\000\005\000\005\000\008\000\026\000\005\000\026\000\
\026\000\005\000\005\000\005\000\000\000\000\000\000\000\005\000\
\000\000\026\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\026\000\000\000\027\000\026\000\027\000\027\000\
\026\000\026\000\026\000\000\000\000\000\000\000\026\000\000\000\
\027\000\027\000\032\000\000\000\032\000\032\000\000\000\000\000\
\027\000\027\000\000\000\000\000\027\000\000\000\000\000\027\000\
\027\000\027\000\000\000\000\000\000\000\027\000\032\000\032\000\
\000\000\000\000\032\000\000\000\000\000\032\000\032\000\032\000\
\000\000\000\000\000\000\032\000\000\000\000\000\000\000\000\000\
\033\000\000\000\033\000\033\000\000\000\000\000\000\000\044\000\
\000\000\044\000\044\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\000\000\000\037\000\
\033\000\037\000\037\000\033\000\033\000\033\000\000\000\044\000\
\000\000\033\000\044\000\044\000\044\000\000\000\000\000\042\000\
\044\000\042\000\042\000\000\000\000\000\000\000\000\000\037\000\
\000\000\000\000\037\000\037\000\037\000\000\000\000\000\043\000\
\037\000\043\000\043\000\000\000\000\000\000\000\000\000\042\000\
\000\000\000\000\042\000\042\000\042\000\000\000\000\000\000\000\
\042\000\000\000\000\000\000\000\000\000\000\000\041\000\043\000\
\041\000\041\000\043\000\043\000\043\000\000\000\000\000\040\000\
\043\000\040\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\000\000\000\
\000\000\041\000\041\000\041\000\000\000\000\000\000\000\041\000\
\000\000\000\000\040\000\040\000\040\000\000\000\000\000\000\000\
\040\000\058\000\059\000\060\000\061\000\000\000\000\000\000\000\
\000\000\064\000\065\000\066\000\067\000"

let yycheck = "\002\001\
\000\000\009\001\010\001\011\001\012\001\028\000\029\000\030\000\
\031\000\001\000\013\001\034\000\015\000\038\001\004\000\009\001\
\010\001\011\001\012\001\022\000\023\000\015\001\016\001\017\001\
\018\001\019\001\020\001\050\000\051\000\052\000\011\001\012\001\
\000\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\003\001\
\038\000\039\000\006\001\038\001\009\001\010\001\011\001\012\001\
\002\001\001\001\015\001\016\001\017\001\018\001\019\001\020\001\
\000\000\055\000\023\001\024\001\003\001\092\000\093\000\094\000\
\002\001\096\000\004\001\038\001\003\001\006\001\101\000\006\001\
\010\001\030\001\031\001\032\001\014\001\008\001\038\001\000\000\
\002\001\015\001\002\001\021\001\022\001\002\001\036\001\025\001\
\026\001\037\001\028\001\029\001\003\001\006\001\003\001\033\001\
\034\001\035\001\036\001\037\001\038\001\003\001\000\000\003\001\
\003\001\003\001\255\255\009\001\010\001\011\001\012\001\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\003\001\023\001\024\001\255\255\255\255\000\000\009\001\010\001\
\011\001\012\001\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\003\001\255\255\023\001\024\001\255\255\255\255\
\009\001\010\001\011\001\012\001\000\000\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\255\255\255\255\023\001\024\001\
\000\000\003\001\255\255\255\255\255\255\255\255\255\255\009\001\
\010\001\011\001\012\001\000\000\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\023\001\024\001\255\255\
\255\255\005\001\255\255\255\255\255\255\009\001\010\001\011\001\
\012\001\255\255\000\000\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\255\255\023\001\024\001\009\001\010\001\011\001\
\012\001\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\000\000\255\255\023\001\024\001\255\255\255\255\027\001\
\255\255\255\255\009\001\010\001\011\001\012\001\255\255\000\000\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\255\255\
\023\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\005\001\006\001\255\255\
\255\255\009\001\010\001\011\001\012\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\000\000\255\255\023\001\
\024\001\255\255\255\255\027\001\000\000\255\255\030\001\031\001\
\032\001\255\255\255\255\003\001\036\001\005\001\006\001\255\255\
\255\255\009\001\010\001\255\255\000\000\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\024\001\255\255\255\255\027\001\000\000\255\255\030\001\031\001\
\032\001\255\255\255\255\003\001\036\001\005\001\006\001\255\255\
\255\255\009\001\010\001\255\255\000\000\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\024\001\255\255\003\001\027\001\005\001\006\001\030\001\031\001\
\032\001\255\255\255\255\000\000\036\001\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\000\000\255\255\023\001\024\001\
\255\255\003\001\027\001\005\001\006\001\030\001\031\001\032\001\
\255\255\255\255\255\255\036\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\255\255\023\001\024\001\255\255\
\003\001\027\001\005\001\006\001\030\001\031\001\032\001\255\255\
\255\255\255\255\036\001\255\255\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\023\001\024\001\255\255\003\001\
\027\001\005\001\006\001\030\001\031\001\032\001\255\255\255\255\
\255\255\036\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\255\255\023\001\024\001\255\255\003\001\027\001\
\005\001\006\001\030\001\031\001\032\001\255\255\255\255\255\255\
\036\001\255\255\015\001\016\001\255\255\255\255\030\001\031\001\
\032\001\255\255\023\001\024\001\036\001\003\001\027\001\005\001\
\006\001\030\001\031\001\032\001\255\255\255\255\255\255\036\001\
\255\255\015\001\016\001\255\255\255\255\255\255\255\255\255\255\
\255\255\023\001\024\001\255\255\003\001\027\001\005\001\006\001\
\030\001\031\001\032\001\255\255\255\255\255\255\036\001\255\255\
\015\001\016\001\003\001\255\255\005\001\006\001\255\255\255\255\
\023\001\024\001\255\255\255\255\027\001\255\255\255\255\030\001\
\031\001\032\001\255\255\255\255\255\255\036\001\023\001\024\001\
\255\255\255\255\027\001\255\255\255\255\030\001\031\001\032\001\
\255\255\255\255\255\255\036\001\255\255\255\255\255\255\255\255\
\003\001\255\255\005\001\006\001\255\255\255\255\255\255\003\001\
\255\255\005\001\006\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\024\001\255\255\003\001\
\027\001\005\001\006\001\030\001\031\001\032\001\255\255\027\001\
\255\255\036\001\030\001\031\001\032\001\255\255\255\255\003\001\
\036\001\005\001\006\001\255\255\255\255\255\255\255\255\027\001\
\255\255\255\255\030\001\031\001\032\001\255\255\255\255\003\001\
\036\001\005\001\006\001\255\255\255\255\255\255\255\255\027\001\
\255\255\255\255\030\001\031\001\032\001\255\255\255\255\255\255\
\036\001\255\255\255\255\255\255\255\255\255\255\003\001\027\001\
\005\001\006\001\030\001\031\001\032\001\255\255\255\255\003\001\
\036\001\005\001\006\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\027\001\255\255\
\255\255\030\001\031\001\032\001\255\255\255\255\255\255\036\001\
\255\255\255\255\030\001\031\001\032\001\255\255\255\255\255\255\
\036\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001"

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
# 389 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                ([], [])
# 395 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 38 "parser.mly"
                ((_2 :: fst _1), snd _1)
# 403 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 39 "parser.mly"
                    (fst _1, (_2 :: snd _1))
# 411 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                                                         ( { 
    ftype = _7;
    fname = _2;
    formals = _4;
    body = _9;
  } )
# 426 "parser.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                ( [] )
# 432 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 51 "parser.mly"
                 ( List.rev _1 )
# 439 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
         ( [(_1, _2)] )
# 447 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                              ( (_3, _4) :: _1)
# 456 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
      ( Int )
# 462 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
         ( Bool )
# 468 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
         ( Void )
# 474 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                ( [] )
# 480 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 64 "parser.mly"
                     ( _2 :: _1 )
# 488 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "parser.mly"
              ( (_1, _2) )
# 496 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                  ( Noexpr )
# 502 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                  ( _1 )
# 509 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
                     ( Literal(_1) )
# 516 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                     ( BoolLit(true) )
# 522 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                     ( BoolLit(false) )
# 528 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                     ( Id(_1) )
# 535 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 543 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 551 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Binop(_1, Mul,  _3) )
# 559 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 567 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 575 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 583 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 591 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 599 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 615 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                     ( Binop(_1, And,   _3) )
# 623 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                     ( Binop(_1, Or,    _3) )
# 631 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 90 "parser.mly"
                    ( Func(_1) )
# 638 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                         ( Unop(Neg, _2) )
# 645 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                     ( Unop(Not, _2) )
# 652 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                     ( Assign(_1, _3) )
# 660 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 94 "parser.mly"
                                 ( Call(_1, _3) )
# 668 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                       ( _2 )
# 675 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                                            ( If(_3, _5, ExprBlock([])) )
# 683 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                                         ( If(_3, _5, _7) )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                                ( For(_3, _5) )
# 700 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                                  ( While(_3, _5) )
# 708 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                ( Return(_2) )
# 715 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 101 "parser.mly"
                    ( Spawn(_2) )
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 102 "parser.mly"
                           ( Send(_2, _3) )
# 730 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 103 "parser.mly"
                   ( Receive(_2) )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                       ( ExprBlock(_2) )
# 744 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
                  ( [] )
# 750 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 108 "parser.mly"
                  ( List.rev _1 )
# 757 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                            ( [_1] )
# 764 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                            ( _3 :: _1 )
# 772 "parser.ml"
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
