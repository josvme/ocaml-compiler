/* Ocamlyacc parser for Luttu */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA NEWLINE COLON LSQUARE RSQUARE
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL UNIT RECV SPAWN SEND FUNCDEF STRUCT VAR STRING
%token <int> LITERAL
%token <string> ID
%token <string> STR 
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { List.rev $1 }
/*
The program can have only declarations. And these declarations can be value / function declarations.
Now we will process them one by one
*/

decls:
  /* nothing */ {[]}
  | decls vdecl {$2 :: $1}
  | decls func_decl {$2 :: $1}
  | decls NEWLINE { $1 }
  | NEWLINE { [Newline()] }

func_decl:
  FUNCDEF ID LPAREN formals_opt RPAREN COLON typ ASSIGN b_expr_list { Func { 
    ftype = $7;
    fname = $2;
    formals = $4;
    body = $9;
  } } 

struct_decl:
  STRUCT ID LBRACE struct_list RBRACE {
    $4
  }

type_id:
  typ ID { ($1, $2) }

struct_list:
   NEWLINE {[]}
  | struct_list type_id NEWLINE {$2 :: $1}

formals_opt:
  /* nothing */ { [] }
  | formals_list { List.rev $1 }

formals_list:
  typ ID { [($1, $2)] }
  | formals_list COMMA typ ID { ($3, $4) :: $1}

typ:
  INT { Int }
  | BOOL { Bool }
  | UNIT { Unit }
  | STRING { Str}

vdecl:
  typ ID SEMI { Var ($1, $2) } /* (ID, value) pair */

expr_list:
   NEWLINE {[]}
  | expr_list expr NEWLINE {$2 :: $1}
  | expr_list RETURN expr NEWLINE { Return($3) :: $1}
  
b_expr_list:
  LBRACE expr_list RBRACE { List.rev $2 }

expr:
    LITERAL          { Literal($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | STR              { Str($1) }
  | typ ID           { Var($1, $2) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mul,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | func_decl        { $1 }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | IF LPAREN expr RPAREN b_expr_list %prec NOELSE { If($3, $5, [Noexpr]) }
  | IF LPAREN expr RPAREN b_expr_list ELSE b_expr_list { If($3, $5, $7) }
  | FOR LPAREN expr RPAREN b_expr_list { For($3, $5) }
  | WHILE LPAREN expr RPAREN b_expr_list { While($3, $5) }
  | SPAWN func_decl { $2 }
  | SEND LITERAL func_decl { $3 }
  | RECV func_decl { $2 }
  | struct_decl { Struct $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
