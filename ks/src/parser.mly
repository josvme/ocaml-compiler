/* Ocamlyacc parser for Luttu */

%{
  open Ast
}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA NEWLINE
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID RECV SPAWN SEND FUNCDEF 
%token <int> LITERAL
%token <string> ID
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
  decls EOF { $1 }
/*
The program can have only declarations. And these declarations can be value / function declarations.
Now we will process them one by one
*/

decls:
  /* nothing */ {[], []} /* [(id, value)](value definitions) and [function definitions] */
  | decls vdecl {($2 :: fst $1), snd $1}
  | decls fdecl {fst $1, ($2 :: snd $1)}

fdecl:
  FUNCDEF ID LPAREN formals_opt RPAREN COLON typ EQ LBRACE vdecl_list expr RBRACE { { 
    ftype = $7;
    fname = $2;
    formals = $4;
    locals = List.rev $10;
    body = List.rev $11;
  } } 

formals_opt:
  /* nothing */ { [] }
  | formal_list { List.rev $1 }

formals_list:
  typ ID { [($1, $2)] }
  | formal_list COMMA typ ID { ($3, $4) :: $1}

typ:
  INT { Int }
  | BOOL { Bool }
  | VOID { Void }

vdecl_list:
  /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
  typ ID SEMI { ($1, $2) } /* (ID, value) pair */

expr:
  