type op = Add | Mul | Div | Sub | Assign | Greater | Geq | And | Or | Leq | Less | Neq | Equal

type typ = Bool | Int | Unit

type unop = Neg | Not

type bind = typ * string

(* mutually recursive types and hence and *)
type func_decl = {
   ftype: typ;
   fname: string;
   formals: bind list;
   body: expr list; (* We have expr list *)
}
and 
expr = 
    Literal of int
  | BoolLit of bool
  | Id of string
  | BinOp of expr * op * expr
  | Func of func_decl
  | Unop of unop * expr
  | Assign of string * expr
  | Call of string * expr list
  | If of expr * expr list * expr list
  | For of expr * expr list
  | While of expr * expr list
  | Noexpr (* Required to handle empty cases *)
  | Return of expr
  | Spawn of func_decl
  | Send of int * func_decl
  | Receive of func_decl

  (*| ExprList of expr list, No idea how this will turn out to be *)
type program = bind list * func_decl list

type exprList = 
  ExprBlock of expr list

(*
Working Input
FUNCDEF ID LPAREN RPAREN COLON INT EQ LBRACE RETURN ID PLUS ID NEWLINE RBRACE EOF
*)