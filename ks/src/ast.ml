type op = Add | Mul | Div | Sub | Assign | Greater | Geq | And | Or | Leq | Less | Neq | Equal

type typ = Bool | Int | Unit

type unop = Neg | Not

type bind = typ * string

(* mutually recursive types and hence and *)
type func_decl = {
   ftype: typ;
   fname: string;
   formals: bind list;
   body: expr; (* We have expr list *)
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
  | If of expr * expr * expr
  | For of expr * expr
  | While of expr * expr
  | Noexpr (* Required to handle empty cases *)
  | Return of expr
  | Spawn of func_decl
  | Send of int * func_decl
  | Receive of func_decl
  | ExprBlock of expr list

  (*| ExprList of expr list, No idea how this will turn out to be *)
type program = bind list * func_decl list