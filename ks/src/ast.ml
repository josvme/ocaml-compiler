type op = Add | Mul | Div | Sub | Assign | Greater | Geq | And | Or | Leq | Less | Neq | Equal

type typ = Bool | Int | Unit

type unop = Neg | Not

type bind = typ * string

(* mutually recursive types and hence and *)
type func_decl = {
   ftype: typ;
   fname: string;
   formals: bind list;
   locals: bind list;
   body: expr; (* We have expr list *)
}
and 
expr = 
    Literal of int
  | BoolLit of bool
  | Call of string * expr list
  | Assign of string * expr
  | Id of string
  | Unop of unop * expr
  | BinOp of expr * op * expr
  | If of expr * expr * expr
  | For of expr * expr
  | Noexpr (* Required to handle empty cases *)
  | Return of expr
  | ExprList of expr list (* No idea how this will turn out to be *)
  | Func of func_decl
  | Spawn of func_decl
  | Send of int * func_decl
  | Receive of func_decl
