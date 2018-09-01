type op = Add | Mul | Div | Sub | Greater | Geq | And | Or | Leq | Less | Neq | Equal

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
  | Binop of expr * op * expr
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
  | ExprList of expr list (* No idea how this will turn out to be *)
type program = bind list * func_decl list

(*
Working Input
FUNCDEF ID LPAREN RPAREN COLON INT ASSIGN LBRACE RETURN ID PLUS ID NEWLINE RBRACE EOF
*)

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | Receive(f) -> "Receive " ^ string_of_fdecl f
  | Send(i, f) -> "Send " ^ string_of_int i ^ string_of_fdecl f
  | Spawn(f) -> "Spawn " ^ string_of_fdecl f 
  | If(e, s, ([n])) -> "if (" ^ string_of_expr e ^ ")\n" ^ String.concat "\n" (List.map string_of_expr s) 
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
     String.concat "\n" (List.map string_of_expr s1)  ^ "else\n" ^ String.concat "\n" (List.map string_of_expr s1)
  | For(c, s) -> "for (" ^ string_of_expr c  ^ String.concat "\n" (List.map string_of_expr s)
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ String.concat "\n" (List.map string_of_expr s)
  | Func(f) -> string_of_fdecl f
  | Return(r) -> "Return " ^ string_of_expr r
  | ExprList (l) -> "ExprList " ^ String.concat ", " (List.map string_of_expr l)

and string_of_fdecl fdecl =
  string_of_typ fdecl.ftype ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_expr fdecl.body) ^
  "}\n"

and string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)