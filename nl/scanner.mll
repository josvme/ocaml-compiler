(* The left side is pattern matched and input is consumed for matching. *)

{ open Parser }

(* Everything except *)
let string_char = [^'"']*

rule token = parse
  [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
| '['      { LSQUARE }
| ']'      { RSQUARE }
| '\n'     { NEWLINE } (* new line *)
| "/*"     { comment lexbuf }           (* Comments *)
| "def"    { FUNCDEF }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| "send"   { SEND }
| "recv"   { RECV }
| "spawn"  { SPAWN }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "struct" { STRUCT }
| "string" { STRING }
| "bool"   { BOOL }
| "unit"   { UNIT }
| "true"   { TRUE }
| "false"  { FALSE }
| "var"    { VAR }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| '"' (string_char as content) '"' { STR (content) } (* String constant, no escaping as of now *)
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
