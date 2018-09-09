open Parser

let to_string = function 
  | EOF -> "EOF"
  | LSQUARE -> "LSQUARE"
  | RSQUARE  -> "RSQUARE"
  | NEWLINE  -> "NEWLINE"
  | FUNCDEF  -> "FUNCDEF"
  | LITERAL(x) -> "LITERAL" 
  | ID(x) -> "ID" 
  | LPAREN  -> "LPAREN"
  | RPAREN  -> "RPAREN"
  | LBRACE  -> "LBRACE"
  | RBRACE  -> "RBRACE"
  | SEMI  -> "SEMI"
  | COLON  -> "COLON"
  | COMMA  -> "COMMA"
  | SEND  -> "SEND"
  | RECV  -> "RECV"
  | SPAWN  -> "SPAWN"
  | PLUS  -> "PLUS"
  | MINUS  -> "MINUS"
  | TIMES  -> "TIMES"
  | DIVIDE  -> "DIVIDE"
  | ASSIGN  -> "ASSIGN"
  | EQ  -> "EQ"
  | NEQ  -> "NEQ"
  | LT  -> "LT"
  | LEQ  -> "LEQ"
  | GT  -> "GT"
  | GEQ  -> "GEQ"
  | AND  -> "AND"
  | OR  -> "OR"
  | NOT  -> "NOT"
  | IF  -> "IF"
  | ELSE  -> "ELSE"
  | FOR  -> "FOR"
  | WHILE  -> "WHILE"
  | RETURN  -> "RETURN"
  | STRUCT  -> "STRUCT"
  | INT  -> "INT"
  | BOOL  -> "BOOL" 
  | UNIT  -> "UNIT"
  | TRUE  -> "TRUE"
  | FALSE  -> "FALSE"
  | STR(x) -> "STR"

let token_to_strings lexbuf =
  let rec loop acc =  function
    | EOF ->  to_string EOF :: acc |> List.rev
    | x   ->  loop (to_string x :: acc) (Scanner.token lexbuf)
  in
  loop [] (Scanner.token lexbuf) 
  |> String.concat " " 
  |> print_endline