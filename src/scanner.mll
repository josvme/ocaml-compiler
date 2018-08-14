{open Parser}

rule token = 
    parse [' ' '\t' '\r' '\n] {token lexbuf}
        | '+' {PLUS}
        | '-' {MINUS}
        | '*' {TIMES}
        | '/' {DIVIDE}
        | "if" {IF}
        | letter (letter | digit | '_')* as id {ID(id)}
        | "/*" {comment lexbuf}
        | ['0'-'9']+ as lit {LITERAL(int_of_string lit)}
        | eof  {EOF}

and comment = 
    parse "*/" {token lexbuf}
        | _ {comment lexbuf} (* this is pretty cool as it constanly calls comment until we reach */ *)