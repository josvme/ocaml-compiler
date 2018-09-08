## Lexing and Parsing

lexer
rule keyword is used to denote the rule and can take any name, normally we use token.
rule matches whatever regular expression and executes the action defineds. {} can be used to put ocaml code there. 

lexbuf is bound to the current lexer buffer. 
https://troydm.github.io/blog/2014/11/14/rewriting-micro-compiler-in-ocaml-using-ocamllex-and-ocamlyacc/

ocamllex generates a .ml file from .mll file for us and we can compile and run it as a normal ml file.

.mly
%token <int> LITERAL is the information int, the lexer will give us. 

Convention is to use Uppercase for tokens/terminals and lower case for non-terminal.

## Testing grammar
 menhir --interpret --interpret-show-cst parser.mly

INT ID LPAREN RPAREN LBRACE ID LPAREN LITERAL RPAREN SEMI RBRACE EOF
INT ID LPAREN RPAREN LBRACE INT ID SEMI ID EQ LITERAL SEMI RBRACE EOF

.mly
The first section of the file is for declarations, including token and type specifications, precedence directives, and other output directives; and the second section is for specifying the grammar of the language to be parsed.   

We'll start by declaring the list of tokens. A token is declared using the syntax %token <type>uid, where the <type> is optional and uid is a capitalized identifier.

%token <string> STRING
%token NULL
%token COLON

The <type> specifications mean that a token carries a value. The STRING token carries a string value. The remaining tokens, such as TRUE, FALSE, or the punctuation, aren't associated with any value, and so we can omit the <type> specification.

We'll start describing the JSON grammar by declaring the start symbol to be the non-terminal symbol prog, and by declaring that when parsed, a prog value should be converted into an OCaml value of type Json.value option. We then end the declaration section of the parser with a %%:

%start <Json.value option> prog
%%


rev_object_fields:
  | (* empty *) { [] }
  | obj = rev_object_fields; COMMA; k = ID; COLON; v = value
    { (k, v) :: obj }
  ;

menhir is left recursive and hence the grammar is more efficient if LR. So this is like recursively expanding and outputting the value.

## Lexer

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { TRUE }

Here in white and newline we recursively skip values, and returns the following token but others we don't. Also in lexer precedence is for the longest match and not in the order. So here white and newline doesn't return anything as they call themselves and pass over to others. 

For example, the first input trueX: 167 matches the regular expression "true" for four characters, and it matches id for five characters. The longer match wins, and the return value is ID "trueX".

Parsing functions take as arguments a lexical analyzer (a function from lexer buffers to tokens) and a lexer buffer, and return the semantic attribute of the corresponding entry point. Lexical analyzer functions are usually generated from a lexer specification by the ocamllex program. So if you define as tokens in .mly file, all these will be taken from lexer file. And the scanner.mll has a open Parser in the top. 

Working Input for menhir
FUNCDEF ID LPAREN RPAREN COLON INT ASSIGN LBRACE RETURN ID PLUS ID NEWLINE RBRACE EOF

Input for luttu

def abc(): int = { return (4+4) }

To debug the ocamlyacc

export OCAMLRUNPARAM='p'

It will print all the states.