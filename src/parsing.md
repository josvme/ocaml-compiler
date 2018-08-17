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
