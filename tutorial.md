## Ocaml Compiler

### Opam
opam is like stack. It install compiler and packages. 
https://opam.ocaml.org/doc/Usage.html


### Resources
http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/
https://dev.realworldocaml.org/toc.html

## ocamllex
The OCaml code for the rules has a parameter called lexbuf that defines the input, including the position in the input file, as well as the text that was matched by the regular expression.

## Setting up Ocaml
opam install merlin
opam install utop # A better shell

## Managing Associativity and Precedence
http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/syntax.pdf (from slide 72)

Most of the slide is about shift-reduce parsing.

## Haskell Value vs Types
https://www.reddit.com/r/haskell/comments/2u0m50/need_to_understand_type_constructor/
Haskell HKT type is seen in things like Functor/Monads.

## Basic Usage
; to sequence and ;; for declarations and expressions.

In ocaml = compares values and == compares pointers
(42, "A");; # A tuple
[1;23;5];; # A List

Concatenate with @
List.hd and List.tl
7 :: [];; # To cons

if 4=8 then 44 else 5;;

Local binding -> let a=5 in a + 5
Global binding -> let name = "F2"

Let is different from haskell, which doesn't allow mutation
let a = 4 in
let a = a + 2 in
let a = a
*
2 in
print_int a;;
12- : unit = ()
# a
;;
Unbound value a

This is a function
let square = fun y -> y*y;;

Closure like let is similar to Elixir, lambda, which is bound at definition and not like JS or python
# let a = 5;;
val a : int = 5
# let adda x = x + a;;
val adda : int -> int = <fun>
# let a = 10;;
val a : int = 10
# adda 0;;
- : int = 5

Binding is not assignment
let a = 5 in
let b x = a + x in
let a = 42 in
b 0;;
- : int = 5

let seems to be like a lambda internally. They are semantically same.  

# let a = 3 in a + 2;;
- : int = 5
# (fun a -> a + 2) 3;;
- : int = 5

By default, a name is not visible in its defining expression. 
# let fac n = if n < 2 then 1 else n
*
fac
(n-1);;
Unbound value fac

You need to use rec to make it visible. 
The and keyword allows for mutual recursion. 

# let rec fac n = if n < 2 then 1 else n * fac1 n and fac1 n = fac (n - 1);;

@ is list concatenation
:: is add element to beginning of list 
http://rigaux.org/language-study/syntax-across-languages-per-language/OCaml.html

type is recursive and mutually recursive ones can be defined with and. 


