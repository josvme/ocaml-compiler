## Ocaml Compiler

### Opam
opam is like stack. It install compiler and packages. 
https://opam.ocaml.org/doc/Usage.html


### Resources
http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/
https://dev.realworldocaml.org/toc.html

## ocamllex
The OCaml code for the rules has a parameter called lexbuf that defines the input, including the position in the input file, as well as the text that was matched by the regular expression.

## Setting up Ocaml (ubuntu 17.04)
opam install merlin
opam install utop # A better shell
opam install ocp-indent # Document extractor
opam install menhir # A better yacc
sudo apt install llvm-3.8 m4
opam install llvm.4.8

## Setting up merlin
To add all the stuff to merlin use this script. 
https://gist.github.com/unhammer/c1ac7320141f09ac38e0

## Ocaml Tips
http://roscidus.com/blog/blog/2013/10/13/ocaml-tips/


## Utop
To get the type of something in Utop use #typeof -> #typeof "List" -> Notice the ""

## Things to see
Once you setup the config, restart vscode as new path needs to be loaded.

## Managing Associativity and Precedence
http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/syntax.pdf (from slide 72)

Most of the slide is about shift-reduce parsing.

## Haskell Value vs Types
https://www.reddit.com/r/haskell/comments/2u0m50/need_to_understand_type_constructor/
Haskell HKT type is seen in things like Functor/Monads.

## Basic Usage
; to sequence and ;; for declarations and expressions.
In Ocaml all types see to be small letters.

Ocaml float operations needs to add a . at the end like - is written as -. And it operates on float.

Tuples by (42, "S")
let p = (4, "D");;
val p : int * string = (4, "D")

In ocaml = compares values and == compares pointers
(42, "A");; # A tuple
[1;23;5];; # A List

Concatenate with @
List.hd and List.tl
7 :: [];; # To cons

Like haskell returns should be of same time in both branches
if 4=8 then 44 else 5;;

Local binding -> let a=5 in a + 5
Global binding -> let name = "F2"

Let is different from haskell, which doesn't allow mutation. This let actually binds in succession based on previous value.
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

This is an anonymous function defined using fun, otherwise use let.
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

A name is bound after the “in” clause of a “let.” If the name is re-bound, the binding takes effect after the “in.

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

You need to use rec to make it visible. The “rec” keyword makes a name visible to its definition. This only makes sense for functions.

The and keyword allows for mutual recursion. 

# let rec fac n = if n < 2 then 1 else n * fac1 n and fac1 n = fac (n - 1);;

function keyword can be used to make pattern matches.

@ is list concatenation
:: is add element to beginning of list 
http://rigaux.org/language-study/syntax-across-languages-per-language/OCaml.html

type is recursive and mutually recursive ones can be defined with and. 

## Higher order functions
List.map (fun x -> x*x) [1;2;3] (* notice ; *)

List.iter print_int [1;2;3]
123

## Type declarations
type name1 = typedef1
and name2 = typedef2

## Pattern matching
let andg p = match p 
  with (true, true) -> true
    | _ -> false

let length = function
[] -> "empty"
| [_] -> "singleton"
| [_; _] -> "pair"
| [_; _; _] -> "triplet"
| hd :: tl -> "many";;
use when for guards and as for naming matches. 

## Records in Ocaml
type base = {x: int; y:int};;
Ocaml normally have ;, instead of ,.

## ADT 
type seasons = Hello | Goaway;; (* Tag name should be capital *)
let weather = function 
  Hello -> "Hai"
  Goaway -> "Bye"
;;

## Exceptions
exception Foo of string;;
use raise to raise an exception and try to try it. 

## Modules
writing things in a file abc.ml makes a module abc and you can import it via open abc
Everything is public and each file is a module. You can also put interfaces in .mli and implementation in .ml

## Functors
A functor is a module which is parameterized by another module, just like a Higher order functions. It is a bit similar Typeclass in Haskell.
Functors returns operations that work exclusively for that value. For example, 
Set module provides a Make functor, which takes one argument which is a module and provides type of elements t and comparison function compare. 

 module Int_set = Set.Make (struct 
  type t = int 
  let compare = compare
  end);;

For sets of string, standard lib provides String module with t and compare. 
module String_set = Set.Make (String);;

Defining a Functor can be done as 

module F (X : X_type) : Y_type =
struct
end

Instantiating can be done as follows.

module Int_set = Set.Make (struct type t = int let compare = compare end);; 

## Imperative features
5;6;7;; (* only 7 is output *)
print result is unit. 

Arrays are made by let a = [|1;2;3|] and they are mutable. a.(1) <- 4;;
You can also use Array.make 5 0;; and accessed using a.(2);;
Array.of_list to make array from list. or Array.append. 
Arrays are O(n) in appending and O(1) in random access while List is the other way around and also immutable. 
Array.make 4 5;; Size 4 with element 5.

## Hash Table
module StringHash = Hashtbl.Make(struct type t = string   (* type of keys *) 
let equal x y = x = y   (* use structural comparison *) 
let hash = Hashtbl.hash ( * generic hash function *) end); 

# Modules
Modules are like normal modules. Open will put it in scope. Otherwise we need to call it with full name just like in elixir. By default all things in a module are public. Inorder to avoid that we will use a module interface. So we only write things there which should be publicly available. And mli files hould be compiled before .ml files. 

ocamlopt compiles to native code.

We can nested modules. Inside E.ml file, we write
```ocaml
module H = struct 
  let message = "H"
end 

(* Accessing from outside *)
let () =
  E.H.message
```
For doing it inside interfaces use **sig** keyword.

# Mutation and Ref
ref can be used to make a reference which is mutable. := can write value to this reference. 
```ocaml
let result = ref 1 in
    for i = 2 to n do
      result := i * !result
```

: is used to type annotation
```ocaml
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
```