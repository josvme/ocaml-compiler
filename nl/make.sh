#!/bin/zsh
ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.ml
ocamlc -c utils.ml
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c luttu.ml
ocamlc -o luttu ast.cmo parser.cmo scanner.cmo utils.cmo luttu.cmo
# Notice the order of compilation and argument is very important. For example ast.cmo should be first in last step.
