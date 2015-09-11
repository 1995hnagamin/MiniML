ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -o ocaml syntax.ml parser.mli parser.ml lexer.ml environment.ml eval.ml main.ml
