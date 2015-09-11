ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -o ocaml util.ml syntax.ml parser.mli parser.ml lexer.ml environment.ml eval.ml main.ml
