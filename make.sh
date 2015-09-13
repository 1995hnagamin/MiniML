ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -o miniml util.ml mySet.ml syntax.ml parser.mli parser.ml lexer.ml environment.ml typing.ml eval.ml main.ml
