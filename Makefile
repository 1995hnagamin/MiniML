RESULT = miniml
SOURCES = \
	src/util.ml src/syntax.ml \
	src/parser.mli src/parser.mly src/lexer.mll \
	src/environment.ml src/eval.ml \
	src/main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
