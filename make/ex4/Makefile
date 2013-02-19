run: ast.cmo eval.cmo main.cmo
	ocamlc -o $@ $^

# dependency
ast.cmi: ast.mli
ast.cmo: ast.ml ast.cmi
eval.cmi: eval.mli ast.cmi
eval.cmo: eval.ml ast.cmi eval.cmi
main.cmo: main.ml ast.cmi eval.cmi

# compilation rules
%.cmi: %.mli
	ocamlc -c $<
%.cmo: %.ml
	ocamlc -c $<
