run: ast.cmo eval.cmo main.cmo
	ocamlc -o $@ $^

%.cmi: %.mli
	ocamlc -c $<
%.cmo: %.ml
	ocamlc -c $<

include .depend
