run: ast.cmo eval.cmo main.cmo
	ocamlc -o $@ $^

ast.cmi: ast.mli
	ocamlc -c $<
ast.cmo: ast.ml ast.cmi
	ocamlc -c $<

eval.cmi: eval.mli ast.cmi
	ocamlc -c $<
eval.cmo: eval.ml ast.cmi eval.cmi
	ocamlc -c $<

main.cmo: main.ml ast.cmi eval.cmi
	ocamlc -c $<
