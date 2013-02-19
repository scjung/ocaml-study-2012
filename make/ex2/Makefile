run: ast.cmo eval.cmo main.cmo
	ocamlc -o run ast.cmo eval.cmo main.cmo

ast.cmi: ast.mli
	ocamlc -c ast.mli
ast.cmo: ast.cmi ast.ml
	ocamlc -c ast.ml

eval.cmi: ast.cmi eval.mli
	ocamlc -c eval.mli
eval.cmo: ast.cmi eval.cmi eval.ml
	ocamlc -c eval.ml

main.cmo: ast.cmi eval.cmi main.ml
	ocamlc -c main.ml