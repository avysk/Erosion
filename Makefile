all:
	ocamlbuild erosion.native
clean:
	rm -rf _build *.cmi *.cmo *.native
