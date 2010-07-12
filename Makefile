all:
	ocamlbuild erosion.native -libs graphics
clean:
	rm -rf _build *.cmi *.cmo *.native
