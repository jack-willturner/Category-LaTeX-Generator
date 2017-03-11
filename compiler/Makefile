default: src/compiler_test.native

%.native:
	eval `opam config env`
	ocamlbuild -use-menhir -use-ocamlfind compiler_test.native
	ocamlbuild -use-menhir -use-ocamlfind -pkg core cc.native

clean:
		rm *.native
		rm -rf _build
