file=syntax-example
default : main

main:
	eval `opam config env`
	corebuild -use-menhir cc.native $(file)
	./cc.native $(file)
	pdflatex $(file).tex
	open $(file).pdf

tests:
	eval `opam config env`
	ocamlbuild -use-menhir -use-ocamlfind parser_test.native
	ocamlbuild -use-menhir -use-ocamlfind compiler_test.native
	ocamlbuild -use-menhir -use-ocamlfind bitmap_test.native
	echo “———————————————————————— PARSER TESTS ————————————————————————“
	./parser_test.native
	echo “———————————————————————— COMPILER TESTS ————————————————————————“
	./compiler_test.native
	echo “———————————————————————— BITMAP TESTS ————————————————————————“
	./bitmap_test.native
	clean

clean:
		rm -rf _build
		rm *.native
		rm *.aux
		rm *.log
		rm *.tex
		rm *.pdf
		
