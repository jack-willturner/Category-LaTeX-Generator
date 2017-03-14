FILENAME=syntax-example
default : main

main:
	eval `opam config env`
	corebuild -use-menhir cc.native $(FILENAME)
	./cc.native $(FILENAME)
	pdflatex $(FILENAME).tex
	open $(FILENAME).tex

clean:
		rm *.native
		rm -rf _build
