file=syntax-example
default : main

main:
	eval `opam config env`
	corebuild -use-menhir cc.native $(file)
	./cc.native $(file)
	pdflatex $(file).tex
	open $(file).pdf

clean:
		rm *.native
		rm *.aux
		rm *.log
		rm *.tex
		rm *.pdf
		rm -rf _build
