file=syntax-example
default : main

main:
	eval `opam config env`
	corebuild -quiet -use-menhir cc.native $(file)
	./cc.native $(file)
	pdflatex $(file).tex

clean:
		rm -rf _build
		rm *.native
		rm *.aux
		rm *.log
		rm *.tex
		rm *.pdf
