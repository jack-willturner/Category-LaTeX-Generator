# The Category Compiler
This is the repository that will be used to manage the code for my final year project at the University of Birmingham. The tool itself will be a web-based application that allows users to describe the string-diagrams of monoidal categories using the syntax defined below. The compiler will then compute and generate the corresponding LaTeX to draw the diagram.

Currently the main features are all working (with a few bugs) but the tool is command-line based. 

# Installation instructions 
Packages you'll need to have installed:
* OCaml 4.04.0
* ocamlbuild
* ocamlfind 
* menhir

Clone the repository into a folder and move into the compiler directory. Then run:
```
eval `opam config env`
```
Change the diagram in `syntax-example` to the diagram that you want to draw. Then run:
```
ocamlbuild -use-menhir -use-ocamlfind compiler_test.native

./compiler_test.native
```
and copy and paste the output into your TEX file.

# Syntax of the Language
Formally the diagrams can be specified as:
```
diagram :=
	     IDENTITY
        | morphism
        | diagram COMPOSE diagram
        | diagram TENSOR diagram


; = COMPOSE
| = TENSOR
```
## Boxes
Each morphism (or box) must be defined for the preprocessor:
```
box <name> <{styling}> : <number of inputs> -> <number of outputs>
```
For example:
```
box f {boxcolour : RED; boxshape : CIRCLE} : 1 -> 3
```
Box definitions must be separated by fullstops.

## Wires
The compiler will infer any non-specified links, but the syntax also allows for explicit labels to be given to ports:
```
link a x, b y, c z.
```

Below is an example of a diagram description in the syntax:
```
box f : 1 -> 2.
box g : 2 -> 1.
box h : 2 -> 1
link x y. f[1,x] ; g ; [1,y]h
```
The `[1,x]` after the f specifies that the f has two outputs. The first can be inferred, but the second should be called `x`. The `[1,y]` after the h specifies that `h` has two inputs - the first can be inferred but the second should be called `y`. The compiler will then link `x` to `y` based on the line `link x y.`

## Modules
The syntax can also be used to describe modules, which can then be imported into other files or just used within the same file for neatness. I will write modules for commonly used categories and consider putting them in a 'standard library'.

A module would be written:
```
module m = {
  box f : 1 -> 2.
  box g : 2 -> 1.
  box h : 2 -> 1
  link x y. f[1,x] ; g ; [1,y]h
}
m ; m
```
where `m ; m` composes `m` onto itself.
