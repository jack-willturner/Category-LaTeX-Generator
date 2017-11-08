# The Category Compiler
Currently the main features are all working (with a few bugs) but the tool is command-line based - there is also the website version [here](http://jack-turner.co.uk/category-compiler/)

# Installation instructions
Packages you'll need to have installed:
* OCaml 4.04.0
* ocamlbuild
* ocamlfind
* menhir
* pdflatex
* corebuild

# Compiling your diagrams
Clone the repository into your chosen directory and move into the compiler directory. Run:

```
make file=$filename$
```
and the tex/pdf file will generate into the same directory as `$filename$`.

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
box f {boxcolour : RED; boxshape : CIRCLE} : 1 -> 3.
```
Box definitions must be separated by semicolons, and a list of boxes should be ended with a full stop.

## Wires
The compiler will infer any non-specified links, but the syntax also allows for explicit labels to be given to ports:
```
link a x, b y, c z.
```

Below is an example of a diagram description in the syntax:
```
box f : 1 -> 2;
box g : 2 -> 1;
box h : 2 -> 1.
link x y. f[1,x] ; g ; [1,y]h
```
The `[1,x]` after the f specifies that the f has two outputs. The first can be inferred, but the second should be called `x`. The `[1,y]` after the h specifies that `h` has two inputs - the first can be inferred but the second should be called `y`. The compiler will then link `x` to `y` based on the line `link x y.`

Alternatively, if links are trivial then you can remove the `link` syntax and allow the compiler to infer the connections:

```
box f : 1 -> 2;
box g : 2 -> 1.
f ; g
```

In addition to this, we can also use constants for forking (`<`) and joining (`>`) wires:

```
box f : 1 -> 2;
box g : 2 -> 1.
link x< y>. f[1,x<] ; [1,y>]g
```

## Modules
The syntax can also be used to describe modules, which can then be imported into other files or just used within the same file for neatness. I will write modules for commonly used categories and consider putting them in a 'standard library'.

A module would be written:
```
module m = {
  box f : 1 -> 2;
  box g : 2 -> 1;
  box h : 2 -> 1.
  link x y. f[1,x] ; g ; [1,y]h
}
$m ; $m
```
where `$m ; $m` composes `m` onto itself.


## Tests
Due to some issues with `corebuild` the tests are best compiled by hand and inspected individually.
Type:
```
make file=test/<test-dir>/<individual-test>
```
to view the generated pdfs. 
