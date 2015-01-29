#lang scribble/manual

@title{Future Work}

Bytecode is a useful format to program with.
The language is much simpler than full-blown Racket.
Besides this simple command-line explorer, here are a few ideas for future tools.

See also the project's @hyperlink["https://github.com/bennn/zordoz/issues"]{issue tracker}.

@section{Checking two files for differences}

Bytecode should be much more amenable to "diffing" than source files.
Superficial changes should disappear after the compiler's extracted the core functionality.
Potential applications:
@itemlist[
  @item{Code search -- find a library function similar to a code chunk}
  @item{Detect malware / corrupted files}
  @item{Cheating detection software}
]
These are just ideas for now.
First thing is to explore and see what's possible and feasible.

See @hyperlink["http://cs.brown.edu/~spr/research/s6.html"]{S6} project for inspiration.
Also, possibly, @hyperlink["http://theory.stanford.edu/~aiken/moss/"]{MOSS}.


@section{More Search Options}

The current @tt{find} tool is very simple.
It is only string matching on struct names.
You need to know @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28part._.Bytecode_.Representation%29"]{what structs} are available before using it at all.

The question is, what search tools would be most useful?
Some ideas:
@itemlist[
  @item{Search for an identifier from the source code.}
  @item{Search for patterns of structs (lambdas containing if-statements, I dunno)}
  @item{Find contracts, or instances of another higher-order construct.}
  @item{Search for often-run structs.}
]


@section{Bytecode Graphs}

Working with the REPL gives a nice idea of that the struct hierarchy in the bytecode looks like.
With a paper and pencil, I can trace out the whole picture for myself.

Unless space / efficiency becomes a problem, we should be able to generate a picture.
Possibly use the @hyperlink["https://github.com/tonyg/racket-explorer"]{racket-explorer}, or maybe just make a graphical tool for Dr. Racket and record the images as the user steps through.
(Imagine a REPL that remembered the picture of a user's search path.)


@section{Dr.Racket Integration}

The command line is nice, but an interactive panel within Dr. Racket would be much nicer.
One of these days.
