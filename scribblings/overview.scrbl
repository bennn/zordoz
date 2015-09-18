#lang scribble/manual

@title{Overview}

@local-table-of-contents[]


@section{Quickstart}

The main entry point is @tt{zordoz.rkt}.

@subsection{Explorer}
If you have a bytecode file ready, run

@racketblock[racket zordoz.rkt FILE.zo]

and you're started.
Type @tt{help} at the REPL to see available commands.

@racketblock[
zo> help
At your service. Available commands:
  alst        Print command aliases
  back        Move up to the previous context
  dive ARG    Step into struct field ARG
  find ARG    Search the current subtree for structs with the name ARG
  help        Print this message
  info        Show information about current context
  jump        Revert to last saved position
  save        Save the current context as jump target
  quit        Exit the interpreter
]

@subsection{Quick Search}
To search a bytecode file for occurrences of a certain zo struct, use the @tt{-f} flag.
@racketblock[racket zordoz.rkt -f STRUCT-NAME FILE.zo]

the number of occurrences of each struct will be printed to the console.
For example:
@racketblock[
$ racket zordoz.rkt private/compiled/zo-string_rkt.zo branch lam
INFO: Loading bytecode file 'private/compiled/zo-string_rkt.zo'...
INFO: Parsing bytecode...
INFO: Parsing complete! Searching...
FIND 'branch' : 427 results
FIND 'lam' : 433 results
All done!
]
The command accepts any number of @tt{-f} flags and struct names.


@section{Building}

The included Makefile runs @tt{raco make zordoz.rkt} to generate an executable.
The Makefile additionally renames this executable to @tt{zordoz}, so you can run:

@racketblock[./zordoz FILE.zo]

If you installed via @tt{raco}, then the command 

@racketblock[raco zordoz FILE.zo]

will be available.


@section{Testing}

Each source file contains a @tt{module+ test} with unit tests.
Run them all with:

@racketblock[make test]

or individually using:

@racketblock[raco test FILE.rkt]


@section{Project Goals}

The goal of this project is to help explore Racket bytecode in any useful way.
This library should be available to as many versions of Racket as possible,
and kept up-to-date.

Racket offers a de-compilation @hyperlink["http://docs.racket-lang.org/raco/decompile.html"]{API}, however the structs it produces are still dense reading.
This project takes a de-compiled @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28def._%28%28lib._compiler%2Fzo-parse..rkt%29._zo-parse%29%29"]{zo struct} and offers:

@itemlist[

    @item{A string representation of the struct, with name and fields clearly labeled.}

    @item{Step-by-step exploration of the struct's fields.}

    @item{A simple search interface for finding structs nested within the current.}

]

We hope to add more features, especially a tool for comparing two bytecode files.
@hyperlink["https://github.com/bennn/zordoz/issues"]{Feedback} and suggestions appreciated!
