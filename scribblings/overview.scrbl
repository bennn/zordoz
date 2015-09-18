#lang scribble/manual

@title{Overview}

@local-table-of-contents[]


@section{Quickstart}

To install, either use @tt{raco}

@racketblock[raco pkg install zordoz]

Or clone the repository and install manually, via raco.

@racketblock[
$ git clone https://github.com/bennn/zordoz
$ raco pkg install zordoz/
]

Zordoz provides a raco command.
To see help information, run:

@racketblock[raco zordoz --help]


@subsection{Explorer}
The default mode is to interactively explore a bytecode file.
Assuming @tt{FILE.zo} is a compiled file on your computer,

@racketblock[raco zordoz FILE.zo]

will start a REPL session.
Type @tt{help} at the REPL to see available commands.
See @Secref{REPL} for a detailed explanation of each.


@subsection{Automated Search}
To search a bytecode file for occurrences of a certain zo struct, use the @tt{-f} flag.
(This flag may be supplied more than once.)
@racketblock[raco zordoz -f STRUCT-NAME FILE.zo]

The number of occurrences of each struct will be printed to the console.
For example:
@racketblock[
$ raco zordoz -f branch -f lam private/compiled/zo-string_rkt.zo
INFO: Loading bytecode file 'private/compiled/zo-string_rkt.zo'...
INFO: Parsing bytecode...
INFO: Parsing complete! Searching...
FIND 'branch' : 427 results
FIND 'lam' : 433 results
All done!
]


@section{Testing}

Each source file contains a @tt{module+ test} with unit tests.
Run them all with:

@racketblock[raco test zordoz]

or individually using:

@racketblock[raco test FILE.rkt]


@section{Project Goals}

Racket offers a de-compilation @hyperlink["http://docs.racket-lang.org/raco/decompile.html"]{API}, however the structs it produces are still dense reading.
This project takes a de-compiled @racket[zo] struct and offers:

@itemlist[

    @item{A string representation of the struct, with name and fields clearly labeled.}

    @item{Interactive exploration of the struct's fields.}

    @item{A simple search interface for finding patterns nested within a struct.}

]

This library should be available to as many versions of Racket as possible,
and kept up-to-date.

We also hope to add more features, especially a tool for comparing two bytecode files.
@hyperlink["https://github.com/bennn/zordoz/issues"]{Feedback} and suggestions appreciated!

