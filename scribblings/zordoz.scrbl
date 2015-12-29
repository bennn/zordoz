#lang scribble/manual
@require[racket/include]

@title[#:tag "top"]{Zordoz}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}
        @hyperlink["https://github.com/LeifAndersen"]{Leif Andersen}]

@defmodule[zordoz]

@bold{Zordoz} is a tool for exploring @tt{.zo} bytecode files.
It offers a simple command-line interface for exploring string representations of bytecode structures.

These files describe the REPL and the API functions supporting it.
Jump to the bottom of the @secref{REPL} section for example usage.

@local-table-of-contents[]

@include-section{overview.scrbl}
@include-section{repl.scrbl}
@include-section{api.scrbl}
@include-section{future-work.scrbl}
