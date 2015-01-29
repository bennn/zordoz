#lang scribble/manual
@require[racket/include]

@title[#:tag "top"]{@bold{Zordoz}}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@bold{Zordoz} is a tool for exploring @tt{.zo} bytecode files.
It offers a simple command-line interface for getting string representations of a bytecode struct and exploring its fields.

These docs describe the REPL and the public functions supporting it.

@local-table-of-contents[]

@include-section{overview.scrbl}
@include-section{repl.scrbl}
@include-section{api.scrbl}
@include-section{future-work.scrbl}
