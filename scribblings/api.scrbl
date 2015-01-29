#lang scribble/manual

@title{API}

@local-table-of-contents[]
@defmodule[zordoz]

These functions support the REPL, but may be useful in more general settings.


@section{String Representations}

The file @tt{zo-string.rkt} exports two functions and a contract.
Broadly, these functions work to convert a zo struct to a human-readable string.
But @code{zo->spec} returns a structured list, just before the full-on string representation, for programming convenience.

@defproc[(zo->string [z zo?] [#:deep? deep boolean?]) string?]{
  Convert a zo struct into a string.
  When the optional argument @code{#:deep?} is set, include the struct's fields in the string representation.
  Otherwise, only print the name.
}

@defproc[(zo->spec [z zo?]) spec/c]{
  Convert a zo struct into a @code{spec/c} representation.
  This is a structured list of strings---not quite as pretty, but easier to program with.

  Specs are a list containing:
  @itemlist[
    @item{A string, representing its name.}
    @item{Pairs, representing the struct's fields.
          The first element of each pair should be a string representing the field name.
          The second element should be a thunk that, when forced, yields either a string or another spec.}
  ]
  The thunks prevent an entire struct from being converted at once.
}


@section{Traversing zo Structs}

The file @tt{zo-transition.rkt} accomplishes a very simple task.
Given a zo struct @tt{z} and a string @tt{str}, it attempts to look up the field named @tt{str} within @tt{z}.
That's it, and it takes about 500 lines of code to do so.

The sole exported function is:

@defproc[(zo-transition [z zo?] [str string?]) (values (values (or/c zo? (listof zo?)) boolean?))]{
  Identify what specific zo struct @tt{z} is, then access its field named @tt{str}, if any.
  The multiple return values deal with the following cases:
  @itemlist[
    @item{If the field @tt{str} does not exist, or does not denote a zo struct, return the argument @tt{z} and the boolean value @tt{#f}.}
    @item{If the field @tt{str} denotes a list and we can parse zo structs from the list, return a list of zo structs and the boolean @tt{#t}.}
    @item{(Expected case) If the field points to a zo struct, return the new zo struct and the boolean @tt{#t}.}
  ]
}


@section{Search}

If you know the name of the @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28part._.Bytecode_.Representation%29"]{zo struct} you hope to find by exploring a subtree, you can automate the exploring.
Literally, @tt{find} is repeated application of @tt{zo->string} and @tt{zo-transition}.

@defproc[(find [z zo?] [str string?]) (listof result?)]{
  Starting with the children of the struct @tt{z}, search recursively for struct instances matching the string @tt{str}.
  For example, if @tt{str} is @racket{application} then @tt{find} will return all @hyperlink["http://docs.racket-lang.org/raco/decompile.html#%28def._%28%28lib._compiler%2Fzo-structs..rkt%29._application%29%29"]{application structs} nested below @tt{z}.

  The return value is a list of @tt{result} structs rather than plain zo structs because we record the path from the argument @tt{z} down to each match.
}

@defstruct*[result ([z zo?] [path (listof zo?)]) #:transparent]{
  A @tt{result} is really just a pair: a zo struct and a path leading to it.
  In the context of @tt{find}, the path is always from the struct @tt{find} was called with.
}

