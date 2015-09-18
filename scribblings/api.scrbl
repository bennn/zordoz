#lang scribble/manual

@title{API}

@local-table-of-contents[]

These functions support the REPL, but may be useful in more general settings.
Import them with `(require zordoz)`.
Typed variants of the same functions are available with `(require zordoz/typed)`.


@section{Starting a REPL}

@defproc[(filename->shell [fname path-string?]) void?]{
  Start a REPL to explore a @tt{.zo} bytecode file.
}

@defproc[(zo->shell [z zo]) void?]{
  Start a REPL to explore a zo struct.
}

@defproc[(syntax->shell [stx syntax?]) void?]{
  Start a REPL to explore a syntax object.
  First compiles the syntax to a zo representation.
}


@section{String Representations}

These tools convert a zo structure to a pretty-printed string, or a more structured representation.

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
  The thunks delay pretty-printing an entire nested struct.
}


@section{Traversing zo Structs}

Racket does not provide a reflective way of accessing struct fields at runtime.
So we provide a function that does this by-force, just for zo structures.


@defproc[(zo-transition [z zo?] [str string?]) (values (values (or/c zo? (listof zo?)) boolean?))]{
  Identify what specific zo struct @tt{z} is, then access its field named @tt{str}, if any.
  The multiple return values deal with the following cases:
  @itemlist[
    @item{If the field @tt{str} does not exist, or does not denote a zo struct, return the argument @tt{z} and the boolean value @tt{#f}.}
    @item{If the field @tt{str} denotes a list and we can parse zo structs from the list, return a list of zo structs and the boolean @tt{#t}.}
    @item{(Expected case) If the field points to a zo struct, return the new zo struct and the boolean @tt{#t}.}
  ]
}


@section{Searching Structs}

If you know the name of the @racket[zo struct] you hope to find by exploring a subtree, you can automate the exploring.
Literally, @tt{find} is repeated application of @tt{zo->string} and @tt{zo-transition}.

@defproc[(zo-find [z zo?] [str string?] [#:limit lim (or/c natural-number/c #f) #f]) (listof result?)]{
  Starting with the children of the struct @tt{z}, search recursively for struct instances matching the string @tt{str}.
  For example, if @tt{str} is @racket{application} then @tt{find} will return all @racket[application] structs nested below @tt{z}.

  The return value is a list of @tt{result} structs rather than plain zo structs because we record the path from the argument @tt{z} down to each match.
}

@defstruct*[result ([z zo?] [path (listof zo?)]) #:transparent]{
  A @tt{result} contains a zo struct and a path leading to it from the search root.
  In the context of @tt{find}, the path is always from the struct @tt{find} was called with.
}

@defproc[(find-all [fname path-string?] [qry* (Listof String)] [#:limit lim (or/c natural-number/c #f) #f]) void?]{
  Apply find iteratively on the bytecode file @racket[fname].
  Print the results for each string in the list @racket[qry*].
}


@section{Compiling and Decompiling}

Tools for compiling syntax fragments rather than entire modules.

@defproc[(syntax->zo [stx syntax?]) zo?]{
  Compiles a syntax object to a @racket[zo] struct.
  The result is wrapped in a @racket[compilation-top] struct.
}

@defproc[(syntax->decompile [stx syntax?]) any/c]{
  Compiles a syntax object, then immediately decompiles the compiled code back to an S-expression.
  This is @emph{not} an identity transformation.
}

@defproc[(zo->compiled-expression [z zo?]) compiled-expression?]{
  Transform a @racket[zo] struct to compiled code.
}

