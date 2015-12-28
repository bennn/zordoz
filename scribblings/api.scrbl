#lang scribble/manual
@require[racket/include
         scribble/eval
         compiler/zo-parse
         @for-label[compiler/zo-parse
                    zordoz
                    racket/base]]

@(define zordoz-eval
  (make-base-eval
    '(begin (require compiler/zo-structs zordoz racket/string))))

@title{API}

@local-table-of-contents[]

These functions support the REPL, but may be useful in more general settings.
Import them with `(require zordoz)`.
Typed variants of the same functions are available with `(require zordoz/typed)`.


@section{Starting a REPL}

@defproc[(filename->shell [fname path-string?]) void?]{
  Start a REPL to explore a @racket[.zo] bytecode file.
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

@examples[#:eval zordoz-eval
  (displayln (zo->string (primval 129)))
  (displayln (zo->string (primval 129) #:deep? #f))
  (displayln (zo->string (branch (= 3 1) "true" 'false)))
]

@defproc[(zo->spec [z zo?]) spec/c]{
  Convert a zo struct into a @racket[spec/c] representation.
  A @racket[spec/c] is a list containing:
  @itemlist[
    @item{A string, representing its name.}
    @item{Pairs, representing the struct's fields.
          The first element of each pair should be a string representing the field name.
          The second element should be a thunk that, when forced, yields either a string or another spec.}
  ]
  The thunks delay pretty-printing an entire nested struct.
}

@examples[#:eval zordoz-eval
  (zo->spec (primval 129))
  (zo->spec (branch (= 3 1) "true" 'false))
]


@section{Traversing @racket[zo] Structs}

Racket does not provide a reflective way of accessing struct fields at runtime.
So we provide a function that does this by-force, just for zo structures.


@defproc[(zo-transition [z zo?] [str string?]) (values (values (or/c zo? (listof zo?)) boolean?))]{
  Identify what specific zo struct @racket[z] is, then access its field named @racket[str], if any.
  The multiple return values deal with the following cases:
  @itemlist[
    @item{If the field @racket[str] does not exist, or does not denote a zo struct, return the argument @racket[z] and the boolean value @racket[#f].}
    @item{If the field @racket[str] denotes a list and we can parse zo structs from the list, return a list of zo structs and the boolean @racket[#t].}
    @item{(Expected case) If the field points to a zo struct, return the new zo struct and the boolean @racket[#t].}
  ]
}

@examples[#:eval zordoz-eval
  (let-values ([(z success?) (zo-transition (primval 42) "foo")])
    (displayln success?)
    z)
  (let-values ([(z success?) (zo-transition (primval 42) "id")])
    (displayln success?)
    z)
  (let-values ([(z success?) (zo-transition
                               (application (primval 42) '())
                               "rator")])
    (displayln success?)
    z)
]


@section{Searching Structs}

If you know the name of the @racket[zo struct] you hope to find by exploring a subtree, you can automate the exploring.
Literally, @racket[find] is repeated application of @racket[zo->string] and @racket[zo-transition].

@defproc[(zo-find [z zo?] [str string?] [#:limit lim (or/c natural-number/c #f) #f]) (listof result?)]{
  Starting with the children of the struct @racket[z], search recursively for struct instances matching the string @racket[str].
  For example, if @racket[str] is @racket[application] then @racket[find] will return all @racket[application] structs nested below @racket[z].

  The return value is a list of @racket[result] structs rather than plain zo structs because we record the path from the argument @racket[z] down to each match.
}

@examples[#:eval zordoz-eval
  (let* ([seq* (list (seq '()) (seq '()))]
         [z (seq (list (seq seq*) (seq seq*)))])
    (zo-find z "seq" #:limit 1))
  (let* ([thn (primval 0)]
         [els (branch #t (primval 1) (primval 2))]
         [z (branch #t thn els)])
    (map result-zo (zo-find z "primval")))
]

@defstruct*[result ([z zo?] [path (listof zo?)]) #:transparent]{
  A @racket[result] contains a zo struct and a path leading to it from the search root.
  In the context of @racket[find], the path is always from the struct @racket[find] was called with.
}

@defproc[(find-all [fname path-string?] [qry* (Listof String)] [#:limit lim (or/c natural-number/c #f) #f]) void?]{
  Apply find iteratively on the bytecode file @racket[fname].
  Print the results for each string in the list @racket[qry*] to @racket[current-output-port].
}


@section{Compiling and Decompiling}

Tools for compiling syntax fragments rather than entire modules.

@defproc[(syntax->zo [stx syntax?]) zo?]{
  Compiles a syntax object to a @racket[zo] struct.
  The result is wrapped in a @racket[compilation-top] struct.
}

@examples[#:eval zordoz-eval
  (syntax->zo #'6)
  (syntax->zo #'(member 'a '(a b c)))
  (syntax->zo #'(if #t 'left 'right))
]

@defproc[(syntax->decompile [stx syntax?]) any/c]{
  Compiles a syntax object, then immediately decompiles the compiled code back to an S-expression.
  Similar to @racket[syntax->zo], except the final output is Racket code and not a @racket[zo] structure.
}

@examples[#:eval zordoz-eval
  (syntax->decompile #'6)
  (syntax->decompile #'(member 'a '(a b c)))
  (syntax->decompile #'(if #t 'left 'right))
]

@defproc[(compiled->zo [cmp compiled-expression?]) zo?]{
  Converts a compiled expression into a zo struct.
  Differs from @racket[zo-parse] in that the input is expected to be a
  @racket[compiled-expression?].
  This function is the inverse of @racket[zo->compiled-expression].
}

@examples[#:eval zordoz-eval
  (compiled->zo (compile-syntax #'6))
  (compiled->zo (compile-syntax #'(member 'a '(a b c))))
  (compiled->zo (compile-syntax #'(if #t 'left 'right)))
]

@defproc[(zo->compiled-expression [z zo?]) compiled-expression?]{
  Transform a @racket[zo] struct to compiled code.
  The compiled code can be run with @racket[eval].
  If the struct @racket[z] encodes a module (i.e., contains a @racket[mod] sub-struct)
  then the result @racket[zo->compiled-expressions z] can be written to a @racket[.rkt] file and run using the Racket executable.
}

@examples[#:eval zordoz-eval
  (let* ([stx #'(string-append "hello, " "world")]
         [z     (syntax->zo stx)]
         [e     (zo->compiled-expression z)])
    (eval e (make-base-namespace)))
]

