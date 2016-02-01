#lang scribble/manual
@require[racket/include
         scribble/eval
         scriblib/footnote
         compiler/zo-parse
         zordoz/typed ;; To format provided identifiers
         @for-syntax[racket/base
                     (only-in racket/list make-list split-at)]
         @for-label[compiler/zo-parse
                    zordoz
                    racket/base
                    syntax/toplevel]]

@(define zordoz-eval
  (make-base-eval
    '(begin (require compiler/zo-structs zordoz racket/string))))

@title{API}

@local-table-of-contents[]

These functions support the REPL, but may be useful in more general settings.
Import them with @racket[(require zordoz)].



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

@defproc[(result-zo [result zo-result?]) zo?]{
Converts a @racket[zo-result?] to the found @racket[zo?] field.
See @racket[zo-find].
}

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

@defproc[(compiled-expression->zo [cmp compiled-expression?]) zo?]{
  Converts a compiled expression into a zo struct.
  Differs from @racket[zo-parse] in that the input is expected to be a
  @racket[compiled-expression?].
  This function is the inverse of @racket[zo->compiled-expression].
}

@examples[#:eval zordoz-eval
  (compiled-expression->zo (compile-syntax #'6))
  (compiled-expression->zo (compile-syntax #'(member 'a '(a b c))))
  (compiled-expression->zo (compile-syntax #'(if #t 'left 'right)))
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

@defproc[(toplevel-syntax->zo [stx syntax?]) (listof zo?)]{
  Variant of @racket[syntax->zo], except can handle top level syntax
  expressions.
  Uses @racket[eval-compile-time-part-of-top-level/compile] to compile syntax
  rather than just @racket[compile].
  As such, this function returns a list of @racket[zo] structs rather than just
  one.
}

@examples[#:eval zordoz-eval
  (toplevel-syntax->zo #'(begin
                           (define x 5)
                           x))
]

@section{Compiling C Modules}

Tools for compiling modules implemented in C.

@defproc[(compile-c-module [c-path (or/c path-string? path?)]) void?]{
Compiles a C module to a form where it can be required later.

See @other-doc['(lib "scribblings/inside/inside.scrbl")] for more information on how
to build Racket modules in C.

@bold{@larger{@larger{WARNING:}}}
Do not replace the file produced by the functions while still
inside the Racket VM.
Doing so will cause undefined and potentially catastrophic behavior.
As a general rule of thumb, if you modify a C file implementing a module,
shut down all Racket VMs using that library. This means restarting
DrRacket (not just reloading the file) whenever the C file is modified.


@racket[c-path] is the path to the C file that implemented the module.

For example:

@racketblock[
(require zordoz)
(compile-c-module "c-module.c")
(dynamic-require "c-module" 0)
]
}

@defform[(from-c c-path)
         #:contracts ([c-path path-string?])]{
A convenience form to compile a C module and require it directly. Use outside
of a @racket[require] form is a syntax error.

@racket[c-path] is the path to the C file that implements the module.

For example:

@racketblock[
(require zordoz
         (from-c "c-module.c"))
]
}

@section{Typed API}

A typed version of this API is available with @racket[(require zordoz/typed)].

@; Collect identifiers from zordoz/typed, render in a table
@(define-for-syntax (parse-provide* x*)
  (for*/list ([phase+id* (in-list x*)]
              [id* (in-list (cdr phase+id*))])
    #`@racket[#,(car id*)] ))
@(define-for-syntax (split-at/no-fail n x*)
  (define N (length x*))
  (if (< N n)
    (let ([padded (append x* (make-list (- n N) ""))])
      (values padded '()))
    (split-at x* n)))
@(define-syntax (render-zordoz/typed stx)
  (define flat-id*
    (let-values (((var* stx*) (module->exports '(lib "zordoz/typed"))))
      (append (parse-provide* var*) (parse-provide* stx*))))
  (with-syntax ([((id* ...) ...)
    (let loop  ([id* flat-id*])
      (if (null? id*)
        '()
        (let-values ([(row rest) (split-at/no-fail 3 id*)])
          (cons row (loop rest)))))])
  #'@tabular[#:sep @hspace[4] (list (list id* ...) ...)]))

@defmodule[zordoz/typed]{
  Require @racket[zordoz/typed] for a typed version @racket[zordoz].
  Provided identifiers are:
  @(render-zordoz/typed)
}

@defmodule[zordoz/typed/zo-structs]{
Require @racket[zordoz/typed/zo-structs] for a typed version of Racket's @racket[compiler/zo-structs].
}
