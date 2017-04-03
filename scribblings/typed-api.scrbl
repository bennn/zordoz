#lang scribble/manual
@require[
  zordoz/typed ;; To format provided identifiers
  (for-syntax racket/base
              (only-in racket/list make-list split-at))]

@title{Typed API}

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
  A typed version of @racketmodname[zordoz].
  Provided identifiers are:
  @;@(render-zordoz/typed)
}

@defmodule[zordoz/typed/zo-structs #:no-declare]{
A typed version of Racket's @racketmodname[compiler/zo-structs].
}
