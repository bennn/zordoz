#lang racket/base

(require compiler/zo-parse
         compiler/zo-marshal
         compiler/decompile
         racket/port)

(provide (all-defined-out))

(define (syntax->zo stx)
  (define x (compile-syntax stx))
  (define-values (in out) (make-pipe))
  (display x out)
  (close-output-port out)
  (define y (port->bytes in))
  (define z (zo-parse (open-input-bytes y)))
  z)

(define (syntax->decompile stx)
  (decompile (syntax->zo stx)))

(define (zo->compiled-expression zo)
  (parameterize ([read-accept-compiled #t])
    (define x (zo-marshal zo))
    (with-input-from-bytes x
      (lambda () (read)))))