#lang racket/base

(provide compile-c-module
         from-c)

(require racket/file
         dynext/file
         dynext/compile
         dynext/link
         (for-syntax racket/base
                     syntax/parse
                     racket/require-transform
                     racket/file
                     dynext/file
                     dynext/compile
                     dynext/link))

(define-syntax-rule (define-for-syntax-and-runtime f ...)
  (begin (define f ...)
         (define-for-syntax f ...)))

(define-for-syntax-and-runtime (compile-c-module c-source)
  (define extensionless-source (path-replace-suffix c-source ""))
  (define object-target-path
    (build-path "compiled" "native" (system-library-subpath)))
  (define object-target
    (build-path object-target-path (append-object-suffix extensionless-source)))
  (define shared-object-target
    (build-path object-target-path (append-extension-suffix extensionless-source)))
  (make-directory* object-target-path)
  (compile-extension #t c-source object-target '())
  (link-extension #t (list object-target) shared-object-target))

(define-syntax from-c
  (make-require-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ c-source:str)
        (define f (syntax-e #'c-source))
        (compile-c-module f)
        (expand-import (datum->syntax stx (path->string (path-replace-suffix f ""))))]))))
