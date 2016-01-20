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

(define-for-syntax-and-runtime object-target-path
  (build-path "compiled" "native" (system-library-subpath)))

(define-for-syntax-and-runtime (compile-c-module c-source #:output-file [output-file #f])
  (define extensionless-source (path-replace-suffix c-source ""))
  (define object-target
    (if output-file
        (append-object-suffix output-file)
        (build-path object-target-path (append-object-suffix extensionless-source))))
  (define shared-object-target
    (if output-file
        (append-extension-suffix output-file)
        (build-path object-target-path (append-extension-suffix extensionless-source))))
  (make-directory* object-target-path)
  (compile-extension #t c-source object-target '())
  (link-extension #t (list object-target) shared-object-target))

(define-syntax from-c
  (make-require-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ c-source:str)
        (define in (syntax-e #'c-source))
        (define out (make-fresh-filename (path-replace-suffix in "")))
        (define cle (current-load-extension))
        (compile-c-module in #:output-file (build-path object-target-path out))
        (parameterize ([current-load-extension (lambda (p s)
                                                 (define-values (base name dir?)
                                                   (split-path (path-replace-suffix in "")))
                                                 (cle p (string->symbol (path->string name))))])
          (expand-import (datum->syntax stx out)))]))))

(define-for-syntax-and-runtime (make-fresh-filename file-base [version 0])
  (define filename (format "~a-~a" file-base version))
  (if (file-exists? (append-object-suffix filename))
      (make-fresh-filename file-base (add1 version))
      filename))
