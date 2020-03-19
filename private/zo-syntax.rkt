#lang racket/base

;; Utilities for decompiling syntax fragments, rather than whole .zo files

;; This code brought to you by Leif Andersen.
;; https://github.com/LeifAndersen/racket-compiler-goodies

(provide

  compiled-expression->zo
  ;; (-> Compiled-Expression compilation-top)
  ;; Turn a compiled expression into a zo struct

  syntax->zo
  ;; (-> Syntax zo)
  ;; Parse a syntax object as a zo struct

  syntax->decompile
  ;; (-> Syntax Any)
  ;; Decompile a syntax object into an S-expression

  toplevel-syntax->zo
  ;; (-> Syntax (Listof zo))
  ;; Convert a toplevel expression into a list of zo compilations

  zo->compiled-expression
  ;; (-> compilation-top Compiled-Expression)
  ;; Parse a zo struct (output of zo-parse) as an S-expression
)

;; -----------------------------------------------------------------------------

(require
  compiler/zo-parse
  compiler/zo-marshal
  compiler/decompile
  syntax/toplevel
  syntax/strip-context
  (only-in racket/port with-input-from-bytes port->bytes)
  (only-in racket/linklet linklet?)
)

;; =============================================================================

(define (compiled-expression->zo compiled)
  (define-values (in out) (make-pipe))
  (display compiled out)
  (close-output-port out)
  (define y (port->bytes in))
  (close-input-port in)
  (zo-parse (open-input-bytes y)))

(define (syntax->zo stx)
  (compiled-expression->zo (compile-syntax (expand-syntax-top-level-with-compile-time-evals stx))))

(define (toplevel-syntax->zo stx)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket/undefined)
    (namespace-require 'racket)
    (map compiled-expression->zo
         (eval-compile-time-part-of-top-level/compile
           (expand-syntax-top-level-with-compile-time-evals
             (namespace-syntax-introduce (strip-context stx)))))))

(define (syntax->decompile stx)
  (decompile (syntax->zo stx)))

(define (zo->compiled-expression zo)
  ;; read-accept-compiled tells the default reader to accept
  ;; compiled code (flagged with #~)
  (parameterize ([read-accept-compiled #t])
    (define x (zo-marshal zo))
    (with-input-from-bytes (zo-marshal zo)
      read)))

;; =============================================================================

(module+ test
  (require rackunit compiler/compile-file racket/runtime-path
           (only-in racket/port with-input-from-string)
           (only-in racket/extflonum extflonum-available?)
           (only-in syntax/modread with-module-reading-parameterization))

  (define-runtime-path test-rkt "test/file.rkt")
  (define-runtime-path test-zo "test/file.zo")
  (define racketcs? (eq? 'chez-scheme (system-type 'vm)))

  (define (machine-code-sexp? x)
    (and (pair? x)
         (eq? (car x) '#%machine-code)))

  (define (linkl-directory->code z)
    (linkl-body
      (hash-ref
        (linkl-bundle-table
          (hash-ref
            (linkl-directory-table z) '())) 0)))

  (test-case "-- compiled-expression->zo"
   (unless racketcs?
    (let* ([e (compile-syntax #'(box 3))]
           [z (compiled-expression->zo e)])
      (check-pred linkl-directory? z)
      (check-pred application? (car (linkl-directory->code z))))))

  (test-case "-- syntax->zo"
   (unless racketcs?
    (let* ([stx #'(+ 1 3)]
           [z (syntax->zo stx)])
     (check-pred linkl-directory? z)
     (check-equal? 4 (car (linkl-directory->code z))))))

  (test-case "syntax->zo 2"
   (unless racketcs?
    (let* ([stx #'(let ([a (box 'a)])
                    (if (unbox a) (set-box! a 'b) (set-box! a 'c)) (unbox a))]
           [z (syntax->zo stx)])
      (check-pred linkl-directory? z)
      (define l (car (linkl-directory->code z)))
      (check-true (let-one? l))
      ;; --- rhs
      (define rhs (let-one-rhs l))
      (check-true (application? rhs))
      (define rator (application-rator rhs))
      (check-true (primval? rator))
      (check-pred integer? (primval-id rator))
      (check-equal? (application-rands rhs) '(a))
      ;; --- body
      (define body (let-one-body l))
      (check-true (seq? body))
      (check-true (branch? (car (seq-forms body))))
      (check-true (application? (cadr (seq-forms body)))))))

  (test-case "-- syntax->decompile"
    (let* ([stx #'(string-append "hello" "world")]
           [d (syntax->decompile stx)])
      (if racketcs?
        (check-pred machine-code-sexp? d)
        (begin
          (check-eq? (car d) 'string-append)
          (check-equal? (car (cdr (car (cdr d)))) "hello"))))

    (let* ([stx #'(displayln "hello world")]
           [d (syntax->decompile stx)])
      (if racketcs?
        (check-pred machine-code-sexp? d)
        (check-equal? (car (cdr d)) '(quote "hello world")))))

  #;(test-case "-- zo->compiled-expression"
    (let* (;[expr '(+ 600 60 6)]
           [_ (with-output-to-file test-rkt #:exists 'replace
                (lambda () (displayln "#lang racket/base") (displayln expr)))]
           [_ (parameterize ([current-namespace (make-base-namespace)])
                (with-module-reading-parameterization (lambda () (compile-file test-rkt test-zo))))]
           [z (with-input-from-file test-zo zo-parse)]
           [c (zo->compiled-expression z)])
      (check-equal? (eval c (make-base-namespace)) 666))

    (let* ([p (prefix 9 '() '() 'wepa)]
           [box-id (primval-id (compilation-top-code (syntax->zo #'box)))]
           [z (compilation-top 0 (hash) p (primval box-id))]
           [c (zo->compiled-expression z)])
      (check-equal? (eval c) box)))
)
