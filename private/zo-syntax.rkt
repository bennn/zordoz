#lang racket/base

;; Utilities for decompiling syntax fragments, rather than whole .zo files

;; This code brought to you by Leif Andersen.
;; https://github.com/LeifAndersen/racket-compiler-goodies

(provide
  syntax->zo
  ;; (-> Syntax zo)
  ;; Parse a syntax object as a zo struct

  syntax->decompile
  ;; (-> Syntax Any)
  ;; Decompile a syntax object into an S-expression

  zo->compiled-expression
  ;; (-> compilation-top Compiled-Expression)
  ;; Parse a zo struct (output of zo-parse) as an S-expression
)

;; -----------------------------------------------------------------------------

(require compiler/zo-parse
         compiler/zo-marshal
         compiler/decompile
         (only-in racket/port with-input-from-bytes port->bytes))

;; =============================================================================

(define (syntax->zo stx)
  (define-values (in out) (make-pipe))
  (display (compile-syntax stx) out)
  (close-output-port out)
  (define y (port->bytes in))
  (close-input-port in)
  (zo-parse (open-input-bytes y)))

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
  (require rackunit)

  ;; -- syntax->zo
  (let* ([stx #'(+ 1 3)]
         [z (syntax->zo stx)])
   (check-true (compilation-top? z))
   (check-equal? (compilation-top-code z) 4))

  (let* ([stx #'(let ([a (box 'a)])
                  (if (unbox a) (set-box! a 'b) (set-box! a 'c)) (unbox a))]
         [z (syntax->zo stx)])
    (check-true (compilation-top? z))
    (define l (compilation-top-code z))
    (check-true (let-one? l))
    ;; --- rhs
    (define rhs (let-one-rhs l))
    (check-true (application? rhs))
    (define rator (application-rator rhs))
    (check-true (primval? rator))
    (check-equal? (primval-id rator) 129)
    (check-equal? (application-rands rhs) '(a))
    ;; --- body
    (define body (let-one-body l))
    (check-true (seq? body))
    (check-true (branch? (car (seq-forms body))))
    (check-true (application? (cadr (seq-forms body)))))

  ;; -- syntax->decompile
  (let* ([stx #'(string-append "hello" "world")]
         [d (syntax->decompile stx)])
    (check-eq? (car d) 'begin)
    (check-equal? (cadr (cadr (caddr d))) "hello"))

  (let* ([stx #'(displayln "hello world")]
         [d (syntax->decompile stx)])
    (check-equal? (cadr (caddr d)) '(quote "hello world")))

  ;; -- zo->compiled-expression
  (let* ([p (prefix 0 '() '() 'a)]
         [z (compilation-top 0 (hash) p 666)]
         [c (zo->compiled-expression z)])
    (check-equal? (eval c) 666))

  (let* ([p (prefix 9 '() '() 'wepa)]
         [z (compilation-top 0 (hash) p (primval 129))]
         [c (zo->compiled-expression z)])
    (check-equal? (eval c) box))
)
