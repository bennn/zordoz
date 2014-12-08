#lang racket/base

;; Count the AST nodes that appear in a bytecode file
;;
(provide count-structs
         zsc->string)

(require compiler/zo-structs)

;; [zsc] (Short for "zo-struct-counter")
;; Each field names a zo-struct.
;; Field names look like "num-X" where "X" is the name of the zo-struct.
;; Each value is a natural number representing the number of times this struct appeared.
(struct zsc
  (num-zo
   num-compilation-top
   num-form
   ;; Shapes
   num-function-shape
   num-struct-shape
   num-constructor-shape
   num-predicate-shape
   num-accessor-shape
   num-mutator-shape
   num-type-shape
   num-other-shape
   ;;
   num-global-bucket
   num-module-variable
   ;;
   num-wrap
   num-wrapped
   ;;
   num-stx
   num-prefix
   ;;
   num-expr
   ;;
   num-provided
   num-toplevel
   num-seq
   num-seq-for-syntax
   num-inline-variant
   num-def-values
   num-def-syntaxes
   num-mod
   num-lam
   num-closure
   num-case-lam
   num-let-one
   num-let-void
   num-install-value
   num-let-rec
   num-boxenv
   num-localref
   num-topsyntax
   num-application
   num-branch
   num-with-cont-mark
   num-beg0
   num-splice
   num-req
   num-free-id-info
   num-lexical-rename
   num-phase-shift
   num-wrap-mark
   num-prune
   num-all-from-module
   num-nominal-path
   num-simple-nominal-path
   num-imported-nominal-path
   num-phased-nominal-path
   num-module-binding
   num-phased-module-binding
   num-exported-nominal-module-binding
   num-nominal-module-binding
   num-exported-module-binding
   num-simple-module-binding
   num-module-rename
   num-top-level-rename
   num-mark-barrier
  )
  #:mutable)

;; Syntax to increment a [zsc] field.
;; "(zsc++ s f)" desugars to "(set-zsc-f! s (add1 (zsc-f s)))"
(require (for-syntax racket/base racket/syntax))
(define-syntax (zsc++ stx)
  (syntax-case stx ()
    [(_)       (raise-syntax-error #f "[zsc++] Expected (struct field-name)")]
    [(_ _)     (raise-syntax-error #f "[zsc++] Expected (struct field-name)")]
    [(_ st fd) (with-syntax* ([setter (format-id stx "set-zsc-~a!" #'fd)]
                              [getter (format-id stx "zsc-~a"      #'fd)])
                             #'(setter st (add1 (getter st))))]))


;; Analyze input from zo-parse. Count the number of structs appearing in the bytecode.
(define (count-structs ct)
  ;; (-> compilation-top? zsc?)
  (error "Not implemented"))

;; Pretty-print the results of [count-structs]. 
(define (zsc->string z)
  ;; (-> zsc? string?)
  (error "Not implemented"))
