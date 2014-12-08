#lang racket/base

;; Count the AST nodes that appear in a bytecode file
;;
(provide count-structs
         zsc->string)

(require compiler/zo-structs
         racket/string)

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
  (define field-names (list "num-zo" "num-compilation-top" "num-form" "num-function-shape" "num-struct-shape" "num-constructor-shape" "num-predicate-shape" "num-accessor-shape" "num-mutator-shape" "num-type-shape" "num-other-shape" "num-global-bucket" "num-module-variable" "num-wrap" "num-wrapped" "num-stx" "num-prefix" "num-expr" "num-provided" "num-toplevel" "num-seq" "num-seq-for-syntax" "num-inline-variant" "num-def-values" "num-def-syntaxes" "num-mod" "num-lam" "num-closure" "num-case-lam" "num-let-one" "num-let-void" "num-install-value" "num-let-rec" "num-boxenv" "num-localref" "num-topsyntax" "num-application" "num-branch" "num-with-cont-mark" "num-beg0" "num-splice" "num-req" "num-free-id-info" "num-lexical-rename" "num-phase-shift" "num-wrap-mark" "num-prune" "num-all-from-module" "num-nominal-path" "num-simple-nominal-path" "num-imported-nominal-path" "num-phased-nominal-path" "num-module-binding" "num-phased-module-binding" "num-exported-nominal-module-binding" "num-nominal-module-binding" "num-exported-module-binding" "num-simple-module-binding" "num-module-rename" "num-top-level-rename" "num-mark-barrier"))
  (define counts      (list (zsc-num-zo z) (zsc-num-compilation-top z) (zsc-num-form z) (zsc-num-function-shape z) (zsc-num-struct-shape z) (zsc-num-constructor-shape z) (zsc-num-predicate-shape z) (zsc-num-accessor-shape z) (zsc-num-mutator-shape z) (zsc-num-type-shape z) (zsc-num-other-shape z) (zsc-num-global-bucket z) (zsc-num-module-variable z) (zsc-num-wrap z) (zsc-num-wrapped z) (zsc-num-stx z) (zsc-num-prefix z) (zsc-num-expr z) (zsc-num-provided z) (zsc-num-toplevel z) (zsc-num-seq z) (zsc-num-seq-for-syntax z) (zsc-num-inline-variant z) (zsc-num-def-values z) (zsc-num-def-syntaxes z) (zsc-num-mod z) (zsc-num-lam z) (zsc-num-closure z) (zsc-num-case-lam z) (zsc-num-let-one z) (zsc-num-let-void z) (zsc-num-install-value z) (zsc-num-let-rec z) (zsc-num-boxenv z) (zsc-num-localref z) (zsc-num-topsyntax z) (zsc-num-application z) (zsc-num-branch z) (zsc-num-with-cont-mark z) (zsc-num-beg0 z) (zsc-num-splice z) (zsc-num-req z) (zsc-num-free-id-info z) (zsc-num-lexical-rename z) (zsc-num-phase-shift z) (zsc-num-wrap-mark z) (zsc-num-prune z) (zsc-num-all-from-module z) (zsc-num-nominal-path z) (zsc-num-simple-nominal-path z) (zsc-num-imported-nominal-path z) (zsc-num-phased-nominal-path z) (zsc-num-module-binding z) (zsc-num-phased-module-binding z) (zsc-num-exported-nominal-module-binding z) (zsc-num-nominal-module-binding z) (zsc-num-exported-module-binding z) (zsc-num-simple-module-binding z) (zsc-num-module-rename z) (zsc-num-top-level-rename z) (zsc-num-mark-barrier z)))
  (string-join (map (lambda (f c) (format "~a\t~a" f c)) field-names counts) "\n"))

(define (zsc-init)
  (zsc 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
