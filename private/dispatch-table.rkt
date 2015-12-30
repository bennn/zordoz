#lang racket/base

;; Macro abstracting dispatch tables for zo structs.
;;
;; Given an action and a list of struct names, create a `cond` table to:
;; - check predicates  (built from each name)
;; - apply actions     (derived by combining the action and the names)

(provide
 make-table
 ;; Create a dispatch table from an action and list of names
)

;; --------------------------------------------------------------------------------

(require
  (for-syntax racket/base syntax/parse racket/syntax))

;; =============================================================================

;; Create a dispatch table from an action and 
(define-syntax (make-table stx)
  (syntax-parse stx
    [(_ (~seq #:action act) ids:id ...)
     #:with (ids? ...) #`(#,@(for/list ([i (syntax->list #'(ids ...))])
                              (format-id stx "~a?" i)))
     #:with (ids* ...) #`(#,@(for/list ([i (syntax->list #'(ids ...))])
                              (format-id stx "~a~a" i #'act)))
     #'(λ (z . a)
         (cond
           [(ids? z) (apply ids* (cons z a))]
           ...
           [else #f]))]))
