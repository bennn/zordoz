#lang racket/base

;; Macro abstracting dispatch tables for zo structs.
;; Given an action and a list of struct names, create a cond
;; table checking predicates (build from each name) and
;; applying actions (derived by combining the action and the names)

(provide
 ;; Create a dispatch table from an action and list of names
 make-table)

(require (for-syntax racket/base syntax/parse racket/syntax)
         pgmp/exclusive-cond)

;; --------------------------------------------------------------------------------

;; Create a dispatch table from an action and 
(define-syntax (make-table stx)
  (syntax-parse stx
    [(_ (~seq #:action act) ids:id ...)
     #:with (ids? ...) #`(#,@(for/list ([i (syntax->list #'(ids ...))])
                              (format-id stx "~a?" i)))
     #:with (ids* ...) #`(#,@(for/list ([i (syntax->list #'(ids ...))])
                              (format-id stx "~a~a" i #'act)))
     (quasisyntax/loc stx
       (λ (z . a)
         (exclusive-cond
           [(ids? z) (apply ids* (cons z a))]
           ...
           [else #f])))]))
