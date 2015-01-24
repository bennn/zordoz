#lang racket/base

;; Macro abstracting dispatch tables:
;; - accept an argument,
;; - check it against some predicates,
;; - if a predicate matches, call an associated function

(provide
 ;; Create a dispatch table from a predicate, action, and arguments.
 make-table)

(require (for-syntax racket/base racket/syntax))

;; --------------------------------------------------------------------------------

;; Create a dispatch table from a set of predicates and actions
(define-syntax-rule
  (make-table [pred action] ...)
  (lambda (z . args)
    (cond
     [(pred z) (apply action (cons z args))]
     ...
     [else #f])))
