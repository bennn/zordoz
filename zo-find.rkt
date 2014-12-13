#lang racket/base

(provide zo-find)

;; -- API functions

(define (zo-find z str #:limit [lim 10000])
  ;; (-> zo? string? (listof zo?))
  ; get children, start a search at each child
  (define results
    (for/list ([z* (get-children z)])
      (zo-find-aux z str 0 lim)))
  (apply append results))

;; -- private functions

(define (zo-find z str i lim)
  ;; dammit, how to do this. Need to transition and search
  ;; OOOH for each field in zo->string output (excluding dummies)
  ;;  add if the field's type matches [str]. Also, zo-transition into that field and
  ;;  keep searching.
  (cond [(< lim i) acc] ;; Search has gone too far
        [else '()]
  ))

;; Deconstruct a printed zo to a string. TODO make this less hack.
(define (parse-string str)
  ;; (-> string? (cons/c string? (listof string?)))
  (cons "" '()))

(define (get-children z)
  ;; (-> zo? (listof zo?))
  '())
