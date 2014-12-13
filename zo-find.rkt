#lang racket/base

(provide zo-find)

(require (only-in racket/list empty?)
         (only-in racket/string string-split string-trim)
         (only-in compiler/zo-structs zo?)
         "zo-transition.rkt"
         "zo-string.rkt")

;; -- API functions

(define (zo-find z str #:limit [lim 10000])
  ;; (-> zo? string? (listof zo?))
  ; get children, start a search at each child
  (define results
    (let-values ([(title children) (parse-zo z)])
      (for/list ([z* children])
        (zo-find-aux z* str 0 lim))))
  (apply append results))

;; -- private functions

(define (zo-find-aux z str i lim)
  (define-values (title children) (parse-zo z))
  (define results
    (cond [(<= lim i) '()]
          [else (apply append (for/list ([z* children])
                  (zo-find-aux z* str (add1 i) lim)))]))
  (if (string=? str title)
      (cons z results)
      results))

(define (parse-zo z)
  ;; (-> zo? (values string? (listof zo?)))
  (define-values (title child-strs) (parse-zo-string (zo->string z)))
  (values title (get-children z child-strs)))

(define (get-children z strs)
  ;; (-> zo? string? (listof zo?))
  (cond [(empty? strs) '()]
        [(string=? (car strs) "dummy") (get-children z (cdr strs))]
        [else (let-values ([(r success?) (transition z (car strs))])
                (cond [(not success?) (get-children z (cdr strs))]
                      [(list? r) (append r (get-children z (cdr strs)))]
                      [(zo?   r) (cons   r (get-children z (cdr strs)))]))]))

;; Deconstruct a printed zo to a string.
(define (parse-zo-string str)
  ;; (-> string? (values string? (listof string?)))
  (define lines (string-split str "\n"))
  (values (car (string-split (cadr (string-split (car lines) ":")) ">"))
          (for/list ([field-str (cdr lines)])
            (string-trim (car (string-split field-str ":"))))))
