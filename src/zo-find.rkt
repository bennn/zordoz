#lang racket/base

(provide zo-find)

(require (only-in racket/list empty?)
         (only-in racket/string string-split string-trim)
         (only-in compiler/zo-structs zo?)
         "zo-transition.rkt"
         (only-in "zo-string.rkt" zo->spec spec/c))

;; -- API functions

(define (zo-find z str #:limit [lim 10000])
  ;; (-> zo? string? (listof zo?))
  (apply append
         (let-values ([(_ children) (parse-zo z)])
           (for/list ([z* children])
             (zo-find-aux z* str 0 lim)))))

;; -- private functions

(define (zo-find-aux z str i lim)
  (define-values (title children) (parse-zo z))
  (define results
    (cond [(<= lim i) '()]
          [else (apply append
                       (for/list ([z* children])
                         (zo-find-aux z* str (add1 i) lim)))]))
  (if (string=? str title)
      (cons z results)
      results))

;; Return the name of the zo [z] and a list of its child zo structs.
(define (parse-zo z)
  ;; (-> zo? (values string? (listof zo?)))
  (define z-spec     (zo->spec z))
  (define title      (car z-spec))
  (define child-strs (for/list ([pair (cdr z-spec)]) (car pair)))
  (values title (get-children z child-strs)))

;; Given a zo [z] and list of possible field names [strs], return the list of zo-structs
;; obtained by looking up each name in [strs] in the struct [z].
(define (get-children z strs)
  ;; (-> zo? string? (listof zo?))
  (cond [(empty? strs) '()]
        ;; TODO necessary?
        ;; [(string=? (car strs) "dummy") (get-children z (cdr strs))]
        [else (let-values ([(r success?) (transition z (car strs))])
                (cond [(not success?) (get-children z (cdr strs))]
                      [(list? r) (append (filter zo? r) (get-children z (cdr strs)))]
                      [(zo?   r) (cons   r (get-children z (cdr strs)))]))]))
