#lang racket/base

;; Simple utility for searching zo structs.
;; Explores the current struct's fields recursively for a exact string match.

(provide
 ;; Search a struct recursively for member zo-structs matching a string.
 zo-find)

(require (only-in racket/list empty?)
         (only-in racket/string string-split string-trim)
         (only-in compiler/zo-structs zo?)
         (only-in "zo-transition.rkt" transition)
         (only-in "zo-string.rkt" zo->spec spec/c))

;; -----------------------------------------------------------------------------

;; -- API functions

;; Searches a zo-struct `z` recursively for member zo-structs matching the `s`.
;; Search terminates after at most `#:limit` recursive calls.
;; Returns a list of matching zo-structs, excluding the root struct `z`.
(define (zo-find z str #:limit [lim 10000])
  ;; (-> zo? string? (listof zo?))
  (apply append
         (let-values ([(_ children) (parse-zo z)])
           (for/list ([z* children])
             (zo-find-aux z* str 0 lim)))))

;; -- private functions

;; Recursive helper for `zo-find`.
;; Add the current struct to the results, if it matches.
;; Check struct members for matches unless the search has reached its limit.
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

;; Return the name of the zo `z` and a list of its child zo structs.
;; Uses `zo-string.rkt` to parse a raw struct.
(define (parse-zo z)
  ;; (-> zo? (values string? (listof zo?)))
  (define z-spec     (zo->spec z))
  (define title      (car z-spec))
  (define child-strs (for/list ([pair (cdr z-spec)]) (car pair)))
  (values title (get-children z child-strs)))

;; Given a zo `z` and list of possible field names `strs`, return the list
;; of zo-structs obtained by looking up each name in `strs` in the struct `z`.
;; Relies on `zo-transition.rkt` to do the lookup.
(define (get-children z strs)
  ;; (-> zo? string? (listof zo?))
  (cond [(empty? strs) '()]
        ;; TODO 2015-01-12: is this check necessary?
        ;; [(string=? (car strs) "dummy") (get-children z (cdr strs))]
        [else (let-values ([(r success?) (transition z (car strs))])
                (cond [(not success?) (get-children z (cdr strs))]
                      [(list? r)      (append (filter zo? r)
                                              (get-children z (cdr strs)))]
                      [(zo?   r)      (cons r
                                            (get-children z (cdr strs)))]))]))

;; -- testing

(module+ test
  (require rackunit)
  (check-equal? #t #t)
)
