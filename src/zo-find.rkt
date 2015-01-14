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

;; --- API functions

;; Searches a zo-struct `z` recursively for member zo-structs matching the `s`.
;; Search terminates after at most `#:limit` recursive calls.
;; Returns a list of matching zo-structs, excluding the root struct `z`.
(define (zo-find z str #:limit [lim 10000])
  ;; (-> zo? string? (listof zo?))
  (apply append
         (let-values ([(_ children) (parse-zo z)])
           (for/list ([z* children])
             (zo-find-aux z* str 1 lim)))))

;; --- private functions

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
        [else (let-values ([(r success?) (transition z (car strs))])
                (cond [(not success?) (get-children z (cdr strs))]
                      [(list? r)      (append (filter zo? r)
                                              (get-children z (cdr strs)))]
                      [(zo?   r)      (cons r
                                            (get-children z (cdr strs)))]))]))

;; -----------------------------------------------------------------------------
;; --- testing

(module+ test
  (require rackunit
           compiler/zo-structs)

  ;; --- API
  ;; Success, one search path
  (let* ([z   (branch #t #t (branch #t #t (branch #t #t (branch #t #t #t))))]
         [arg "branch"]
         [res (zo-find z arg)])
    (begin (check-equal? (length res) 3)
           (check-equal? (car res) (branch-else z))))

  ;; Success, #:limit-ed results
  (let* ([z   (branch #t #t (branch #t #t (branch #t #t (branch #t #t #t))))]
         [arg "branch"]
         [res (zo-find z arg #:limit 2)])
    (begin (check-equal? (length res) 2)
           (check-equal? (cadr res) (branch-else (branch-else z)))))

  ;; Fail, no results
  (let* ([z (primval 8)]
         [arg "apply-values"]
         [res (zo-find z arg)])
    (check-equal? res '()))

  ;; Fail, search excludes root
  (let* ([z (primval 8)]
         [arg "primval"]
         [res (zo-find z arg)])
    (check-equal? res '()))

  ;; --- private
  ;; -- find-aux
  ;; Success, search INCLUDES root
  (let* ([z (primval 8)]
         [arg "primval"]
         [res (zo-find-aux z arg 1 10)])
    (check-equal? res (list z)))

  ;; Failure, search at limit
  (let* ([z (primval 8)]
         [arg "primval"]
         [res (zo-find-aux z arg 9 9)])
    (check-equal? res (list z)))

  ;; Failure, search past limit
  (let* ([z (primval 8)]
         [arg "primval"]
         [res (zo-find-aux z arg 9 1)])
    (check-equal? res (list z)))

  ;; Success, searching a few branches
  (let* ([tgt (inline-variant (branch #f #f #f) (branch #f #f #f))]
         [z   (with-cont-mark (let-one (boxenv 7 #f) (localref #t 1 #t #t #f) #f #t)
                              (seq (list tgt))
                              #f)]
         [arg "inline-variant"]
         [res (zo-find-aux z arg 1 10)])
    (check-equal? res (list tgt)))

  ;; Success, find multiple results
  (let* ([tgt (topsyntax 1 2 3)]
         [z   (application (beg0 (list (beg0 (list tgt))))
                           (list (primval 3) (primval 4) tgt tgt))]
         [arg "topsyntax"]
         [res (zo-find-aux z arg 1 10)])
    (check-equal? res (list tgt tgt tgt)))

  ;; -- parse-zo
  ;; Simple zo, no interesting fields
  (let ([z (topsyntax 1 2 3)])
    (let-values ([(title children) (parse-zo z)])
      (begin (check-equal? title             "topsyntax")
             (check-equal? (length children) 0))))

  ;; Three interesting fields
  (let ([z (branch (branch #t #t #t) (branch #t #t #f) (branch #t #f #f))])
    (let-values ([(title children) (parse-zo z)])
      (begin (check-equal? title             "branch")
             (check-equal? (length children) 3)
             (check-equal? (car children)    (branch #t #t #t)))))

  ;; 2 of 3 fields are interesting
  (let ([z (branch #f (branch #t #t #f) (branch #t #f #f))])
    (let-values ([(title children) (parse-zo z)])
      (begin (check-equal? title             "branch")
             (check-equal? (length children) 2)
             (check-equal? (car children)    (branch #t #t #f)))))

  ;; Nested children are not returned
  (let* ([tgt (beg0 (list (beg0 '())))]
         [z (apply-values tgt
                          (assign (toplevel 1 1 #t #t) #f #f))])
    (let-values ([(title children) (parse-zo z)])
      (begin (check-equal? title            "apply-values")
             (check-equal? (length children) 2)
             (check-equal? (car children)    tgt))))

  ;; -- get-children
  ;; Two valid fields, only 1 result
  (let* ([tgt  (toplevel 1 1 #t #f)]
         [z    (def-values (list 'A 'B 'C tgt) #f)]
         [args (list "ids" "rhs")]
         [res  (get-children z args)])
    (begin (check-equal? (length res) 1)
           (check-equal? (car res) tgt)))

  ;; Two fields, 2 results
  (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
         [z    (inline-variant tgt (let-rec '() #f))]
         [args (list "inline" "direct")]
         [res  (get-children z args)])
    (begin (check-equal? (length res) 2)
           (check-pred (lambda (x) (memq tgt res)) '())))

  ;; Only search 1 of 2 possible fields
  (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
         [z    (inline-variant tgt (let-rec '() #f))]
         [args (list "direct")]
         [res  (get-children z args)])
    (begin (check-equal? (length res) 1)
           (check-equal? (car res) tgt)))

  ;; Failure, search no valid fields
  (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
         [z    (inline-variant tgt (let-rec '() #f))]
         [args '()]
         [res  (get-children z args)])
    (check-equal? (length res) 0))

  (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
         [z    (inline-variant tgt (let-rec '() #f))]
         [args (list "outline" "NOTHING")]
         [res  (get-children z args)])
    (check-equal? (length res) 0))

  ;; Failure, no fields are zo
  (let* ([z    (let-void 777 #f 'NOTHING)]
         [args (list "count" "boxes?" "body" "something" "anything" "zo")]
         [res  (get-children z args)])
    (check-equal? (length res) 0))
)
