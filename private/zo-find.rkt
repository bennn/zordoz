#lang racket/base

;; Simple utility for searching zo structs.
;; Explores the current struct's fields recursively for a exact string match.

(provide
 zo-find
 ;; (->* [zo? string?] [#:limit (or/c natural-number/c #f)] (listof result?))
 ;; Search a struct recursively for member zo-structs matching a string.

 result result? result-zo result-path
 ;; Search result: a zo-struct and the path to reach it
)

;; -----------------------------------------------------------------------------

(require
  (only-in racket/string string-split string-trim)
  (only-in compiler/zo-structs zo?)
  (only-in zordoz/private/zo-transition zo-transition)
  (only-in zordoz/private/zo-string zo->spec spec/c)
  (only-in racket/list append*)
  racket/match)

;; =============================================================================

;; --- API functions

(struct result (zo path) #:transparent)

;; Searches a zo-struct `z` recursively for member zo-structs matching the `s`.
;; Terminates after at most `#:limit` recursive calls.
;; Return a list of 'result' structs.
(define (zo-find z str #:limit [lim #f])
  ;; (-> zo? string? (listof result?))
  (define-values (_ children) (parse-zo z))
  (append* (for/list ([z* children]) (zo-find-aux z* '() str 1 lim))))

;; --- private functions

;; Check if `str` is one of the known looping zo-structs.
;; 2015-01-23: So far as I know, only closures may loop.
;; 2015-07-29: New macro expander => scope and multi-scope can loop
(define (may-loop? str)
  ;; (-> string? boolean?)
  (member str (list "closure" "scope" "multi-scope")))

;; Recursive helper for `zo-find`.
;; Add the current struct to the results, if it matches.
;; Check struct members for matches unless the search has reached its limit.
(define (zo-find-aux z hist str i lim)
  (define-values (title children) (parse-zo z))
  (define zstr (format "~a" z))
  (define results
    (cond
     [(and lim (<= lim i))
      '()]
     ;; Terminate search if we're seeing a node for the second time
     [(and (may-loop? title) (memq z hist))
      '()]
     [else
      ;; Remember current node if we might see it again.
      (define hist* (cons z hist))
      (apply append (for/list ([z* children]) (zo-find-aux z* hist* str (add1 i) lim)))]))
  (if (and (string=? str title) (not (memq z (map result-zo results))))
      (cons (result z hist) results)
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
  (match strs
    ['() '()]
    [(cons hd tl)
     (define-values (r success?) (zo-transition z hd))
     (cond [(not success?) (get-children z tl)]
           [(list? r)      (append (filter zo? r) (get-children z tl))]
           [(hash? r)      (append (filter zo? (hash-values r)) (get-children z tl))]
           [(zo?   r)      (cons r (get-children z tl))])]))

;; =============================================================================
;; --- testing

(module+ test
  (require rackunit
           (only-in "zo-string.rkt" zo->string)
           compiler/zo-structs)

  ;; --- API
  (test-case "Success, one search path"
    (let* ([z   (branch #t #t (branch #t #t (branch #t #t (branch #t #t #t))))]
           [arg "branch"]
           [res (zo-find z arg)])
      (begin (check-equal? (length res) 3)
             (check-equal? (result-zo (car res))   (branch-else z))
             (check-equal? (result-path (car res)) '()))))

  (test-case "Success, #:limit-ed results"
    (let* ([z   (branch #t #t (branch #t #t (branch #t #t (branch #t #t #t))))]
           [arg "branch"]
           [res (zo-find z arg #:limit 2)])
      (begin (check-equal? (length res) 2)
             (check-equal? (result-zo (cadr res)) (branch-else (branch-else z)))
             (check-equal? (result-path (cadr res)) (list (branch-else z))))))

  (test-case "Fail, no results"
    (let* ([z (primval 8)]
           [arg "apply-values"]
           [res (zo-find z arg)])
      (check-equal? res '())))

  (test-case "Fail, search excludes root"
    (let* ([z (primval 8)]
           [arg "primval"]
           [res (zo-find z arg)])
      (check-equal? res '())))

  ;; --- private
  (test-case "-- find-aux"
    ;; Success, search INCLUDES root (empty history)
    (let* ([z (primval 8)]
           [arg "primval"]
           [res (zo-find-aux z '() arg 1 10)])
      (begin (check-equal? (length res) 1)
             (check-equal? (result-zo (car res)) z)
             (check-equal? (result-path (car res)) '()))))

  (test-case "Success, search INCLUDES root (make sure history is passed in result)"
    (let* ([z (primval 8)]
           [arg "primval"]
           [hist '(a b c d)]
           [res (zo-find-aux z hist arg 1 10)])
      (begin (check-equal? (result-zo (car res)) z)
             (check-equal? (result-path (car res)) hist))))

  (test-case "Failure, search at limit (remember, find-aux searches the head)"
    (let* ([z (branch #t #t (primval 8))]
           [arg "primval"]
           [hist '()]
           [res (zo-find-aux z hist arg 9 9)])
      (check-equal? res '())))

  (test-case "Failure, search past limit"
    (let* ([z (branch #t #t (primval 8))]
           [arg "primval"]
           [hist '()]
           [res (zo-find-aux z hist arg 9 1)])
      (check-equal? res '())))

  (test-case "find nothing, malformed zo"
    (let* ([tgt (inline-variant (branch #f #f #f) (branch #f #f #f))]
           [z   (with-cont-mark (let-one (boxenv 7 #f) (localref #t 1 #t #t #f) #f #t)
                                (seq (list tgt))
                                #f)]
           [arg "inline-variant"]
           [hist '(a b)]
           [res (zo-find-aux z hist arg 1 10)])
      (check-equal? (length res) 0)))

  (test-case "Success, searching a few branches"
    (let* ([tgt (branch #f #f #f)]
           [z   (with-cont-mark (let-one (boxenv 7 #f) (localref #t 1 #t #t #f) #f #t)
                                (seq (list tgt))
                                #f)]
           [arg "branch"]
           [hist '(a b)]
           [res (zo-find-aux z hist arg 1 10)])
      (begin (check-equal? (length res) 1)
             (check-equal? (result-zo (car res)) tgt)
             (check-equal? (result-path (car res)) (cons (with-cont-mark-val z) (cons z hist))))))

  (test-case "Success, find multiple results"
    (let* ([tgt (toplevel 1 2 #t #f)]
           [z   (application (beg0 (list (beg0 (list tgt))))
                             (list (primval 3) (primval 4) tgt tgt))]
           [arg "toplevel"]
           [hist '(a b c)]
           [res (zo-find-aux z hist arg 1 10)])
      (begin (check-equal? (length res) 3)
             (check-equal? (result-zo (car res)) tgt)
             (check-equal? (result-zo (cadr res)) tgt)
             (check-equal? (result-zo (caddr res)) tgt)
             ;; Verify one history
             (check-equal? (result-path (car res)) (cons (car (beg0-seq (application-rator z)))
                                                         (cons (application-rator z)
                                                               (cons z hist)))))))

  (test-case "Success, it's a closure but we have not seen it"
    (let* ([z (closure (lam 'N '() 0 '() #f '#() '() #f 0 #f) 'C)]
           [arg "lam"]
           [res (zo-find-aux z '() arg 1 10)])
      (begin (check-equal? (length res) 1)
             (check-equal? (result-zo (car res)) (closure-code z)))))

  (test-case "-- parse-zo"
    ;; Simple zo, no interesting fields
    (let ([z (toplevel 1 2 #t #t)])
      (let-values ([(title children) (parse-zo z)])
        (begin (check-equal? title "toplevel")
               (check-equal? (length children) 0)))))

  (test-case "Three interesting fields"
    (let ([z (branch (branch #t #t #t) (branch #t #t #f) (branch #t #f #f))])
      (let-values ([(title children) (parse-zo z)])
        (begin (check-equal? title             "branch")
               (check-equal? (length children) 3)
               (check-equal? (car children)    (branch #t #t #t))))))

  (test-case "2 of 3 fields are interesting"
    (let ([z (branch #f (branch #t #t #f) (branch #t #f #f))])
      (let-values ([(title children) (parse-zo z)])
        (begin (check-equal? title             "branch")
               (check-equal? (length children) 2)
               (check-equal? (car children)    (branch #t #t #f))))))

  (test-case "Nested children are not returned"
    (let* ([tgt (beg0 (list (beg0 '())))]
           [z (apply-values tgt
                            (assign (toplevel 1 1 #t #t) #f #f))])
      (let-values ([(title children) (parse-zo z)])
        (begin (check-equal? title            "apply-values")
               (check-equal? (length children) 2)
               (check-equal? (car children)    tgt)))))

  (test-case "-- get-children"
    ;; Two valid fields, only 1 result
    (let* ([tgt  (toplevel 1 1 #t #f)]
           [z    (def-values (list 'A 'B 'C tgt) #f)]
           [args (list "ids" "rhs")]
           [res  (get-children z args)])
      (begin (check-equal? (length res) 1)
             (check-equal? (car res) tgt)))
    (let* ([tgt  (toplevel 1 1 #t #f)]
           [z    (linkl-bundle (make-hash (list (cons 0 tgt) (cons 1 2))))]
           [args (list "table")]
           [res  (get-children z args)])
      (begin (check-equal? (length res) 1)
             (check-equal? (car res) tgt))))

  (test-case "Two fields, 2 results"
    (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
           [z    (inline-variant tgt (let-rec '() #f))]
           [args (list "inline" "direct")]
           [res  (get-children z args)])
      (begin (check-equal? (length res) 2)
             (check-pred (lambda (x) (memq tgt res)) '()))))

  (test-case "Only search 1 of 2 possible fields"
    (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
           [z    (inline-variant tgt (let-rec '() #f))]
           [args (list "direct")]
           [res  (get-children z args)])
      (begin (check-equal? (length res) 1)
             (check-equal? (car res) tgt))))

  (test-case "Failure, search no valid fields"
    (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
           [z    (inline-variant tgt (let-rec '() #f))]
           [args '()]
           [res  (get-children z args)])
      (check-equal? (length res) 0)))

  (let* ([tgt  (lam 'name '() 0 '() #f '#() '() #f 0 #f)]
         [z    (inline-variant tgt (let-rec '() #f))]
         [args (list "outline" "NOTHING")]
         [res  (get-children z args)])
    (check-equal? (length res) 0))

  (test-case "Failure, no fields are zo"
    (let* ([z    (let-void 777 #f 'NOTHING)]
           [args (list "count" "boxes?" "body" "something" "anything" "zo")]
           [res  (get-children z args)])
      (check-equal? (length res) 0)))
)
