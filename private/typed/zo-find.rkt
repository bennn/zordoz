#lang typed/racket/base

;; Simple utility for searching zo structs.
;; Explores the current struct's fields recursively for a exact string match.

(provide
 ;; (-> zo String [#:limit (U Natural #f)] (Listof result))
 ;; Search a struct recursively for member zo-structs matching a string.
 zo-find
 ;; Search result: a zo-struct and the path to reach it
 (struct-out result))

(require racket/match
         (only-in racket/list empty?)
         (only-in racket/string string-split string-trim)
         "typed-zo-structs.rkt"
         (only-in "zo-transition.rkt" zo-transition)
         (only-in "zo-string.rkt" zo->spec))

;; -----------------------------------------------------------------------------

;; --- API functions

(struct result ([z : zo]
                [path : (Listof zo)])
        #:transparent)

(: append-all (All (A) (-> (Listof (Listof A)) (Listof A))))
(define (append-all xss)
  (cond [(empty? xss) '()]
        [else (append (car xss) (append-all (cdr xss)))]))

;; Searches a zo-struct `z` recursively for member zo-structs matching the `s`.
;; Terminates after at most `#:limit` recursive calls.
;; Return a list of 'result' structs.
(: zo-find (-> zo String [#:limit (U Natural #f)] (Listof result)))
(define (zo-find z str #:limit [lim #f])
  ;; (-> zo? string? (listof result?))
  (define-values (_ children) (parse-zo z))
  (append-all (for/list : (Listof (Listof result)) ([z* : zo children])
                        (zo-find-aux z* '() str 1 lim '()))))

;; ;; --- private functions

;; Check if `str` is one of the known looping zo-structs.
;; 2015-01-23: So far as I know, only closures may loop.
;; 2015-07-29: New macro expander => scope and multi-scope can loop
(: may-loop? (-> String Boolean))
(define (may-loop? str)
  (if (member str '("closure" "multi-scope" "scope"))
      #t #f))

;; Recursive helper for `zo-find`.
;; Add the current struct to the results, if it matches.
;; Check struct members for matches unless the search has reached its limit.
(: zo-find-aux (-> zo (Listof zo) String Natural (U Natural #f) (Listof String) (Listof result)))
(define (zo-find-aux z hist str i lim seen)
  (define-values (title children) (parse-zo z))
  (define zstr (format "~a" z))
  (: results (Listof result))
  (define results
    (cond
     [(and lim (<= lim i))
      '()]
     ;; Terminate search if we're seeing a node for the second time
     [(and (may-loop? title) (member zstr seen))
      '()]
     [else
      ;; Remember current node if we might see it again.
      (: seen* (Listof String))
      (define seen* (if (may-loop? title) (cons zstr seen) seen))
      (: hist* (Listof zo))
      (define hist* (cons z hist))
      (append-all (for/list : (Listof (Listof result)) ([z* : zo children])
                              (zo-find-aux z* hist* str (add1 i) lim seen*)))]))
  (if (and (string=? str title) (not (member z seen)))
      (cons (result z hist) results)
      results))

;; Return the name of the zo `z` and a list of its child zo structs.
;; Uses `zo-string.rkt` to parse a raw struct.
(: parse-zo (-> zo (values String (Listof zo))))
(define (parse-zo z)
  (: z-spec Spec)
  (define z-spec     (zo->spec z))
  (: title String)
  (define title      (car z-spec))
  (: child-strs (Listof String))
  (define child-strs (for/list : (Listof String) ([pair : (Pair String (-> (U String Spec))) (cdr z-spec)])
                               (car pair)))
  (values title (get-children z child-strs)))

;; Given a zo `z` and list of possible field names `strs`, return the list
;; of zo-structs obtained by looking up each name in `strs` in the struct `z`.
;; Relies on `zo-transition.rkt` to do the lookup.
(: get-children (-> zo (Listof String) (Listof zo)))
(define (get-children z strs)
  (match strs
    ['() '()]
    [(cons hd tl)
     (define-values (r* success*) (zo-transition z hd))
     (: r (U zo (Listof zo)))
     (define r r*)
     (: success? Boolean)
     (define success? success*)
     (cond [(not success?) (get-children z tl)]
           [(list? r)      (append (filter zo? r) (get-children z tl))]
           [(zo?   r)      (cons r (get-children z tl))])]))
