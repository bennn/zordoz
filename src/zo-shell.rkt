#lang racket/base

;; Command-line UI for exploring decompiled bytecode.
;; (Use `raco make` to generate bytecode)

(provide
 ;; Start a REPL using command-line arguments
 init)

(require (only-in compiler/zo-parse zo? zo-parse)
         (only-in racket/string string-split string-join)
         (only-in racket/list   empty?)
         (only-in "zo-find.rkt" zo-find)
         (only-in "zo-string.rkt" zo->string)
         (only-in "zo-transition.rkt" transition))

;; --- constants & contracts

(define DEBUG   #f) ;when set, print extra debugging information
(define VERSION 0.5) ;for aesthetic purposes
(define VNAME   "outlands") ;also aesthetic
;; (define nat? natural-number/c)
;; (define context? (or/c zo? (listof zo?)))
;; (define history? (listof context?))

;; -----------------------------------------------------------------------------

;; --- API functions

;; Entry point to the REPL, expects command-line arguments passed as a list.
;; In the future, there may be more entry points.
(define (init args)
  ;; (-> (listof string?) void?)
  (cond [(empty? args)       (print-usage)]
        [(empty? (cdr args)) (init-from-filename (car args))]
        [else                (print-usage)]))

;; --- REPL

;; Start REPL from a filename
(define (init-from-filename name)
  ;; (-> string? void?)
  (print-info (format "Loading bytecode file '~a'..." name))
  (define port (open-input-file name))
  (print-info "Parsing bytecode...")
  (define ctx  (zo-parse port))
  (print-info "Parsing complete!")
  (init-repl ctx))

;; Print a welcoming message, initialize REPL history, enter the loop.
;; Outside functions should call this method instead of `repl` directly.
(define (init-repl ctx)
  ;; (-> context? void?)
  (print-welcome)
  (repl ctx '()))

;; The REPL loop. Process a command using context `ctx` and history `hist`.
(define (repl ctx hist)
  ;; (-> context? history? void?)
  (when DEBUG (print-history hist))
  (print-prompt)
  (define raw (read-line))
  ;; 2015-01-12: Command parsing is currently very simple.
  (cond [(back? raw) (cond [(empty? hist) (print-unknown raw)
                                          (repl ctx hist)]
                           [else          (call-with-values (lambda () (pop hist)) repl)])]
        [(dive? raw) (call-with-values (lambda () (dive ctx hist raw)) repl)]
        [(find? raw) (cond [(zo? ctx) (define ctx* (find ctx raw))
                                      (if (empty? ctx*)
                                          (repl ctx hist)
                                          (repl ctx* (push hist ctx)))]
                           [else      (print-unknown raw)
                                      (repl ctx hist)])]
        [(help? raw) (print-help)
                     (repl ctx hist)]
        [(info? raw) (print-context ctx)
                     (repl ctx hist)]
        [(quit? raw) (print-goodbye)]
        [else        (print-unknown raw)
                     (repl ctx hist)]))

;; --- command predicates

;; BACK
;; Takes no arguments
(define (back? raw)
  ;; (-> string? boolean?)
  (member raw (list "back" "b" "up" "u" ".." "../")))

;; DIVE ARG
;; Takes one argument, a string denoting the field to enter.
(define (dive? raw)
  ;; (-> string? boolean?)
  (define hd (if (memq #\space (string->list raw))
                 (car (string-split raw))
                 ""))
  (member hd (list "dive" "next" "step" "d")))

;; FIND ARG
;; Takes one argument, a string denoting the struct name to search for.
(define (find? raw)
  ;; (-> string? boolean?)
  (define hd (if (memq #\space (string->list raw))
                 (car (string-split raw))
                 ""))
  (member hd (list "find" "f" "query" "search" "look")))

;; HELP
;; Takes no arguments
(define (help? raw)
  ;; (-> string? boolean?)
  (member raw (list "help" "h" "--h" "--help" "-h" "-help" "fuck")))

;; INFO
;; Takes no arguments
(define (info? raw)
  ;; (-> string? boolean?)
  (member raw (list "info" "i" "print" "p" "show" "ls")))

;; QUIT
;; Takes no arguments
(define (quit? raw)
  ;; (-> string? boolean?)
  (member raw (list "q" "quit" "exit")))

;; --- command implementations

;; Search context `ctx` for a new context matching string `raw`.
;; Push `ctx` onto the stack `hist` on success.
;; If `ctx` is a zo struct, use `raw` to search for a field with `transition`.
;; If `ctx` is a list, parse `raw` for an integer index into the list.
(define (dive ctx hist raw)
  ;; (-> context? history? string? (values context? history?))
  (define arg (split-snd raw))
  (cond [(not arg)   (print-unknown raw)
                     (values ctx hist)]
        [(list? ctx) (dive-list ctx hist arg)]
        [(zo?   ctx) (dive-zo   ctx hist arg)]
        [else        (error (format "Invalid context '~a'" ctx))]))

;; Parse the string `arg` to an integer n.
;; If n is within the bounds of the list `ctx`,
;; push `ctx` onto the stack `hist` and return the n-th element of `ctx`.
;; Otherwise, return `ctx` and `hist` unchanged.
(define (dive-list ctx hist arg)
  ;; (-> (listof zo?) history? string? (values context? history?))
  (define index (string->number arg))
  (cond [(or (not index)
             (< index 0)
             (>= index (length ctx))) (print-unknown (format "dive ~a" arg))
                                      (values ctx hist)]
        [else (values (list-ref ctx index) (push hist ctx))]))

;; Use the string `field` to access a field in the zo struct `ctx`.
;; If the field exists and denotes another zo struct, return that
;; struct and push `ctx` on to the stack `hist`.
;; Otherwise, return `ctx` and `hist` unchanged.
(define (dive-zo ctx hist field)
  ;; (-> zo? history? string? (values context? history?))
  (let-values ([(ctx* success?) (transition ctx field)])
    (cond [success? (values ctx* (push hist ctx))]
          [else     (print-unknown (format "dive ~a" field))
                    (values ctx hist)])))

;; Search the fields of the struct `ctx` recursively for all zo structs
;; matching the string `raw`.
;; Returns a list of matching structs.
(define (find ctx raw)
  ;; (-> context? string? (listof zo?))
  (define arg (split-snd raw))
  (define results (if arg (zo-find ctx arg) '()))
  (printf "FIND returned ~a results\n" (length results))
  results)
               
;; --- history

;; Add the context `ctx` to the stack `hist`.
(define (push hist ctx)
  ;; (-> history? context? history?)
  (cons ctx hist))

;; Remove the top context from the stack `hist`.
;; Return the popped value and tail of `hist`.
;; Callers must avoid calling `pop` on empty stacks.
(define (pop hist)
  ;; (-> history? (values context? history?))
  (values (car hist) (cdr hist)))

;; --- print

;; Print a history object.
(define (print-history hist)
  ;; (-> history? void?)
  (printf "History is: ~a\n" hist))

;; Print a help message for the REPL.
(define (print-help)
  ;; (-> void?)
  (displayln (string-join (list "At your service. Available commands:"
                                "  back        Move up to the previous context"
                                "  dive ARG    Step into struct field ARG"
                                "  find ARG    Search the current subtree for structs with the name ARG"
                                "  help        Print this message"
                                "  info        Show information about current context"
                                "  quit        Exit the interpreter"
                                )
                          "\n")))

;; Print a context.
(define (print-context ctx)
  ;; (-> context? void?)
  (cond [(zo?   ctx) (displayln (zo->string ctx))]
        [(list? ctx) (printf "~a[~a]\n"
                             (for/list ([z ctx]) (zo->string z #:deep? #f))
                             (length ctx))]
        [else        (error (format "Unknown context '~a'"  ctx))]))

;; Print an error message (after receiving an undefined/invalid command).
(define (print-unknown raw)
  ;; (-> string? void?)
  (printf "'~a' not permitted.\n" raw))

;; Print a goodbye message (when the user exits the REPL).
(define (print-goodbye)
  ;; (-> void?)
  (printf "Ascending to second-level meditation. Goodbye.\n\n"))

;; Print a debugging message.
(define (print-debug str)
  ;; (-> string? void?)
  (printf "DEBUG: ~a\n" str))

;; Print a welcome message (when the user enters the REPL).
(define (print-welcome)
  ;; (-> void?)
  (printf "--- Welcome to the .zo shell, version ~a '~a' ---\n" VERSION VNAME))

;; Print the REPL prompt.
(define (print-prompt)
  ;; (-> void?)
  (printf "zo> "))

;; Print an informative message.
(define (print-info str)
  ;; (-> string? void?)
  (printf "INFO: ~a\n" str))

;; Print a warning.
(define (print-warn str)
  ;; (-> string? void?)
  (printf "WARN: ~a\n" str))

;; Print an error message.
(define (print-error str)
  ;; (-> string? void?)
  (printf "ERROR: ~a\n" str))

;; Print usage information.
(define (print-usage)
  (displayln "Usage: zo-shell FILE.zo"))

;; --- parsing
;; TODO replace these functions with a command line library

;; Split the string [raw] by whitespace and
;; return the second element of the split, if any.
;; Otherwise return [#f].
(define (split-snd raw)
  ;; (-> string? (or/c #f string?))
  (define splt (string-split raw))
  (cond [(empty? splt)             #f]
        [(empty? (cdr splt))       #f]
        [(empty? (cdr (cdr splt))) (car (cdr splt))]
        [else                      (print-warn (format "Ignoring extra arguments: '~a'" (cdr (cdr splt))))
                                   (car (cdr splt))]))

;; -----------------------------------------------------------------------------
;; --- testing

(module+ test
  (require rackunit
           compiler/zo-structs)

  ;; --- API
  ;; -- invalid args for init
  (check-equal? (init '())                 (void))
  (check-equal? (init '(two args))         (void))
  (check-equal? (init '(more than 2 args)) (void))
  
  ;; --- command predicates
  (check-pred back? "back")
  (check-pred back? "b")
  (check-pred back? "up")
  (check-pred back? "../")

  (check-false (back? "back ARG"))
  (check-false (back? "BACK"))
  (check-false (back? "help"))
  (check-false (back? ""))

  ;; -- DIVE command requires a single argument (doesn't fail for multiple arguments)
  (check-pred dive? "dive ARG")
  (check-pred dive? "d ARG")
  (check-pred dive? "next ARG")
  (check-pred dive? "step ARG1 ARG2 ARG3")

  (check-false (dive? "dive"))
  (check-false (dive? "d"))
  (check-false (dive? "quit"))
  (check-false (dive? "a mistake"))

  ;; -- FIND command takes one argument, just like DIVE
  (check-pred find? "find ARG")
  (check-pred find? "f ARG1 ARG2 ARG3")
  (check-pred find? "query ")
  (check-pred find? "search branch")
  (check-pred find? "look up")

  (check-false (find? "find"))
  (check-false (find? "back"))
  (check-false (find? "hello world"))

  (check-pred help? "help")
  (check-pred help? "h")
  (check-pred help? "--help")
  (check-pred help? "-help")

  (check-false (help? 'help))
  (check-false (help? "help me"))
  (check-false (help? "lost"))
  (check-false (help? "stuck, please help"))

  (check-pred info? "info")
  (check-pred info? "i")
  (check-pred info? "print")
  (check-pred info? "show")

  (check-false (info? "println"))
  (check-false (info? "help"))
  (check-false (info? "display"))
  (check-false (info? "write to out"))

  (check-pred quit? "q")
  (check-pred quit? "quit")
  (check-pred quit? "exit")

  (check-false (quit? "leave"))
  (check-false (quit? "(quit)"))
  (check-false (quit? "(exit)"))
  (check-false (quit? "get me out of here"))

  ;; --- command implementations

  ;; -- dive end-to-end
  ;; Invalid command
  (let ([ctx '()] [hist '()] [arg "dive "])
    (let-values ([(ctx* hist*) (dive ctx hist arg)])
      (begin (check-equal? ctx ctx*)
             (check-equal? hist hist*))))

  ;; List out-of-bounds
  (let ([ctx '((a) (b))] [hist '((c) (d))] [arg  "dive 2"])
    (let-values ([(ctx* hist*) (dive ctx hist arg)])
      (begin (check-equal? ctx ctx*)
             (check-equal? hist hist*))))

  ;; List, in-bounds
  (let ([ctx '((a) (b))] [hist '((c) (d))] [arg "dive 0"])
    (let-values ([(ctx* hist*) (dive ctx hist arg)])
      (begin (check-equal? ctx*  (car ctx))
             (check-equal? hist* (cons ctx hist) ))))

  ;; zo, valid field
  (let* ([z (wrap)]
         [ctx (wrapped #f (list z z z) 'tainted)]
         [hist '()]
         [arg "dive wraps"])
    (let-values ([(ctx* hist*) (dive ctx hist arg)])
      (begin (check-equal? ctx*  (list z z z))
             (check-equal? hist* (cons ctx hist) ))))

  ;; zo, invalid field
  (let ([ctx (wrapped #f '() 'tainted)] [hist '()] [arg "dive datum"])
    (let-values ([(ctx* hist*) (dive ctx hist arg)])
      (begin (check-equal? ctx*  ctx)
             (check-equal? hist* hist))))

  ;; -- dive list
  ;; Valid list access
  (let ([ctx '(a b c)] [hist '(d)] [arg "2"])
    (let-values ([(ctx* hist*) (dive-list ctx hist arg)])
      (begin (check-equal? ctx*  (caddr ctx))
             (check-equal? hist* (cons ctx hist)))))

  ;; Invalid, index is not an integer
  (let ([ctx '(a)] [hist '()] [arg "x"])
    (let-values ([(ctx* hist*) (dive-list ctx hist arg)])
      (begin (check-equal? ctx*  ctx)
             (check-equal? hist* hist))))

  ;; Invalid, index is not in bounds
  (let ([ctx '(a b c)] [hist '(d)] [arg "3"])
    (let-values ([(ctx* hist*) (dive-list ctx hist arg)])
      (begin (check-equal? ctx*  ctx)
             (check-equal? hist* hist))))

  ;; -- dive zo (I'm creating these zo structs arbitrarily, using the contracts in 'zo-lib/compiler/zo-structs.rkt')
  ;; Valid, field is a zo
  (let* ([z (localref #f 0 #f #f #f)]
         [ctx (branch #t z #t)]
         [hist '()]
         [arg "then"])
    (let-values ([(ctx* hist*) (dive-zo ctx hist arg)])
      (begin (check-equal? ctx*  z)
             (check-equal? hist* (cons ctx hist)))))

  ;; Valid, field is a list of zo
  (let* ([z (toplevel 999 1 #t #t)]
         [ctx (def-values (list z z 'arbitrary-symbol) #f)]
         [hist '(d)]
         [arg "ids"])
    (let-values ([(ctx* hist*) (dive-zo ctx hist arg)])
      (begin (check-equal? ctx*  (list z z))
             (check-equal? hist* (cons ctx hist)))))

  ;; Invalid, field is not a zo
  (let* ([z (localref #f 0 #f #f #f)]
         [ctx (branch #t z #t)]
         [hist '()]
         [arg "test"])
    (let-values ([(ctx* hist*) (dive-zo ctx hist arg)])
      (begin (check-equal? ctx*  ctx)
             (check-equal? hist* hist))))

  ;; Invalid, field is a list that does not contain any zo
  (let* ([z (toplevel 999 1 #t #t)]
         [ctx (def-values (list z z 'arbitrary-symbol) #f)]
         [hist '(d)]
         [arg "rhs"])
    (let-values ([(ctx* hist*) (dive-zo ctx hist arg)])
      (begin (check-equal? ctx*  ctx)
             (check-equal? hist* hist))))

  ;; Invalid, field does not exist
  (let* ([z (localref #f 0 #f #f #f)]
         [ctx (branch #t z #t)]
         [hist '()]
         [arg ""])
    (let-values ([(ctx* hist*) (dive-zo ctx hist arg)])
      (begin (check-equal? ctx*  ctx)
             (check-equal? hist* hist))))

  ;; -- find
  ;; Success, search 1 level down
  (let* ([z (wrap-mark 42)]
         [ctx (wrapped #f (list z z z) 'tainted)]
         [arg "find wrap-mark"]
         [res (find ctx arg)])
    (check-equal? res (list z z z)))
  
  ;; Failure, search 1 level down
  (let* ([z (wrap-mark 42)]
         [ctx (wrapped #f (list z z z) 'tainted)]
         [arg "find mummy"]
         [res (find ctx arg)])
    (check-equal? res '()))

  ;; Success, deeper search. Note that the top struct is not in the results
  (let* ([ctx  (branch #t #t (branch #t #t (branch #t #t (branch #t #t #t))))]
         [arg "find branch"]
         [res (find ctx arg)])
      (begin (check-equal? (length res) 3)
             (check-equal? (car res) (branch-else ctx))))

  ;; Success, deeper search.
  (let* ([z (beg0 '())]
         [ctx  (branch #t #t (branch #t #t (branch #t #t (branch #t z #t))))]
         [arg "find beg0"]
         [res (find ctx arg)])
      (begin (check-equal? (length res) 1)
             (check-equal? (car res) z)))

  ;; --- history manipulation
  (check-equal? (push '() 'x) '(x))
  (check-equal? (push '() '()) '(()))

  (let-values ([(hd tl) (pop '(a b c))])
    (begin (check-equal? hd 'a)
           (check-equal? tl '(b c))))
  (let-values ([(hd tl) (pop '(()))])
    (begin (check-equal? hd '())
           (check-equal? tl '())))

  ;; --- printing

  ;; --- parsing
  ;; Success, has exactly one whitespace
  (let* ([arg "hey jude"]
         [res "jude"])
    (check-equal? (split-snd arg) res))

  ;; Success, but prints warning about extra arguments
  (let* ([arg "hel lo world"]
         [res "lo"])
    (check-equal? (split-snd arg) res))

  ;; Failure, no whitespace
  (let* ([arg "yolo"]
         [res #f])
    (check-equal? (split-snd arg) res))

  ;; Failure, no characters
  (let* ([arg ""]
         [res #f])
    (check-equal? (split-snd arg) res))

) ;; --- end testing
