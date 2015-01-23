#lang racket/base

;; Command-line UI for exploring decompiled bytecode.
;; (Use `raco make` to generate bytecode)

(provide
 ;; Start a REPL using command-line arguments
 init)

(require (only-in compiler/zo-parse zo? zo-parse)
         (only-in racket/string string-split string-join)
         (only-in "zo-find.rkt" zo-find result result? result-zo result-path)
         (only-in "zo-string.rkt" zo->string)
         (only-in "zo-transition.rkt" transition)
         racket/match)

;; --- constants & contracts

(define DEBUG   #f) ;when set, print extra debugging information
(define VERSION 0.6) ;for aesthetic purposes
(define VNAME   "outlands") ;also aesthetic
;; (define nat? natural-number/c)
;; (define context? (or/c zo? (listof zo?) (listof result?)))
;; (define history? (listof context?))

;; -----------------------------------------------------------------------------

;; --- API functions

;; Entry point to the REPL, expects command-line arguments passed as a list.
;; In the future, there may be more entry points.
(define (init args)
  ;; (-> (listof string?) void?)
  (match args
    [(list fname) (init-from-filename fname)]
    [_            (print-usage)]))

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
  (repl ctx '() '()))

;; The REPL loop. Process a command using context `ctx` and history `hist`.
(define (repl ctx hist pre-hist)
  ;; (-> context? history? void?)
  (when DEBUG (print-history hist))
  (print-prompt)
  (define raw (read-line))
  ;; 2015-01-12: Command parsing is currently very simple.
  (match raw
    [(? alias?) (print-alias)
                (repl ctx hist pre-hist)]
    [(? back?)  (call-with-values (lambda () (back raw ctx hist pre-hist)) repl)]
    [(? dive?)  (let-values ([(ctx* hist*) (dive ctx hist raw)])
                  (repl ctx* hist* pre-hist))]
    [(? find?)  (call-with-values (lambda () (find raw ctx hist pre-hist)) repl)]
    [(? help?)  (print-help)
                (repl ctx hist pre-hist)]
    [(? info?)  (print-context ctx)
                (repl ctx hist pre-hist)]
    [(? jump?)  (call-with-values (lambda () (jump raw ctx hist pre-hist)) repl)]
    [(? save?)  (call-with-values (lambda () (save raw ctx hist pre-hist)) repl)]
    [(? quit?)  (print-goodbye)]
    [_          (print-unknown raw)
                (repl ctx hist pre-hist)]))

;; --- command predicates

;; ALIAS
;; No arguments
(define alst-cmds (list "alst" "a" "alias" "aliases"))
(define (alias? raw)
  (member raw alst-cmds))

;; BACK
;; Takes no arguments
(define back-cmds (list "back" "b" "up" "u" ".." "../" "cd .." "cd ../"))
(define (back? raw)
  ;; (-> string? boolean?)
  (member raw back-cmds))

;; DIVE ARG
;; Takes one argument, a string denoting the field to enter.
(define dive-cmds (list "dive" "d" "cd" "next" "step"))
(define (dive? raw)
  ;; (-> string? boolean?)
  (define hd (if (memq #\space (string->list raw))
                 (car (string-split raw))
                 ""))
  (member hd dive-cmds))

;; FIND ARG
;; Takes one argument, a string denoting the struct name to search for.
(define find-cmds (list "find" "f" "query" "search" "look"))
(define (find? raw)
  ;; (-> string? boolean?)
  (define hd (if (memq #\space (string->list raw))
                 (car (string-split raw))
                 ""))
  (member hd find-cmds))

;; HELP
;; Takes no arguments
(define help-cmds (list "help" "h" "-h" "--h" "-help" "--help"))
(define (help? raw)
  ;; (-> string? boolean?)
  (member raw help-cmds))

;; INFO
;; Takes no arguments
(define info-cmds (list "info" "i" "ls" "print" "p" "show"))
(define (info? raw)
  ;; (-> string? boolean?)
  (member raw info-cmds))

;; JUMP
;; No args
(define jump-cmds (list "jump" "j" "warp" "top"))
(define (jump? raw)
  (member raw jump-cmds))

;; SAVE
;; No args
(define save-cmds (list "save" "mark"))
(define (save? raw)
  ;; (-> string boolean?)
  (member raw save-cmds))

;; QUIT
;; Takes no arguments
(define quit-cmds (list "quit" "q" "exit"))
(define (quit? raw)
  ;; (-> string? boolean?)
  (member raw quit-cmds))

;; --- command implementations

;; Step back to a previous context, if any, and reduce the history.
;; Try popping from `hist`, fall back to list-of-histories `pre-hist`.
(define (back raw ctx hist pre-hist)
  (match (list hist pre-hist)
    [(list '() '()) (print-unknown raw)
                    (values ctx hist pre-hist)]
    [(list '() _)   (let-values ([(hist* pre-hist*) (pop pre-hist)])
                      (displayln "BACK removing most recent 'save' mark. Be sure to save if you want to continue exploring search result.")
                      (back raw ctx hist* pre-hist*))]
    [_              (let-values ([(ctx* hist*) (pop hist)])
                      (values ctx* hist* pre-hist))]))

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
  ;; (-> (listof (or/c zo? result?)) history? string? (values context? history?))
  (define index (string->number arg))
  (cond [(or (not index)
             (< index 0)
             (>= index (length ctx))) (print-unknown (format "dive ~a" arg))
                                      (values ctx hist)]
        [else (define res (list-ref ctx index))
              ;; If list elements are search results,
              ;; current `hist` can be safely ignored.
              (cond [(result? res) (values (result-zo res) (result-path res))]
                    [else          (values res             (push hist ctx))])]))

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

;; Parse argument, then search for & save results.
(define (find raw ctx hist pre-hist)
  (define arg (split-snd raw))
  (cond [(and arg (zo? ctx)) (define results (zo-find ctx arg))
                             (printf "FIND returned ~a results\n" (length results))
                             (match results
                               ['() (values ctx hist pre-hist)]
                               ;; Success! Show the results and save them, to allow jumps
                               [_   (print-context results)
                                    (printf "FIND automatically saving context\n")
                                    (save "" results (push hist ctx) pre-hist)])]
        [else      (print-unknown raw)
                   (values ctx hist pre-hist)]))


;; Jump back to a previously-saved location, if any.
(define (jump raw ctx hist pre-hist)
  (match pre-hist
    ['() (print-unknown raw)
         (values ctx hist pre-hist)]
    [_   (let-values ([(hist* pre-hist*) (pop pre-hist)])
           (back raw ctx hist* pre-hist*))]))

;; Save the current context and history to the pre-history
;; For now, erases current history. 
(define (save raw ctx hist pre-hist)
  (values ctx '() (push pre-hist (push hist ctx))))

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

(define (print-alias)
  ;; (-> void?)
  (displayln (string-join (list "At your service. Command aliases:"
                                (format "  alst        ~a" (string-join alst-cmds))
                                (format "  back        ~a" (string-join back-cmds))
                                (format "  dive        ~a" (string-join dive-cmds))
                                (format "  find        ~a" (string-join find-cmds))
                                (format "  help        ~a" (string-join help-cmds))
                                (format "  info        ~a" (string-join info-cmds))
                                (format "  jump        ~a" (string-join jump-cmds))
                                (format "  save        ~a" (string-join save-cmds))
                                (format "  quit        ~a" (string-join quit-cmds)))
                          "\n")))

;; Print a history object.
(define (print-history hist)
  ;; (-> history? void?)
  (printf "History is: ~a\n" hist))

;; Print a help message for the REPL.
(define (print-help)
  ;; (-> void?)
  (displayln (string-join (list "At your service. Available commands:"
                                "  alst        Print command aliases"
                                "  back        Move up to the previous context"
                                "  dive ARG    Step into struct field ARG"
                                "  find ARG    Search the current subtree for structs with the name ARG"
                                "  help        Print this message"
                                "  info        Show information about current context"
                                "  jump        Revert to last saved position"
                                "  save        Save the current context as jump target"
                                "  quit        Exit the interpreter"
                                )
                          "\n")))

;; Print a context.
(define (print-context ctx)
  ;; (-> context? void?)
  (cond [(zo?   ctx) (displayln (zo->string ctx))]
        [(list? ctx) (printf "~a[~a]\n"
                             (for/list ([x ctx])
                               ;; If we have a list of search results, extract & print the zo
                               (let ([z (if (result? x) (result-zo x) x)])
                                 (zo->string z #:deep? #f)))
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
  (display
   (format "\033[1;34m--- Welcome to the .zo shell, version ~a '~a' ---\033[0;0m\n" VERSION VNAME)))

;; Print the REPL prompt.
(define (print-prompt)
  ;; (-> void?)
  (display "\033[1;32mzo> \033[0;0m"))

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
  (match splt
    [(list _ x)       x]
    [(list _ x ys ...) (print-warn (format "Ignoring extra arguments: '~a'" ys))
                       x]
    [_ #f]))

;; -----------------------------------------------------------------------------
;; --- testing

(module+ test
  (require rackunit
           compiler/zo-structs)

  ;; Hijack the print statements
  (define-values (in out) (make-pipe))
  (current-output-port out)

  ;; --- API
  ;; -- invalid args for init. read-line makes sure some message was printed.
  (check-equal? (init '())                 (void))
  (check-pred read-line in)
  (check-equal? (init '(two args))         (void))
  (check-pred read-line in)
  (check-equal? (init '(more than 2 args)) (void))
  (check-pred read-line in)

  ;; --- command predicates
  (check-pred alias? "alst")
  (check-pred alias? "a")
  (check-pred alias? "alias")
  (check-pred alias? "aliases")

  (check-false (alias? "alias ARG"))
  (check-false (alias? "ALIAS"))
  (check-false (alias? "help"))
  (check-false (alias? ""))

  (check-pred back? "back")
  (check-pred back? "b")
  (check-pred back? "up")
  (check-pred back? "../")
  (check-pred back? "cd ../")

  (check-false (back? "back ARG"))
  (check-false (back? "BACK"))
  (check-false (back? "help"))
  (check-false (back? ""))

  ;; -- DIVE command requires a single argument (doesn't fail for multiple arguments)
  (check-pred dive? "dive ARG")
  (check-pred dive? "d ARG")
  (check-pred dive? "cd ARG")
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

  (check-pred jump? "jump")
  (check-pred jump? "j")
  (check-pred jump? "warp")
  (check-pred jump? "top")

  (check-false (jump? "jump a"))
  (check-false (jump? "w"))

  (check-pred save? "save")
  (check-pred save? "mark")

  (check-false (save? "lasd"))
  (check-false (save? "step"))
  (check-false (save? ""))
  (check-false (save? "save z"))

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
  (check-pred read-line in)

  ;; List out-of-bounds
  (let ([ctx '((a) (b))] [hist '((c) (d))] [arg  "dive 2"])
    (let-values ([(ctx* hist*) (dive ctx hist arg)])
      (begin (check-equal? ctx ctx*)
             (check-equal? hist hist*))))
  (check-pred read-line in)

  ;; List, in-bounds
  (let ([ctx '((a) (b))] [hist '((c) (d))] [arg "dive 0"])
    (let-values ([(ctx* hist*) (dive ctx hist arg)])
      (begin (check-equal? ctx*  (car ctx))
             (check-equal? hist* (cons ctx hist) ))))

  ;; List, search results. Ignores current history
  (let ([ctx  (list (result (zo) '()))]
        [hist '((c) (d))]
        [arg "dive 0"])
    (let-values ([(ctx* hist*) (dive ctx hist arg)])
      (begin (check-equal? ctx*  (result-zo (car ctx)))
             (check-equal? hist* '()))))

  ;; List, search results. Ignores current history, overwrites with search result history
  (let ([ctx  (list (result (zo) '(a a a)))]
        [hist '((c) (d))]
        [arg "dive 0"])
    (let-values ([(ctx* hist*) (dive ctx hist arg)])
      (begin (check-equal? ctx*  (result-zo (car ctx)))
             (check-equal? hist* (result-path (car ctx))))))

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
  (check-pred read-line in)

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
  (check-pred read-line in)

  ;; Invalid, index is not in bounds
  (let ([ctx '(a b c)] [hist '(d)] [arg "3"])
    (let-values ([(ctx* hist*) (dive-list ctx hist arg)])
      (begin (check-equal? ctx*  ctx)
             (check-equal? hist* hist))))
  (check-pred read-line in)

  ;; Search results, hist overwritten
  (let ([ctx (list (result (zo) '(a)) 
                   (result (expr) '(b)) 
                   (result (wrap) '(c))
                   (result (form) '(d)))]
        [hist '(e)]
        [arg "3"])
    (let-values ([(ctx* hist*) (dive-list ctx hist arg)])
      (begin (check-equal? ctx*  (result-zo (cadddr ctx)))
             (check-equal? hist* (result-path (cadddr ctx))))))

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
  (check-pred read-line in)

  ;; Invalid, field is a list that does not contain any zo
  (let* ([z (toplevel 999 1 #t #t)]
         [ctx (def-values (list z z 'arbitrary-symbol) #f)]
         [hist '(d)]
         [arg "rhs"])
    (let-values ([(ctx* hist*) (dive-zo ctx hist arg)])
      (begin (check-equal? ctx*  ctx)
             (check-equal? hist* hist))))
  (check-pred read-line in)

  ;; Invalid, field does not exist
  (let* ([z (localref #f 0 #f #f #f)]
         [ctx (branch #t z #t)]
         [hist '()]
         [arg ""])
    (let-values ([(ctx* hist*) (dive-zo ctx hist arg)])
      (begin (check-equal? ctx*  ctx)
             (check-equal? hist* hist))))
  (check-pred read-line in)

  ;; -- find
  ;; Success, search 1 level down
  (let* ([z (wrap-mark 42)]
         [ctx (wrapped #f (list z z z) 'tainted)]
         [raw "find wrap-mark"]
         [hist '(A)]
         [pre-hist '(a b)])
    (let-values ([(ctx* hist* pre-hist*) (find raw ctx hist pre-hist)])
      (begin (check-equal? (result-zo (car ctx*)) z)
             (check-equal? (result-path (car ctx*)) '())
             (check-equal? hist* '())
             (check-equal? pre-hist* (cons (cons ctx* (cons ctx hist)) pre-hist)))))
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)

  ;; Failure, search 1 level down
  (let* ([z (wrap-mark 42)]
         [ctx (wrapped #f (list z z z) 'tainted)]
         [raw "find mummy"]
         [hist '(A)]
         [pre-hist '(a b)])
    (let-values ([(ctx* hist* pre-hist*) (find raw ctx hist pre-hist)])
      (begin (check-equal? ctx* ctx)
             (check-equal? hist* hist)
             (check-equal? pre-hist* pre-hist))))
  (check-pred read-line in)

  ;; Success, deeper search. Note that the top struct is not in the results
  (let* ([ctx  (branch #t #t (branch #t #t (branch #t #t (branch #t #t #t))))]
         [raw "find branch"]
         [hist '(asa)]
         [pre-hist '(b c s)])
    (let-values ([(ctx* hist* pre-hist*) (find raw ctx hist pre-hist)])
      (begin (check-equal? (length ctx*) 3)
             (check-equal? (result-zo (car ctx*)) (branch-else ctx))
             (check-equal? (result-path (cadr ctx*)) (list (branch-else ctx)))
             (check-equal? hist* '())
             (check-equal? pre-hist* (cons (cons ctx* (cons ctx hist)) pre-hist)))))
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)

  ;; Success, deeper search.
  (let* ([z (beg0 '())]
         [ctx  (branch #t #t (branch #t #t (branch #t #t (branch #t z #t))))]
         [raw "find beg0"]
         [hist '(asa)]
         [pre-hist '(b c s)])
    (let-values ([(ctx* hist* pre-hist*) (find raw ctx hist pre-hist)])
      (begin (check-equal? (length ctx*) 1)
             (check-equal? (result-zo (car ctx*)) (branch-then (branch-else (branch-else (branch-else ctx)))))
             (check-equal? (result-path (car ctx*)) (list (branch-else (branch-else (branch-else ctx)))
                                                          (branch-else (branch-else ctx))
                                                          (branch-else ctx)))
             (check-equal? hist* '())
             (check-equal? pre-hist* (cons (cons ctx* (cons ctx hist)) pre-hist)))))
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)

  ;; -- back
  ;; - Failure, cannot go back
  (let* ([ctx      'a]
         [hist     '()]
         [pre-hist '()])
    (let-values ([(ctx* hist* pre-hist*) (back 'foo ctx hist pre-hist)])
      (begin (check-equal? ctx* ctx)
             (check-equal? hist* hist)
             (check-equal? pre-hist* pre-hist))))
  (check-pred read-line in)

  ;; - Success, use hist to go back
  (let* ([ctx      'a]
         [hist     '(b)]
         [pre-hist '()])
    (let-values ([(ctx* hist* pre-hist*) (back 'foo ctx hist pre-hist)])
      (begin (check-equal? ctx* 'b)
             (check-equal? hist* '())
             (check-equal? pre-hist* pre-hist))))

  ;; - Success, use hist to go back (Do not change pre-hist)
  (let* ([ctx      'a]
         [hist     '(b c)]
         [pre-hist '(x y)])
    (let-values ([(ctx* hist* pre-hist*) (back 'foo ctx hist pre-hist)])
      (begin (check-equal? ctx* 'b)
             (check-equal? hist* (cdr hist))
             (check-equal? pre-hist* pre-hist))))

  ;; - Success, use pre-hist to go back
  (let* ([ctx      'z]
         [hist     '()]
         [pre-hist '((a b c) (d e f))])
    (let-values ([(ctx* hist* pre-hist*) (back 'foo ctx hist pre-hist)])
      (begin (check-equal? ctx* 'a)
             (check-equal? hist* (cdar pre-hist))
             (check-equal? pre-hist* (cdr pre-hist)))))
  (check-pred read-line in)
  
  ;; -- jump
  ;; - Fail, no pre-hist
  (let* ([ctx      'a]
         [hist     '(b c d)]
         [pre-hist '()])
    (let-values ([(ctx* hist* pre-hist*) (jump 'raw ctx hist pre-hist)])
      (begin (check-equal? ctx* ctx)
             (check-equal? hist* hist)
             (check-equal? pre-hist* pre-hist))))
  (check-pred read-line in)

  ;; - Success! Has pre-hist
  (let* ([ctx      'z]
         [hist     '()]
         [pre-hist '((a b c) (d e f))])
    (let-values ([(ctx* hist* pre-hist*) (jump 'raw ctx hist pre-hist)])
      (begin (check-equal? ctx* 'a)
             (check-equal? hist* (cdar pre-hist))
             (check-equal? pre-hist* (cdr pre-hist)))))

  ;; - Success, clobber old hist
  (let* ([ctx      'z]
         [hist     '(l o l)]
         [pre-hist '((a b c) (d e f))])
    (let-values ([(ctx* hist* pre-hist*) (jump 'raw ctx hist pre-hist)])
      (begin (check-equal? ctx* 'a)
             (check-equal? hist* (cdar pre-hist))
             (check-equal? pre-hist* (cdr pre-hist)))))

  ;; -- save
  ;; - Always succeeds, just move hist to the pre-hist
  (let* ([ctx      'z]
         [hist     '(l o l)]
         [pre-hist '((a b c) (d e f))])
    (let-values ([(ctx* hist* pre-hist*) (save 'raw ctx hist pre-hist)])
      (begin (check-equal? ctx* 'z)
             (check-equal? hist* '())
             (check-equal? pre-hist* (cons (cons ctx hist) pre-hist)))))

  (let* ([ctx      'z]
         [hist     '()]
         [pre-hist '()])
    (let-values ([(ctx* hist* pre-hist*) (save 'raw ctx hist pre-hist)])
      (begin (check-equal? ctx* 'z)
             (check-equal? hist* hist)
             (check-equal? pre-hist* (cons (list ctx) pre-hist)))))

  (let* ([ctx      'z]
         [hist     '()]
         [pre-hist '(yolo)])
    (let-values ([(ctx* hist* pre-hist*) (save 'raw ctx hist pre-hist)])
      (begin (check-equal? ctx* 'z)
             (check-equal? hist* hist)
             (check-equal? pre-hist* '((z) yolo)))))

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
  (print-alias)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  
  
  (print-history '())
  (check-pred read-line in)

  (print-help)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)

  (print-context '())
  (check-pred read-line in)

  (print-context (beg0 '()))
  (check-pred read-line in)
  (check-pred read-line in)

  (print-context (list (result (beg0 '()) '())))
  (check-pred read-line in)

  (print-unknown "")
  (check-pred read-line in)

  (print-goodbye)
  (check-pred read-line in)
  (check-pred read-line in)

  (print-debug "")
  (check-pred read-line in)

  (print-welcome)
  (check-pred read-line in)

  (print-prompt) (displayln "")
  (check-pred read-line in)

  (print-info "")
  (check-pred read-line in)

  (print-warn "")
  (check-pred read-line in)

  (print-error "")
  (check-pred read-line in)

  (print-usage)
  (check-pred read-line in)
  
  ;; --- parsing
  ;; Success, has exactly one whitespace
  (let* ([arg "hey jude"]
         [res "jude"])
    (check-equal? (split-snd arg) res))

  ;; Success, but prints warning about extra arguments
  (let* ([arg "hel lo world"]
         [res "lo"])
    (check-equal? (split-snd arg) res))
  (check-pred read-line in)

  ;; Failure, no whitespace
  (let* ([arg "yolo"]
         [res #f])
    (check-equal? (split-snd arg) res))

  ;; Failure, no characters
  (let* ([arg ""]
         [res #f])
    (check-equal? (split-snd arg) res))

) ;; --- end testing
