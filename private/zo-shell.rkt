#lang racket/base

;; Command-line UI for exploring decompiled bytecode.
;; (Use `raco make` to generate bytecode)

(provide
 init-from-filename
 ;; (-> String Void)
 ;; Start a repl using the zo file `filename`

 find-all
 ;; (->* [String (Listof String)] [#:limit (U Natural #f)] Void)
 ;; (find-all zo arg* #:lim n)
 ;; Searches the bytecode file `zo` for all zo-structs named by the list `arg*`
 ;;  and prints the number of matches.
 ;; If `#:limit` is not false, recursive searches are terminated at depth `n`.
 ;;
 ;; For example, if `arg*` is '("branch" "lam"), then the result will be the number
 ;; of zo structs in the decompiled output that match the `branch?` or `lam?` predicates.

 print-usage
 ;; (-> Void)
 ;; Display terms-of-use
)

(require (only-in compiler/zo-parse zo? zo-parse)
         (only-in racket/string string-split string-join string-trim)
         (only-in "zo-find.rkt" zo-find result result? result-zo result-path)
         (only-in "zo-string.rkt" zo->string zo->spec)
         (only-in "zo-transition.rkt" zo-transition)
         racket/match
)

;; -----------------------------------------------------------------------------

;; --- constants & contracts

;; when set, print extra debugging information
(define DEBUG   #f)
;; For aesthetic purposes
(define VERSION 1.0)
(define VNAME   "vortex")
;; (define nat? natural-number/c)
;; (define context? (or/c zo? (listof zo?) (listof result?)))
;; (define history? (listof context?))

;; --- API functions

;; Entry point to the REPL, expects command-line arguments passed as a list.
;; In the future, there may be more entry points.
(define (init args)
  ;; (-> (vectorof string?) void?)
  (match args
    ['#()
     (print-usage)]
    ;; Catch --help flag, and any others
    [(? has-any-flags?) (print-usage)]
    [(vector fname)
     (init-from-filename fname)]
    [(vector fname args ...)
     (find-all fname args)]))

;; --- Commands (could go in their own file)

(struct command (name       ;; String
                 num-args   ;; Natural
                 aliases    ;; Listof String
                 help-msg)) ;; String
(define ALST (command "alst"
                      0
                      (list "a" "alias" "aliases")
                      "Print command aliases"))
(define BACK (command "back"
                      0
                      (list "b" "up" "u" ".." "../" "cd .." "cd ../")
                      "Move up to the previous context"))
(define DIVE (command "dive"
                      1
                      (list "d" "cd" "next" "step")
                      "Step into struct field ARG"))
(define FIND (command "find"
                      1
                      (list "f" "query" "search" "look")
                      "Search the current subtree for structs with the name ARG"))
(define HELP (command "help"
                      0
                      (list "h" "-h" "--h" "-help" "--help")
                      "Print this message"))
(define INFO (command "info"
                      0
                      (list "i" "ls" "print" "p" "show")
                      "Show information about current context"))
(define JUMP (command "jump"
                      0
                      (list "j" "warp" "top")
                      "Revert to last saved position"))
(define SAVE (command "save"
                      0
                      (list "mark")
                      "Save the current context as jump target"))
(define QUIT (command "quit"
                      0
                      (list "q" "exit" "leave" "bye")
                      "Exit the interpreter"))
(define COMMANDS
  (list ALST BACK DIVE FIND HELP INFO JUMP SAVE QUIT))

(define ((cmd? c) str)
  ;; (-> command? (-> string? boolean?))
  (define splt (string-split str))
  (or
   ;; Special cases
   (and (string=? "back" (command-name c))
        (member str (list "cd .." "cd ../")))
   ;; Everything else
   (and
    ;; Has the right number of arguments
    (= (sub1 (length splt))
       (command-num-args c))
    ;; First word matches command name (or an alias)
    (or (string=? (car splt) (command-name c))
        (member   (car splt) (command-aliases c))))))

;; --- REPL

;; Start REPL from a filename
(define (init-from-filename name)
  ;; (-> string? void?)
  (print-info (format "Loading bytecode file '~a'..." name))
  (call-with-input-file name
    (lambda (port)
      (print-info "Parsing bytecode...")
      (define ctx  (zo-parse port))
      (print-info "Parsing complete!")
      (print-welcome)
      ((repl ctx '() '()) '()))))

;; Check if second arg is a prefix of the first
(define (starts-with? str prefix)
  ;; (-> string? string? boolean?)
  (and (<= (string-length prefix)
           (string-length str))
       (for/and ([c1 (in-string str)]
                 [c2 (in-string prefix)])
         (char=? c1 c2))))

;; Split a path like "cd ../BLAH/.." into a list of commands "cd ..; cd BLAH; cd .."
(define (split-cd cmd*)
  ;; (-> (listof string?) (listof string?))
  (match cmd*
    ['() '()]
    [(cons cd-cmd rest)
     #:when (starts-with? cd-cmd "cd ")
     ;; Split "cd " commands by "/"
     (append
      (map (lambda (x) (string-append "cd " x)) (string-split (substring cd-cmd 3) "/"))
      (split-cd rest))]
    [(cons cmd rest)
     ;; Leave other commands alone
     (cons cmd (split-cd rest))]))

;; The REPL loop. Process a command using context `ctx` and history `hist`.
(define ((repl ctx hist pre-hist) cmd*)
  ;; (-> context? history? void?)
  (when DEBUG (print-history hist))
  (match cmd*
    ['()
     (print-prompt ctx)
     (match (read-line)
       [(? eof-object? _)
        (error 'zo-shell:repl "EOF: you have penetrated me")]
       [str
        ((repl ctx hist pre-hist) (split-cd (map string-trim (string-split str ";"))))])]
    [(cons (? (cmd? ALST) raw) cmd*)
     (print-alias) ((repl ctx hist pre-hist) cmd*)]
    [(cons (? (cmd? BACK) raw) cmd*)
     ((call-with-values (lambda () (back raw ctx hist pre-hist)) repl) cmd*)]
    [(cons (? (cmd? DIVE) raw) cmd*)
     ((call-with-values (lambda () (dive raw ctx hist pre-hist)) repl) cmd*)]
    [(cons (? (cmd? FIND) raw) cmd*)
     ((call-with-values (lambda () (find raw ctx hist pre-hist)) repl) cmd*)]
    [(cons (? (cmd? HELP) raw) cmd*)
     (begin (print-help) ((repl ctx hist pre-hist) cmd*))]
    [(cons (? (cmd? INFO) raw) cmd*)
     (begin (print-context ctx) ((repl ctx hist pre-hist) cmd*))]
    [(cons (? (cmd? JUMP) raw) cmd*)
     ((call-with-values (lambda () (jump raw ctx hist pre-hist)) repl) cmd*)]
    [(cons (? (cmd? SAVE) raw) cmd*)
     ((call-with-values (lambda () (save raw ctx hist pre-hist)) repl) cmd*)]
    [(cons (? (cmd? QUIT) raw) cmd*)
     (print-goodbye)]
    [(cons raw cmd*)
     (begin (print-unknown raw) ((repl ctx hist pre-hist) cmd*))]))

;; --- command implementations

;; 2015-01-23: Warn about possible-unexpected behavior
(define BACK-WARNING
  (string-append
   "BACK removing most recent 'save' mark. "
   "Be sure to save if you want to continue exploring search result."))

;; Step back to a previous context, if any, and reduce the history.
;; Try popping from `hist`, fall back to list-of-histories `pre-hist`.
(define (back raw ctx hist pre-hist)
  (match (list hist pre-hist)
    [(list '() '())
     ;; Nothing to pop from
     (print-unknown raw)
     (values ctx hist pre-hist)]
    [(list '() _)
     ;; Pop from pre-history
     (displayln BACK-WARNING)
     (define-values (hist* pre-hist*) (pop pre-hist))
     (back raw ctx hist* pre-hist*)]
    [_
     (define-values (ctx* hist*) (pop hist))
     (values ctx* hist* pre-hist)]))

;; Search context `ctx` for a new context matching string `raw`.
;; Push `ctx` onto the stack `hist` on success.
(define (dive raw ctx hist pre-hist)
  ;; (-> string? context? history? (listof history?) (values context? history? (listof history?)))
  (define arg (split-snd raw))
  (define-values (ctx* hist*)
    (cond
     [(not arg)
      ;; Failed to parse argument,
      (print-unknown raw)
      (values ctx hist)]
     [(list? ctx)
      ;; Context is a list, try accessing by index
      (dive-list ctx hist arg)]
     [(zo?   ctx)
      ;; Context is a zo, try looking up field
      (dive-zo   ctx hist arg)]
     [else
      ;; Should never happen! REPL controls the context.
      (error 'zo-shell:dive (format "Invalid context '~a'" ctx))]))
  ;; Return pre-hist unchanged
  (values ctx* hist* pre-hist))

;; Parse the string `arg` to an integer n.
;; If n is within the bounds of the list `ctx`,
;; push `ctx` onto the stack `hist` and return the n-th element of `ctx`.
;; Otherwise, return `ctx` and `hist` unchanged.
(define (dive-list ctx hist arg)
  ;; (-> (listof (or/c zo? result?)) history? string? (values context? history?))
  (define index (string->number arg))
  (cond [(or (not index) (< index 0) (>= index (length ctx)))
         ;; Index out of bounds, or not a number. Cannot dive.
         (print-unknown (format "dive ~a" arg))
         (values ctx hist)]
        [else
         ;; Select from list,
         (define res (list-ref ctx index))
         ;; If list elements are search results, current `hist` can be safely ignored.
         (if (result? res)
             (values (result-zo res) (result-path res))
             (values res             (push hist ctx)))]))

;; Use the string `field` to access a field in the zo struct `ctx`.
;; If the field exists and denotes another zo struct, return that
;; struct and push `ctx` on to the stack `hist`.
;; Otherwise, return `ctx` and `hist` unchanged.
(define (dive-zo ctx hist field)
  ;; (-> zo? history? string? (values context? history?))
  (define-values (ctx* success?) (zo-transition ctx field))
  (cond
   [success?
    (values ctx* (push hist ctx))]
   [else
    (print-unknown (format "dive ~a" field))
    (values ctx hist)]))

;; Parse argument, then search for & save results.
(define (find raw ctx hist pre-hist)
  (define arg (split-snd raw))
  (cond [(and arg (zo? ctx))
         (define results (zo-find ctx arg))
         (printf "FIND returned ~a results\n" (length results))
         (match results
           ['()
            ;; No results, don't make a save mark
            (values ctx hist pre-hist)]
           [_
            ;; Success! Show the results and save them, to allow jumps
            (printf "FIND automatically saving context\n")
            (print-context results)
            (save "" results (push hist ctx) pre-hist)])]
        [else
         (print-unknown raw)
         (values ctx hist pre-hist)]))


;; Jump back to a previously-saved location, if any.
(define (jump raw ctx hist pre-hist)
  (match pre-hist
    ['()
     ;; Nothing to jump to
     (print-unknown raw)
     (values ctx hist pre-hist)]
    [_
     (define-values (hist* pre-hist*) (pop pre-hist))
     (back raw ctx hist* pre-hist*)]))

;; Save the current context and history to the pre-history
;; For now, erases current history.
(define (save raw ctx hist pre-hist)
  (values ctx '() (push pre-hist (push hist ctx))))

;; --- history manipulation

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
  (displayln "At your service. Command aliases:")
  (displayln
   (string-join
    (for/list ([cmd COMMANDS])
      (format "  ~a        ~a"
              (command-name cmd)
              (string-join (command-aliases cmd))))
   "\n")))

;; Print a history object.
(define (print-history hist)
  ;; (-> history? void?)
  (printf "History is: ~a\n" hist))

;; Print a help message for the REPL.
(define (print-help)
  ;; (-> void?)
  (displayln "At your service. Available commands:")
  (displayln
   (string-join
    (for/list ([cmd COMMANDS])
      (format "  ~a~a    ~a"
              (command-name cmd)
              (if (= 1 (command-num-args cmd)) " ARG" "    ") ;; hack
              (command-help-msg cmd)))
    "\n")))

;; Print a context.
(define (print-context ctx)
  ;; (-> context? void?)
  (match ctx
    [(? zo?)
     (displayln (zo->string ctx))]
    ['()
     (displayln "'()")]
    [(cons x _)
     (define z (if (result? x) (result-zo x) x))
     (printf "~a[~a]\n"
             (zo->string z #:deep? #f)
             (length ctx))]
    [_
     (error 'zo-shell:info (format "Unknown context '~a'"  ctx))]))

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
(define (print-prompt ctx)
  ;; (-> void?)
  (define tag (cond [(list? ctx) (format "[~a]" (length ctx))]
                    [(zo? ctx)   (format "(~a)" (car (zo->spec ctx)))]
                    [else ""]))
  (display (string-append tag " \033[1;32mzo> \033[0;0m")))

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
(define USAGE
  "Usage: zo-shell <OPTIONS> FILE.zo")
(define (print-usage)
  (displayln USAGE))

;; --- misc

(define (find-all name args #:limit [lim #f])
  ;; (-> string? (listof string?) void)
  (print-info (format "Loading bytecode file '~a'..." name))
  (call-with-input-file name
    (lambda (port)
      (print-info "Parsing bytecode...")
      (define ctx (zo-parse port))
      (print-info "Parsing complete! Searching...")
      (for ([arg (in-list args)])
        (printf "FIND '~a' : " arg)
        (printf "~a results\n" (length (zo-find ctx arg #:limit lim))))
      (displayln "All done!"))))

;; Split the string `raw` by whitespace and
;; return the second element of the split, if any.
;; Otherwise return `#f`.
(define (split-snd raw)
  ;; (-> string? (or/c #f string?))
  (define splt (string-split raw))
  (match splt
    [(list _ x)       x]
    [(list _ x ys ...) (print-warn (format "Ignoring extra arguments: '~a'" ys))
                       x]
    [_ #f]))

;; True if the vector contains any command-line flags.
;; All flags begin with a hyphen, -
(define (has-any-flags? v)
  ;; (-> (vectorof string) boolean?)
  (for/or ([str (in-vector v)])
    (and (< 0 (string-length str))
         (eq? #\- (string-ref str 0)))))

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
  ;; -- TODO more init tests
  (check-equal? (init '#())                 (void))
  (check-pred read-line in)

  ;; --- command predicates
  (check-pred (cmd? ALST) "alst")
  (check-pred (cmd? ALST) "a")
  (check-pred (cmd? ALST) "alias")
  (check-pred (cmd? ALST) "aliases")

  (check-false ((cmd? ALST) "alias ARG"))
  (check-false ((cmd? ALST) "ALIAS"))
  (check-false ((cmd? ALST) "help"))
  (check-false ((cmd? ALST) ""))

  (check-pred (cmd? BACK) "back")
  (check-pred (cmd? BACK) "b")
  (check-pred (cmd? BACK) "up")
  (check-pred (cmd? BACK) "../")
  (check-pred (cmd? BACK) "cd ../")

  (check-false ((cmd? BACK) "back ARG"))
  (check-false ((cmd? BACK) "BACK"))
  (check-false ((cmd? BACK) "help"))
  (check-false ((cmd? BACK) ""))

  ;; -- DIVE command requires a single argument (doesn't fail for multiple arguments)
  (check-pred (cmd? DIVE) "dive ARG")
  (check-pred (cmd? DIVE) "d ARG")
  (check-pred (cmd? DIVE) "cd ARG")
  (check-pred (cmd? DIVE) "next ARG")

  (check-false ((cmd? DIVE) "step ARG1 ARG2 ARG3"))
  (check-false ((cmd? DIVE) "dive"))
  (check-false ((cmd? DIVE) "d"))
  (check-false ((cmd? DIVE) "quit"))
  (check-false ((cmd? DIVE) "a mistake"))

  ;; -- FIND command takes one argument, just like DIVE
  (check-pred (cmd? FIND) "find ARG")
  (check-pred (cmd? FIND) "search branch")
  (check-pred (cmd? FIND) "look up")

  (check-false ((cmd? FIND) "query "))
  (check-false ((cmd? FIND) "f ARG1 ARG2 ARG3"))
  (check-false ((cmd? FIND) "find"))
  (check-false ((cmd? FIND) "back"))
  (check-false ((cmd? FIND) "hello world"))

  (check-pred (cmd? HELP) "help")
  (check-pred (cmd? HELP) "h")
  (check-pred (cmd? HELP) "--help")
  (check-pred (cmd? HELP) "-help")

  (check-false ((cmd? HELP) "ohgosh"))
  (check-false ((cmd? HELP) "help me"))
  (check-false ((cmd? HELP) "lost"))
  (check-false ((cmd? HELP) "stuck, please help"))

  (check-pred (cmd? INFO) "info")
  (check-pred (cmd? INFO) "i")
  (check-pred (cmd? INFO) "print")
  (check-pred (cmd? INFO) "show")

  (check-false ((cmd? INFO) "println"))
  (check-false ((cmd? INFO) "help"))
  (check-false ((cmd? INFO) "display"))
  (check-false ((cmd? INFO) "write to out"))

  (check-pred (cmd? JUMP) "jump")
  (check-pred (cmd? JUMP) "j")
  (check-pred (cmd? JUMP) "warp")
  (check-pred (cmd? JUMP) "top")

  (check-false ((cmd? JUMP) "jump a"))
  (check-false ((cmd? JUMP) "w"))

  (check-pred (cmd? SAVE) "save")
  (check-pred (cmd? SAVE) "mark")

  (check-false ((cmd? SAVE) "lasd"))
  (check-false ((cmd? SAVE) "step"))
  (check-false ((cmd? SAVE) ""))
  (check-false ((cmd? SAVE) "save z"))

  (check-pred (cmd? QUIT) "q")
  (check-pred (cmd? QUIT) "quit")
  (check-pred (cmd? QUIT) "exit")
  (check-pred (cmd? QUIT) "leave")

  (check-false ((cmd? QUIT) "(quit)"))
  (check-false ((cmd? QUIT) "(exit)"))
  (check-false ((cmd? QUIT) "get me out of here"))

  ;; --- command implementations

  ;; -- dive end-to-end
  ;; Invalid command
  (let ([ctx '()]
        [hist '()]
        [arg "dive "]
        [pre-hist '(a aa)])
    (define-values (ctx* hist* pre*) (dive arg ctx hist pre-hist))
    (check-equal? ctx ctx*)
    (check-equal? hist hist*)
    (check-equal? pre-hist pre*)
    (check-pred read-line in))

  ;; List out-of-bounds
  (let ([ctx '((a) (b))]
        [hist '((c) (d))]
        [arg  "dive 2"]
        [pre-hist '(x y z)])
    (define-values (ctx* hist* pre*) (dive arg ctx hist pre-hist))
    (check-equal? ctx ctx*)
    (check-equal? hist hist*)
    (check-equal? pre-hist pre*)
    (check-pred read-line in))

  ;; List, in-bounds
  (let ([ctx '((a) (b))]
        [hist '((c) (d))]
        [arg "dive 0"]
        [pre-hist '('())])
    (define-values (ctx* hist* pre*) (dive arg ctx hist pre-hist))
    (check-equal? ctx*  (car ctx))
    (check-equal? hist* (cons ctx hist)))

  ;; List, search results. Ignores current history
  (let ([ctx  (list (result (zo) '()))]
        [hist '((c) (d))]
        [pre-hist '(blah)]
        [arg "dive 0"])
    (define-values (ctx* hist* pre*) (dive arg ctx hist pre-hist))
    (check-equal? ctx*  (result-zo (car ctx)))
    (check-equal? pre-hist pre*)
    (check-equal? hist* '()))

  ;; List, search results. Ignores current history, overwrites with search result history
  (let ([ctx  (list (result (zo) '(a a a)))]
        [hist '((c) (d))]
        [arg "dive 0"]
        [pre-hist '(a 78)])
    (define-values (ctx* hist* pre*) (dive arg ctx hist pre-hist))
    (check-equal? ctx*  (result-zo (car ctx)))
    (check-equal? hist* (result-path (car ctx)))
    (check-equal? pre-hist pre*))

  ;; zo, valid field
  (let* ([z (wrap '() '() '())]
         [ctx (stx-obj 0 z 'armed)]
         [hist '()]
         [pre-hist '(7 7 7)]
         [arg "dive wrap"])
    (define-values (ctx* hist* pre*) (dive arg ctx hist pre-hist))
    (check-equal? ctx*  z)
    (check-equal? hist* (cons ctx hist))
    (check-equal? pre-hist pre*))

  ;; zo, invalid field
  (let ([ctx (stx-obj 0 (wrap '() '() '()) 'armed)]
        [hist '()]
        [pre-hist '(a b x)]
        [arg "dive datum"])
    (define-values (ctx* hist* pre*) (dive arg ctx hist pre-hist))
    (check-equal? ctx*  ctx)
    (check-equal? hist* hist)
    (check-equal? pre* pre-hist)
    (check-pred read-line in))

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
                   (result (wrap '() '() '()) '(c))
                   (result (form) '(d)))]
        [hist '(e)]
        [arg "3"])
    (let-values ([(ctx* hist*) (dive-list ctx hist arg)])
      (begin (check-equal? ctx*  (result-zo (cadddr ctx)))
             (check-equal? hist* (result-path (cadddr ctx))))))

  ;; -- dive zo (I'm creating these zo structs arbitrarily,
  ;;             using the contracts in 'zo-lib/compiler/zo-structs.rkt')
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
  (let* ([z (wrap '() '() '())]
         [st (stx-obj 0 z 'clean)]
         [ctx (stx st)]
         [raw "find wrap"]
         [hist '(A)]
         [pre-hist '(a b)])
    (let-values ([(ctx* hist* pre-hist*) (find raw ctx hist pre-hist)])
      (begin (check-equal? (result-zo (car ctx*)) z)
             (check-equal? (result-path (car ctx*)) (list st))
             (check-equal? hist* '())
             (check-equal? pre-hist* (cons (cons ctx* (cons ctx hist)) pre-hist)))))
  (check-pred read-line in)
  (check-pred read-line in)
  (check-pred read-line in)

  ;; Failure, search 1 level down
  (let* ([z (wrap '() '() '())]
         [st (stx-obj 0 z 'clean)]
         [ctx (stx st)]
         [raw "find local-binding"]
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
             (check-equal? (result-zo (car ctx*))
                           (branch-then (branch-else (branch-else (branch-else ctx)))))
             (check-equal? (result-path (car ctx*))
                           (list (branch-else (branch-else (branch-else ctx)))
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

  (print-prompt '()) (displayln "")
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

  ;; -- has-any-flags?
  (define-syntax-rule (has-any-flags-test arg res)
    (check-equal? (has-any-flags? arg) res))

  (has-any-flags-test
    (vector "a" "b" "c")
    #f)

  (has-any-flags-test
    (vector "" "b" "c")
    #f)

  (has-any-flags-test
    (vector "file.zo")
    #f)

  (has-any-flags-test
    (vector "file.zo" "arg1" "arg2" "arg3")
    #f)

  (has-any-flags-test
    (vector "file.zo" "--help")
    #t)

  (has-any-flags-test
    (vector "--help" "file.zo")
    #t)

  (has-any-flags-test
    (vector "--help" "file.zo" "struct-name")
    #t)

  (has-any-flags-test
    (vector "-file.zo")
    #t)

  (has-any-flags-test
    (vector "file.zo" "struct1" "struct2" "-accident")
    #t)

  ;; -- starts-with
  (check-true (starts-with? "racket" ""))
  (check-true (starts-with? "racket" "r"))
  (check-true (starts-with? "racket" "rack"))
  (check-true (starts-with? "racket" "racket"))
  (check-false (starts-with? ""       "racket"))
  (check-false (starts-with? "racket" "R"))
  (check-false (starts-with? "racket" "rak"))
  (check-false (starts-with? "racket" "racket2"))

  ;; -- split-cd
  (check-equal? (split-cd '("")) '(""))
  (check-equal? (split-cd '("cd ../../")) '("cd .." "cd .."))
  (check-equal? (split-cd '("a" "b" "dive" "c")) '("a" "b" "dive" "c"))
  (check-equal? (split-cd '("a" "cd ../foo/bar" "car")) '("a" "cd .." "cd foo" "cd bar" "car"))

) ;; --- end testing
