#lang typed/racket/base

;; Command-line UI for exploring decompiled bytecode.
;; (Use `raco make` to generate bytecode)

(provide
 ;; Start a REPL using command-line arguments
 init)

(require racket/match
         (only-in racket/string string-split string-join)
         "typed-zo-structs.rkt"
         (only-in "zo-string.rkt" zo->string)
         (only-in "zo-transition.rkt" zo-transition)
         (only-in "zo-find.rkt" zo-find result result? result-z result-path))
(require/typed compiler/zo-parse
               [zo-parse (->* () (Input-Port) zo)])

;; -----------------------------------------------------------------------------

;; --- constants & contracts

;; when set, print extra debugging information
(: DEBUG Boolean)
(define DEBUG   #f)
;; For aesthetic purposes
(: VERSION String)
(define VERSION "1.0")
(: VNAME String)
(define VNAME   "vortex")

(define-type Context (U zo (Listof zo) (Listof result)))
(define-type History (Listof Context))
(define-type PreHistory (Listof History))

;; --- API functions

;; Entry point to the REPL, expects command-line arguments passed as a list.
;; In the future, there may be more entry points.
(: init (-> (Vectorof String) Void))
(define (init args)
  (match args
    ['#()
     (print-usage)]
    [(vector fname)
     (init-from-filename fname)]
    [(vector fname args ...)
     (find-all fname args)]))

;; --- Commands (could go in their own file)

(struct command ([name : String]
                 [num-args : Index]
                 [aliases : (Listof String)]
                 [help-msg : String]))
(: ALST command)
(define ALST (command "alst"
                      0
                      (list "a" "alias" "aliases")
                      "Print command aliases"))
(: BACK command)
(define BACK (command "back"
                      0
                      (list "b" "up" "u" ".." "../" "cd .." "cd ../")
                      "Move up to the previous context"))
(: DIVE command)
(define DIVE (command "dive"
                      1
                      (list "d" "cd" "next" "step")
                      "Step into struct field ARG"))
(: FIND command)
(define FIND (command "find"
                      1
                      (list "f" "query" "search" "look")
                      "Search the current subtree for structs with the name ARG"))
(: HELP command)
(define HELP (command "help"
                      0
                      (list "h" "-h" "--h" "-help" "--help")
                      "Print this message"))
(: INFO command)
(define INFO (command "info"
                      0
                      (list "i" "ls" "print" "p" "show")
                      "Show information about current context"))
(: JUMP command)
(define JUMP (command "jump"
                      0
                      (list "j" "warp" "top")
                      "Revert to last saved position"))
(: SAVE command)
(define SAVE (command "save"
                      0
                      (list "mark")
                      "Save the current context as jump target"))
(: QUIT command)
(define QUIT (command "quit"
                      0
                      (list "q" "exit" "leave" "bye")
                      "Exit the interpreter"))
(: COMMANDS (Listof command))
(define COMMANDS
  (list ALST BACK DIVE FIND HELP INFO JUMP SAVE QUIT))

(: cmd? (-> command (-> String Boolean)))
(define ((cmd? c) str)
  (define splt (string-split str))
  (or
   ;; Special cases
   (and (string=? "back" (command-name c))
        (member? str (list "cd .." "cd ../")))
   ;; Everything else
   (and
    ;; Has the right number of arguments
    (= (sub1 (length splt))
       (command-num-args c))
    ;; First word matches command name (or an alias)
    (or (string=? (car splt) (command-name c))
        (member?   (car splt) (command-aliases c))))))

;; --- REPL

;; Start REPL from a filename
(: init-from-filename (-> String Void))
(define (init-from-filename name)
  ;; (-> string? void?)
  (print-info (format "Loading bytecode file '~a'..." name))
  (call-with-input-file name
    (lambda ([port : Input-Port])
      (print-info "Parsing bytecode...")
      (define ctx  (zo-parse port))
      (print-info "Parsing complete!")
      (print-welcome)
      (repl ctx '() '()))))

;; The REPL loop. Process a command using context `ctx` and history `hist`.
(: repl (-> Context History PreHistory Void))
(define (repl ctx hist pre-hist)
  (when DEBUG (print-history hist))
  (print-prompt)
  (: input (U EOF String))
  (define input (read-line))
  (when (eof-object? input)
    (error "caught EOF in REPL"))
  (match input
    [(? (cmd? ALST) raw)
     (print-alias) (repl ctx hist pre-hist)]
    [(? (cmd? BACK) raw)
     (call-with-values (lambda () (back raw ctx hist pre-hist)) repl)]
    [(? (cmd? DIVE) raw)
     (call-with-values (lambda () (dive raw ctx hist pre-hist)) repl)]
    [(? (cmd? FIND) raw)
     (call-with-values (lambda () (find raw ctx hist pre-hist)) repl)]
    [(? (cmd? HELP) raw)
     (print-help) (repl ctx hist pre-hist)]
    [(? (cmd? INFO) raw)
     (print-context ctx) (repl ctx hist pre-hist)]
    [(? (cmd? JUMP) raw)
     (call-with-values (lambda () (jump raw ctx hist pre-hist)) repl)]
    [(? (cmd? SAVE) raw)
     (call-with-values (lambda () (save raw ctx hist pre-hist)) repl)]
    [(? (cmd? QUIT) raw)
     (print-goodbye)]
    [raw
     (print-unknown raw) (repl ctx hist pre-hist)]))

;; --- command implementations

;; 2015-01-23: Warn about possible-unexpected behavior
(: BACK-WARNING String)
(define BACK-WARNING
  (string-append
   "BACK removing most recent 'save' mark. "
   "Be sure to save if you want to continue exploring search result."))
  
;; Step back to a previous context, if any, and reduce the history.
;; Try popping from `hist`, fall back to list-of-histories `pre-hist`.
(: back (-> String Context History PreHistory (values Context History PreHistory)))
(define (back raw ctx hist pre-hist)
  (match (list hist pre-hist)
    [(list '() '())
     ;; Nothing to pop from
     (print-unknown raw)
     (values ctx hist pre-hist)]
    [(list '() (cons hist* pre-hist*))
     ;; Pop from pre-history
     (displayln BACK-WARNING)
     (back raw ctx hist* pre-hist*)]
    [(list (cons ctx* hist*) _)
     (values ctx* hist* pre-hist)]))

;; Search context `ctx` for a new context matching string `raw`.
;; Push `ctx` onto the stack `hist` on success.
(: dive (-> String Context History PreHistory (values Context History PreHistory)))
(define (dive raw ctx hist pre-hist)
  ;; (-> string? context? history? (listof history?) (values context? history? (listof history?)))
  (: arg (U String #f))
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
      (error (format "Invalid context '~a'" ctx))]))
  ;; Return pre-hist unchanged
  (values ctx* hist* pre-hist))

;; Parse the string `arg` to an integer n.
;; If n is within the bounds of the list `ctx`,
;; push `ctx` onto the stack `hist` and return the n-th element of `ctx`.
;; Otherwise, return `ctx` and `hist` unchanged.
(: dive-list (-> (U (Listof zo) (Listof result)) History String (values Context History)))
(define (dive-list ctx hist arg)
  (: index (U #f Index))
  (define index
    (let: ([res : (U #f Number) (string->number arg)])
      (if (index? res) res #f)))
  (cond [(or (not index) (< index 0) (>= index (length ctx)))
         ;; Index out of bounds, or not a number. Cannot dive.
         (print-unknown (format "dive ~a" arg))
         (values ctx hist)]
        [else
         ;; Select from list,
         (: res (U zo result))
         (define res (list-ref ctx index))
         ;; If list elements are search results, current `hist` can be safely ignored.
         (if (result? res)
             (values (result-z res) (result-path res))
             (values res             (push hist ctx)))]))

;; Use the string `field` to access a field in the zo struct `ctx`.
;; If the field exists and denotes another zo struct, return that
;; struct and push `ctx` on to the stack `hist`.
;; Otherwise, return `ctx` and `hist` unchanged.
(: dive-zo (-> zo History String (values Context History)))
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
(: find (-> String Context History PreHistory (values Context History PreHistory)))
(define (find raw ctx hist pre-hist)
  (: arg (U #f String))
  (define arg (split-snd raw))
  (cond [(and arg (zo? ctx))
         (: results (Listof result))
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
(: jump (-> String Context History PreHistory (values Context History PreHistory)))
(define (jump raw ctx hist pre-hist)
  (match pre-hist
    ['()
     ;; Nothing to jump to
     (print-unknown raw)
     (values ctx hist pre-hist)]
    [(cons hist* pre-hist*)
     (back raw ctx hist* pre-hist*)]))

;; Save the current context and history to the pre-history
;; For now, erases current history. 
(: save (-> String Context History PreHistory (values Context History PreHistory)))
(define (save raw ctx hist pre-hist)
  (values ctx '() (cons (push hist ctx) pre-hist)))

;; --- history manipulation

;; Add the context `ctx` to the stack `hist`.
(: push (-> History Context History))
(define (push hist ctx)
  (cons ctx hist))

;; Remove the top context from the stack `hist`.
;; Return the popped value and tail of `hist`.
;; Callers must avoid calling `pop` on empty stacks.
(: pop (-> History (values Context History)))
(define (pop hist)
  ;; (-> history? (values context? history?))
  (values (car hist) (cdr hist)))

;; --- print
(: print-alias (-> Void))
(define (print-alias)
  ;; (-> void?)
  (displayln "At your service. Command aliases:")
  (displayln
   (string-join
    (for/list : (Listof String) ([cmd : command COMMANDS])
      (format "  ~a        ~a"
              (command-name cmd)
              (string-join (command-aliases cmd))))
   "\n")))

;; Print a history object.
(: print-history (-> History Void))
(define (print-history hist)
  ;; (-> history? void?)
  (printf "History is: ~a\n" hist))

;; Print a help message for the REPL.
(: print-help (-> Void))
(define (print-help)
  ;; (-> void?)
  (displayln "At your service. Available commands:")
  (displayln
   (string-join
    (for/list : (Listof String) ([cmd : command COMMANDS])
      (format "  ~a~a    ~a"
              (command-name cmd)
              (if (= 1 (command-num-args cmd)) " ARG" "    ") ;; hack
              (command-help-msg cmd)))
    "\n")))

;; Print a context.
(: print-context (-> Context Void))
(define (print-context ctx)
  (match ctx
    [(? zo?)
     (displayln (zo->string ctx))]
    ['()
     (displayln "'()")]
    [(cons x _)
     (: z zo)
     (define z (if (result? x) (result-z x) x))
     (printf "~a[~a]\n"
             (zo->string z #:deep? #f)
             (length ctx))]
    [_
     (error (format "Unknown context '~a'"  ctx))]))

;; Print an error message (after receiving an undefined/invalid command).
(: print-unknown (-> String Void))
(define (print-unknown raw)
  ;; (-> string? void?)
  (printf "'~a' not permitted.\n" raw))

;; Print a goodbye message (when the user exits the REPL).
(: print-goodbye (-> Void))
(define (print-goodbye)
  ;; (-> void?)
  (printf "Ascending to second-level meditation. Goodbye.\n\n"))

;; Print a debugging message.
(: print-debug (-> String Void))
(define (print-debug str)
  ;; (-> string? void?)
  (printf "DEBUG: ~a\n" str))

;; Print a welcome message (when the user enters the REPL).
(: print-welcome (-> Void))
(define (print-welcome)
  ;; (-> void?)
  (display
   (format "\033[1;34m--- Welcome to the .zo shell, version ~a '~a' ---\033[0;0m\n" VERSION VNAME)))

;; Print the REPL prompt.
(: print-prompt (-> Void))
(define (print-prompt)
  ;; (-> void?)
  (display "\033[1;32mzo> \033[0;0m"))

;; Print an informative message.
(: print-info (-> String Void))
(define (print-info str)
  ;; (-> string? void?)
  (printf "INFO: ~a\n" str))

;; Print a warning.
(: print-warn (-> String Void))
(define (print-warn str)
  ;; (-> string? void?)
  (printf "WARN: ~a\n" str))

;; Print an error message.
(: print-error (-> String Void))
(define (print-error str)
  ;; (-> string? void?)
  (printf "ERROR: ~a\n" str))

;; Print usage information.
(: print-usage (-> Void))
(define (print-usage)
  (displayln "Usage: zo-shell FILE.zo"))

;; --- misc
(: find-all (-> String (Listof String) Void))
(define (find-all name args)
  ;; (-> string? (vectorof string?) void)
  (print-info (format "Loading bytecode file '~a'..." name))
  (call-with-input-file name
    (lambda ([port : Input-Port])
      (print-info "Parsing bytecode...")
      (: ctx zo)
      (define ctx (zo-parse port))
      (print-info "Parsing complete! Searching...")
      (for : Void ([arg : String (in-list args)])
        (printf "FIND '~a' : " arg)
        (printf "~a results\n" (length (zo-find ctx arg))))
      (displayln "All done!"))))

;; Split the string `raw` by whitespace and
;; return the second element of the split, if any.
;; Otherwise return `#f`.
(: split-snd (-> String (U String #f)))
(define (split-snd raw)
  ;; (-> string? (or/c #f string?))
  (define splt (string-split raw))
  (match splt
    [(list _ x)       x]
    [(list _ x ys ...) (print-warn (format "Ignoring extra arguments: '~a'" ys))
                       x]
    [_ #f]))

;; Why isn't this standard?
(: member? (All (A) (-> A (Listof A) Boolean)))
(define (member? x xs)
  (if (member x xs) #t #f))

