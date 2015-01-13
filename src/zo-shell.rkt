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

;; Entry point to the REPL. Loads a file using a single command line argument.
;; In the future, there may be more entry points.
(define (init)
  ;; (-> void?)
  (define args (vector->list (current-command-line-arguments)))
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

;; -- testing

(module+ test
  (require rackunit)
  (check-equal? #t #t)
)
