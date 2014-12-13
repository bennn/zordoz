#lang racket/base

;; Command-line UI for exploring the output of zo-parse
(provide init)

(require compiler/zo-parse
         (only-in racket/string string-split string-join)
         (only-in racket/list   empty?)
         "color.rkt"
         "zo-find.rkt"
         "zo-string.rkt"
         "zo-transition.rkt")

;; --- constants

(define DEBUG   #f)
(define VERSION 0.1)
(define VNAME   "outlands")

;; --- misc

;; (define nat? natural-number/c)
;; (define context? (or/c zo? (listof zo?)))

;; Split [raw] by whitespace. Return the second element of the split, if any
;; otherwise return [#f].
(define (split-snd raw)
  ;; (-> string? (or/c #f string?))
  (define splt (string-split raw))
  (cond [(empty? splt)             #f]
        [(empty? (cdr splt))       #f]
        [(empty? (cdr (cdr splt))) (car (cdr splt))]
        [else                      (print-warn (format "Ignoring extra arguments: '~a'" (cdr (cdr splt))))
                                   (car (cdr splt))]))

;; --- REPL

(define (init-repl ctx)
  ;; (-> context? void?)
  (print-welcome)
  (repl ctx '()))

(define (repl ctx hist)
  ;; (-> context? history? void?)
  (when DEBUG (print-history hist))
  (print-prompt)
  (define raw (read-line))
  (cond [(back? raw) (cond [(empty? hist) (print-unknown raw)
                                          (repl ctx hist)]
                           [else          (let-values ([(a b) (pop hist)]) (repl a b))])]
        [(dive? raw) (let-values ([(ctx* hist*) (dive ctx hist raw)])
                       (repl ctx* hist*))]
        [(find? raw) (let ([ctx* (find ctx raw)])
                       (if (empty? ctx*)
                           (repl ctx hist)
                           (repl ctx* (push hist ctx))))]
        [(help? raw) (print-help)
                     (repl ctx hist)]
        [(info? raw) (print-context ctx)
                     (repl ctx hist)]
        [(quit? raw) (print-goodbye)]
        [else        (print-unknown raw)
                     (repl ctx hist)]))

;; --- commands

(define (back? raw)
  ;; (-> string? boolean?)
  (or (string=? raw "back")
      (string=? raw "b")
      (string=? raw "up")
      (string=? raw "u")
      (string=? raw "..")
      (string=? raw "../")))

(define (dive? raw)
  ;; (-> string? boolean?)
  (define hd (if (memq #\space (string->list raw))
                 (car (string-split raw))
                 ""))
  (or (string=? hd "dive")
      (string=? hd "next")
      (string=? hd "step")
      (string=? hd "d")))

(define (find? raw)
  ;; (-> string? boolean?)
  (define hd (if (memq #\space (string->list raw))
                 (car (string-split raw))
                 ""))
  (or (string=? hd "find")
      (string=? hd "f")
      (string=? hd "query")
      (string=? hd "search")
      (string=? hd "look")))
      

(define (help? raw)
  ;; (-> string? boolean?)
  (or (string=? raw "help")
      (string=? raw "h")
      (string=? raw "fuck")))

(define (info? raw)
  ;; (-> string? boolean?)
  (or (string=? raw "info")
      (string=? raw "i")
      (string=? raw "print")
      (string=? raw "p")
      (string=? raw "show")
      (string=? raw "ls")))

(define (quit? raw)
  ;; (-> string? boolean?)
  (or (string=? raw "q")
      (string=? raw "quit")
      (string=? raw "exit")))

(define (dive ctx hist raw)
  ;; (-> context? history? string? (values context? history?))
  (define arg (split-snd raw))
  (cond [(not arg)   (print-unknown raw)
                     (values ctx hist)]
        [(list? ctx) (dive-list ctx hist arg)]
        [(zo?   ctx) (dive-zo   ctx hist arg)]
        [else        (error (format "Invalid context '~a'" ctx))]))

(define (dive-list ctx hist arg)
  ;; (-> context? history? string? (values context? history?))
  (let ([index (string->number arg)])
    (cond [(or (not index)
               (< index 0)
               (>= index (length ctx))) (print-unknown (format "dive ~a" arg))
                                        (values ctx hist)]
          [else (values (list-ref ctx index) (push hist ctx))])))
  
(define (dive-zo ctx hist field)
  ;; (-> context? history? string? (values context? history?))
  (let-values ([(ctx* success?) (transition ctx field)])
    (cond [success? (values ctx* (push hist ctx))]
          [else     (print-unknown (format "dive ~a" field))
                    (values ctx hist)])))

(define (find ctx raw)
  (define arg (split-snd raw))
  (define results (if arg (zo-find ctx arg) '()))
  (printf "FIND returned ~a results\n" (length results))
  results)
               
;; --- history

;; (define history? (listof context?))

(define (push hist ctx)
  ;; (-> history? context? history?)
  (cons ctx hist))

(define (pop hist)
  ;; (-> history? (values context? history?))
  (values (car hist) (cdr hist)))

;; --- loading

;; Start REPL from a filename
(define (init-from-filename name)
  ;; (-> string? void?)
  (print-info (format "Loading bytecode file '~a'..." name))
  (define port (open-input-file name))
  (print-info "Parsing bytecode...")
  (define ctx  (zo-parse port))
  (print-info "Parsing complete!")
  (init-repl ctx))

;; --- print

(define (print-history hist)
  ;; (-> history? void?)
  (printf "History is: ~a\n" hist))

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

(define (print-context ctx)
  ;; (-> context? void?)
  (cond [(zo?   ctx) (displayln (zo->string ctx))]
        [(list? ctx) (printf "~a[~a]\n"
                             (map (lambda (z) (zo->string z #:deep? #f)) ctx)
                             (length ctx))]
        [else        (error (format "Unknown context '~a'"  ctx))]))

(define (print-unknown raw)
  ;; (-> string? void?)
  (printf "'~a' not permitted.\n" raw))

(define (print-goodbye)
  ;; (-> void?)
  (printf "Ascending to second-level meditation. Goodbye.\n\n"))

(define (print-debug str)
  ;; (-> string? void?)
  (printf "DEBUG: ~a\n" str))

(define (print-welcome)
  ;; (-> void?)
  (printf "--- Welcome to the .zo shell, version ~a '~a' ---\n" VERSION VNAME))

(define (print-prompt)
  ;; (-> void?)
  (display "\033[1;32mzo> \033[0;0m"))

(define (print-info str)
  ;; (-> string? void?)
  (printf "INFO: ~a\n" str))

(define (print-warn str)
  ;; (-> string? void?)
  (printf "WARN: ~a\n" str))

(define (print-error str)
  ;; (-> string? void?)
  (printf "ERROR: ~a\n" str))

;; Print usage information
(define (print-usage)
  (displayln "Usage: zo-shell <file.zo>"))

;; --- main

;; Enter here
(define (init)
  ;; (-> void?)
  (define args (vector->list (current-command-line-arguments)))
  (cond [(empty? args)       (print-usage)]
        [(empty? (cdr args)) (init-from-filename (car args))]
        [else                (print-usage)]))