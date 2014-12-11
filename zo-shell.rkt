#lang racket/base

;; Command-line UI for exploring the output of zo-parse


(require compiler/zo-parse
         (only-in racket/string string-split string-join)
         "zo-string.rkt"
         "zo-transition.rkt")

;; --- constants

(define DEBUG   #t)
(define VERSION 0.1)
(define VNAME   "outlands")

;; --- REPL

;; (define context? (or/c zo? (listof zo?)))

(define (init-repl ctx)
  ;; (-> context? void?)
  (begin
    (print-welcome)
    (repl ctx '())
  ))

(define (repl ctx hist)
  ;; (-> context? history? void?)
  (begin
    (print-prompt)
    (define raw (read-line))
    (cond [(quit? raw) (print-goodbye)]
          [(help? raw) (begin (print-help) (repl ctx hist))]
          [(info? raw) (begin (print-context ctx) (repl ctx hist))]
          [(dive? raw) (let-values ([(ctx* hist*) (dive ctx hist raw)])
                         (repl ctx* hist*))]
          [(back? raw) (if (empty? hist)
                           (begin (print-unknown raw) (repl ctx hist))
                           (let-values ([(a b) (pop hist)]) (repl a b)))]
          [else        (begin (print-unknown raw) (repl ctx hist))])
  ))

;; --- commands

(define (quit? raw)
  ;; (-> string? boolean?)
  (or (string=? raw "q")
      (string=? raw "quit")
      (string=? raw "exit")))

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

(define (dive? raw)
  ;; (-> string? boolean?)
  (define hd
    (let* ([strs (string-split raw)]
           [splt (if (empty? strs) (cons "" '()) strs)])
      (string-slice (car splt) 0 3)))
  (or (string=? hd "dive")
      (string=? hd "next")
      (string=? hd "step")
      (string=? hd "d")))

(define (back? raw)
  ;; (-> string? boolean?)
  (or (string=? raw "back")
      (string=? raw "b")
      (string=? raw "up")
      (string=? raw "u")
      (string=? raw "..")
      (string=? raw "../")))

(define (dive ctx hist raw)
  ;; (-> context? history? string? (values context? history?))
  (define arg ;; parse [raw] for field name i.e. second argument in [raw]
    (let ([splt (string-split raw)])
      (cond [(empty? splt)             #f]
            [(empty? (cdr splt))       #f]
            [(empty? (cdr (cdr splt))) (car (cdr splt))]
            [else                      (begin (print-warn (format "Ignoring extra arguments to dive: '~a'" (cdr (cdr splt))))
                                              (car (cdr splt)))])))
  (cond [(not arg) (begin (print-unknown (format "dive ~a" raw))
                            (values ctx hist))]
        [(list? ctx) (dive-list ctx hist arg)]
        [(zo?   ctx) (dive-zo   ctx hist arg)]
        [else (error (format "Invalid context '~a'" ctx))]))

(define (dive-list ctx hist arg)
  ;; (-> context? history? string? (values context? history?))
  (let ([index (string->number arg)])
    (cond [(or (not index)
               (< index 0)
               (>= index (length ctx))) (begin (print-unknown (format "dive ~a" arg))
                                               (values ctx hist))]
          [else (values (list-ref ctx index) (push ctx hist))])))
  
(define (dive-zo ctx hist field)
  ;; (-> context? history? string? (values context? history?))
  (let-values ([(ctx* success?) (transition ctx field)])
    (if success?
        (values ctx* (push ctx hist))
        (begin (print-unknown (format "dive ~a" field))
               (values ctx hist)))))

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
  (begin
    (print-info (format "Loading bytecode file '~a'..." name))
    (define port (open-input-file name))
    (print-info "Parsing bytecode...")
    (define ctx  (zo-parse port))
    (print-info "Parsing complete!")
    (init-repl ctx)
  ))

;; --- print

(define (print-help)
  ;; (-> void?)
  (displayln (string-join (list "At your service. Available commands:"
                                "  quit        Exit the interpreter"
                                "  help        Print this message"
                                "  info        Show information about current context"
                                "  dive ARG    Step into struct field ARG"
                                "  back        Move up to the previous context"
                                )
                          "\n")))

(define (print-context ctx)
  ;; (-> context? void?)
  ;; TODO ctx is list, sometimes
  (displayln (zo->string ctx)))

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
  (printf "zo> "))

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

;; --- misc

;; Create a new string from characters in [str].
;; Begins at [start-i], ends no further than [end-i].
;; If [str] has too few characters then the result will be shorter than [end-i - start-i]
(define (string-slice str start-i end-i)
  ;; (-> string? nat? nat? string?)
  (define end* (min end-i (sub1 (string-length str))))
  (cond [(string=? "" str) ""]
        [(< start-i 0)    (error (format "[string-slice] invalid start index '~a'." start-i))]
        [(< end* start-i) (error (format "[string-slice] invalid end index '~a' for start index '~a'." end* start-i))]
        [else             (string-slice-aux str start-i end* '())]))

(define (string-slice-aux str curr-i end-i acc)
  ;; (-> string? nat? nat? (listof character?) string?)
  (define c    (string-ref str curr-i))
  (define acc* (cons c acc))
  (cond [(= curr-i end-i) (list->string (reverse acc*))]
        [else             (string-slice-aux str (add1 curr-i) end-i acc*)]))

(define (empty? xs)
  ;; (-> list? boolean?)
  (eq? '() xs))

;; (define nat? natural-number/c)

;; --- main

;; Enter here
(define (init)
  ;; (-> void?)
  (define args (vector->list (current-command-line-arguments)))
  (cond [(empty? args)       (print-usage)]
        [(empty? (cdr args)) (init-from-filename (car args))]
        [else                (print-usage)]))
(init)
