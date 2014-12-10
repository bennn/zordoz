#lang racket/base

;; Command-line UI for exploring the output of zo-parse


(require compiler/zo-parse
         compiler/zo-structs)

(define (repl ctx)
  ;; (-> context? void?)
  )

;; Start REPL from a filename
(define (init-from-filename name)
  ;; (-> string? void?)
  )

;; Print usage information
(define (print-usage)
  (displayln "Usage: zo-shell <file.zo>"))

;; Enter here
(define (init)
  ;; (-> void?)
  (define args (vector->list (current-command-line-args)))
  (cond [(empty? args)       (print-usage)]
        [(empty? (cdr args)) (init-from-filename (car args))]
        [else                (print-usage)]))
(init)
