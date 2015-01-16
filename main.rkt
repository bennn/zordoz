#lang racket/base

;; Executing this file starts a new REPL session.

(module+ main
  (require (only-in "src/zo-shell.rkt" init))
  (init (vector->list (current-command-line-arguments))))
