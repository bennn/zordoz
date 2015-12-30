#lang racket/base

;; Readline bindings, without the readline

(provide
  readline-prompt
  (rename-out [winread read-line]))

;; =============================================================================

(define readline-prompt (make-parameter #"> "))

(define (winread)
  (display (readline-prompt))
  (read-line))
