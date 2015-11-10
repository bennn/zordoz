#lang racket/base

(provide
  readline-prompt
  (rename-out [winread read-line]))

;; =============================================================================

(define readline-prompt (make-parameter #"> "))

(define (winread)
  (display (readline-prompt))
  (read-line))