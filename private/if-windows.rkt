#lang racket/base

;; Conditional "if windows" compile-time check

(provide if-windows)

(require (for-syntax racket/base syntax/parse))

;; =============================================================================

(define-syntax (if-windows stx)
  (syntax-parse stx
   [(_ yes no)
    (case (system-type 'os)
      [(windows) (syntax/loc stx yes)]
      [(unix macosx) (syntax/loc stx no)])]
   [_ (error 'if-windows
        (format "Expected (if-windows YES NO), got '~a'" (syntax->datum stx)))]))

