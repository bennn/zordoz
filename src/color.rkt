#lang racket/base

(provide red
         green
         blue
         purple
         cyan)

;; Add colors to text using ANSI escape sequences
;; http://tldp.org/HOWTO/Bash-Prompt-HOWTO/x329.html
;; (define ANSI-NOTHING "0")
;; (define ANSI-BLACK   "0;30")
;; (define ANSI-RED     "0;31")
;; (define ANSI-GREEN   "0;32")
;; (define ANSI-BROWN   "0;33")
;; (define ANSI-BLUE    "0;34")
;; (define ANSI-PURPLE  "0;35")
;; (define ANSI-CYAN    "0;36")
;; (define ANSI-GREY    "0;37")

(define (red str)
  (format "\033[0;31m~a\033[0m" str))

(define (green str)
  (format "\033[1;32m~a\033[0m" str))

(define (blue str)
  (format "\033[0;34m~a\033[0m" str))

(define (purple str)
  (format "\033[1;35m~a\033[0m" str))

(define (cyan str)
  (format "\033[1;36m~a\033[0m" str))
