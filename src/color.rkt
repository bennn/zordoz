#lang racket/base

;; (require (only-in racket/list memq))

;; Add colors to text using ANSI escape sequences
;; http://tldp.org/HOWTO/Bash-Prompt-HOWTO/x329.html

(provide color)

;; A few colors
(define ANSI-NOTHING "0")
(define ANSI-BLACK   "0;30")
(define ANSI-RED     "0;31")
(define ANSI-GREEN   "0;32")
(define ANSI-BROWN   "0;33")
(define ANSI-BLUE    "0;34")
(define ANSI-PURPLE  "0;35")
(define ANSI-CYAN    "0;36")
(define ANSI-GREY    "0;37")
;; (define color? (or/c 'NOTHING 'BLACK 'RED 'GREEN 'BROWN 'BLUE 'PURPLE 'CYAN 'GREY))

;; Apply a color to a string
;; Example: 'echo  "\033[0;32m hello \033[0"'
(define (color-aux c s)
  ;; (-> string? string? string?)
  ;; (string-append "\033[" c s "\033[0m" " "))
  ;; FUCK
  (format "\033[~s ~s\033[0m" c s))

;; "\033[0;32m hello \033[0m"

;; [color flag s] Color the string [s] using the color denoted by [flag].
(define (color flag s)
  ;; (-> color? string? string?)
  (cond [(eq? flag 'NOTHING) (color-aux ANSI-NOTHING s)]
        [(eq? flag 'BLACK)   (color-aux ANSI-BLACK s)]
        [(eq? flag 'RED)     (color-aux ANSI-RED s)]
        [(eq? flag 'GREEN)   (color-aux ANSI-GREEN s)]
        [(eq? flag 'BROWN)   (color-aux ANSI-BROWN s)]
        [(eq? flag 'BLUE)    (color-aux ANSI-BLUE s)]
        [(eq? flag 'PURPLE)  (color-aux ANSI-PURPLE s)]
        [(eq? flag 'CYAN)    (color-aux ANSI-CYAN s)]
        [(eq? flag 'GREY)    (color-aux ANSI-GREY s)]
        [else                (error (format "Unknown color '~a'" flag))]))

