#lang racket/base

;; Executing this file starts a new REPL session.

(module+ main
  (require racket/cmdline
           (only-in "private/zo-shell.rkt" filename->shell find-all print-usage))
  ;; -- parameters
  (define search-limit (make-parameter #f))
  (define start-repl? (make-parameter #t))
  (define to-find (make-parameter '()))
  ;; -- helpers
  (define (assert-zo filename)
    (define offset (- (string-length filename) 3))
    (or (and (positive? offset)
             (equal? ".zo" (substring filename offset)))
        (and (print-usage)
             #f)))
  ;; -- commandline
  (command-line
   #:program "zordoz"
   #:multi
   [("-f" "--find")
    f*
    "Name of zo structs to find"
    (begin
      (start-repl? #f)
      (to-find (cons f* (to-find))))]
   #:once-each
   [("-l" "--limit")
    l
    "Maximum depth to search during --find queries"
    (search-limit l)]
   #:args (filename)
   (when (assert-zo filename)
     (if (start-repl?)
         (filename->shell filename)
         (find-all filename (to-find) #:limit (search-limit)))))
)
