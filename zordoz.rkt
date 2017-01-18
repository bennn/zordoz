#lang racket/base

;; Executing this file starts a new REPL session.

(module+ main
  (require racket/cmdline
           racket/pretty
           (prefix-in u: zordoz/private/zo-shell))
  ;; -- parameters
  (define search-limit (make-parameter #f))
  (define start-repl? (make-parameter #t))
  (define just-print? (make-parameter #f))
  (define to-find (make-parameter '()))
  ;; -- helpers
  (define (assert-zo filename)
    (define offset (- (string-length filename) 3))
    (or (and (positive? offset)
             (equal? ".zo" (substring filename offset)))
        (and (u:print-usage)
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
   [("-p" "--print")
    "Just the the parsed zo to STDOUT, do not open a REPL"
    (just-print? #t)]
   #:args (filename)
   (when (assert-zo filename)
     (cond
      [(just-print?)
       (pretty-print (u:filename->zo filename))]
      [(start-repl?)
       (u:filename->shell filename)]
      [else
       (u:find-all filename (to-find) #:limit (search-limit))])))
)
