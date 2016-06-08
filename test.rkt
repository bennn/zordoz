#lang typed/racket/base

(require zordoz/typed)

(module+ main
  (for ((x (in-vector (current-command-line-arguments))))
    (find-all x '("branch"))))
