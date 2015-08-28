#lang info
(define collection "zordoz")
(define deps '(("base" #:version "6.2")
               "compiler-lib"
               "rackunit-lib"
               "zo-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define pkg-desc "REPL for exploring .zo bytecode files")
(define version "0.6")
(define pkg-authors '(ben))
(define raco-commands '(("zordoz" (submod zordoz/zordoz main) "open a REPL for a bytecode file (aka 'zo Explorer')" #f)))
