#lang info
(define collection "zordoz")
(define deps '("base" ;; Expects 6.2.900.15 or greater
               "compiler-lib"
               "zo-lib"
               "typed-racket-lib"
               "typed-racket-more"))
(define build-deps '("cover"
                     "cover-coveralls"
                     "rackunit-lib"
                     "scribble-lib"
                     "racket-doc"))
(define pkg-desc "REPL for exploring .zo bytecode files")
(define version "0.6")
(define pkg-authors '(ben))
(define raco-commands '(("zordoz" (submod zordoz/zordoz main) "open a REPL for a bytecode file (aka 'zo Explorer')" #f)))
(define scribblings '(("scribblings/zordoz.scrbl")))
