#lang typed/racket/base

(require compiler/zo-structs
         zordoz/typed/private/zo-string
         zordoz/typed/private/zo-transition
         zordoz/typed/private/zo-find
         zordoz/typed/private/zo-shell
         zordoz/typed/private/zo-syntax)

(provide
  result result? result-zo result-path
  ;; Struct wrapping results of `find` queries.
  ;; Contains a zo struct & its path from the search root

  zo->string
  ;; Render a zo struct as a string.
  ;; Optional argument #:deep? determines whether to render the struct's
  ;;  fields, or just its name.

  zo->spec
  ;; Convert a zo struct to a string specification.
  ;; Specifications are structured strings -- they are lists with
  ;; the same number of fields as the struct they represent.

  zo-transition
  ;; (zo-transition z s) retrieves the field named `s` from the
  ;;  zo struct `z`, provided:
  ;; - this field `s` exists
  ;; - the type of `s` is a zo struct (and not an integer, list, ...)

  zo-find
  ;; Recursively search a zo struct for sub-structures
  ;;  with names exactly matching the argument string.
  ;; Matching structs are return along with the path taken to reach them

  zo->shell
  ;; Start a REPL session to explore a zo struct

  syntax->shell
  ;; Start a REPL session to explore a syntax object

  syntax->zo
  ;; Compile a syntax object to a zo struct

  syntax->decompile
  ;; Compile a syntax object, then decompile the result to an S-expression

  zo->compiled-expression
  ;; Compile a zo struct

)
