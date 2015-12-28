#lang racket/base

(require racket/contract
         compiler/zo-structs
         zordoz/private/zo-string
         zordoz/private/zo-transition
         zordoz/private/zo-find
         zordoz/private/zo-shell
         zordoz/private/zo-syntax)

(provide result result? result-zo result-path)
(provide (contract-out
          [zo->string (->* (zo?) (#:deep? boolean?) string?)]
          ;; Render a zo struct as a string.
          ;; Optional argument #:deep? determines whether to render the struct's
          ;;  fields, or just its name.

          [zo->spec (->i ([z zo?]) () [res (z) (and/c spec/c (specof z))])]
          ;; Convert a zo struct to a string specification.
          ;; Specifications are structured strings -- they are lists with
          ;; the same number of fields as the struct they represent.

          [zo-transition (-> zo? string? (values (or/c zo? (listof zo?)) boolean?))]
          ;; (zo-transition z s) retrieves the field named `s` from the
          ;;  zo struct `z`, provided:
          ;; - this field `s` exists
          ;; - the type of `s` is a zo struct (and not an integer, list, ...)

          [zo-find (->* [zo? string?] [#:limit (or/c natural-number/c #f)] (listof result?))]
          ;; Recursively search a zo struct for sub-structures
          ;;  with names exactly matching the argument string.
          ;; Matching structs are return along with the path taken to reach them

          [filename->shell (-> path-string? void?)]
          ;; Start a REPL session to explore a .zo bytecode file

          [zo->shell (-> zo? void?)]
          ;; Start a REPL session to explore a zo struct

          [syntax->shell (-> syntax? void?)]
          ;; Start a REPL session to explore a syntax object

          [compiled->zo (-> compiled-expression? zo?)]
          ;; Convert a compiled expression into a zo struct

          [syntax->zo (-> syntax? zo?)]
          ;; Compile a syntax object to a zo struct

          [syntax->decompile (-> syntax? any/c)]
          ;; Compile a syntax object, then decompile the result to an S-expression

          [zo->compiled-expression (-> zo? compiled-expression?)]
          ;; Compile a zo struct
))
