#lang typed/racket/base

(require
  zordoz/typed/zo-structs
  typed/racket/unsafe)

(require/typed zordoz/private/zo-find
  (#:opaque result result?)
  (result-zo (-> result Zo))
  (result-path (-> result (Listof Zo))))

;; -----------------------------------------------------------------------------

(unsafe-require/typed zordoz/private/zo-string
  (zo->string (->* [Zo] [#:deep? Boolean] String))
  (zo->spec (-> Zo Spec)))

(unsafe-require/typed zordoz/private/zo-transition
  (zo-transition (-> Zo String (Values (U Zo (Listof Zo)) Boolean))))

(unsafe-require/typed zordoz/private/zo-find
  (zo-find (->* [Zo] [#:limit (U Natural #f)] (Listof result))))

(unsafe-require/typed zordoz/private/zo-shell
  (filename->shell (-> Path-String Void))
  (zo->shell (-> Zo Void))
  (syntax->shell (-> Syntax Void)))

(unsafe-require/typed zordoz/private/zo-syntax
  (compiled-expression->zo (-> Compiled-Expression Zo))
  (syntax->zo (-> Syntax Zo))
  (syntax->decompile (-> Syntax Any))
  (toplevel-syntax->zo (-> Syntax (Listof Zo)))
  (zo->compiled-expression (-> Zo Compiled-Expression)))

;; =============================================================================

(provide
  result result? result-zo result-path
  zo->string
  zo->spec
  zo-transition
  zo-find
  filename->shell
  zo->shell
  syntax->shell
  compiled-expression->zo
  syntax->zo
  syntax->decompile
  toplevel-syntax->zo
  zo->compiled-expression
)
