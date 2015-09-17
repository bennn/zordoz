#lang racket/base

(require racket/contract
         compiler/zo-structs
         zordoz/private/zo-string
         zordoz/private/zo-transition
         zordoz/private/zo-find
         zordoz/private/zo-shell
         zordoz/private/compiler-goodies)

(provide result result? result-zo result-path)
(provide (contract-out
          ;; zo-string
          [zo->string (->* (zo?) (#:deep? boolean?) string?)]
          [zo->spec (->i ([z zo?]) () [res (z) (and/c spec/c (specof z))])]
          ;; zo-transition
          [zo-transition (-> zo? string? (values (or/c zo? (listof zo?)) boolean?))]
          ;; zo-find
          [zo-find (-> zo? string? (listof result?))]
          ;; zo-shell
          [zo->shell (-> zo? void?)]
          [syntax->zo (-> syntax? zo?)]
          [syntax->decompile (-> syntax? any/c)]
          [zo->compiled-expression (-> zo? compiled-expression?)]))
