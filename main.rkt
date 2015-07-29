#lang racket/base

(require racket/contract
         compiler/zo-structs)

(require "private/zo-string.rkt"
         "private/zo-transition.rkt"
         "private/zo-find.rkt"
         "private/zo-shell.rkt")

(provide result result? result-zo result-path)
(provide (contract-out
          ;; zo-string
          [zo->string (->* (zo?) (#:deep? boolean?) string?)]
          [zo->spec (->i ([z zo?]) () [res (z) (and/c spec/c (specof z))])]
          ;; zo-transition
          [zo-transition (-> zo? string? (values (or/c zo? (listof zo?)) boolean?))]
          ;; zo-find
          [zo-find (-> zo? string? (listof result?))]))
