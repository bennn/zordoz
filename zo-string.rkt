#lang racket/base

(provide zo->string)

(require compiler/zo-structs)

;; -- API functions

(define (zo->string z)
  ;; (-> zo? string?)
  (cond [(compilation-top? z) (compilation-top->string  z)]
        [(prefix?          z) (prefix->string           z)]
        [(global-bucket?   z) (global-bucket->string    z)]
        [(module-variable? z) (module-variable->string  z)]
        [(stx?             z) (stx->string              z)]
        [(form?            z) (form->string             z)]
        [(expr?            z) (expr->string             z)]
        [(wrapped?         z) (wrapped->string          z)]
        [(wrap?            z) (wrap->string             z)]
        [(free-id-info?    z) (free-id-info->string     z)]
        [(all-from-module? z) (all-from-module->string  z)]
        [(module-binding?  z) (module-binding->string   z)]
        [(nominal-path?    z) (nominal-path->string     z)]
        [else (error (format "[zo->string] Unknown struct '~a'" z))]))

;; -- private functions

(define (compilation-top->string z)
  ;; (-> compilation-top? string?)
  (error "[compilation-top->string] not implemented"))

(define (prefix->string z)
  ;; (-> prefix? string?)
  (error "[prefix->string] not implemented"))

(define (global-bucket->string z)
  ;; (->  global-bucket? string?)
  (error "[global-bucket->string] not implemented"))

(define (module-variable->string z)
  ;; (-> module-variable? string?)
  (error "[module-variable->string] not implemented"))

(define (stx->string z)
  ;; (-> stx? string?)
  (error "[stx->string] not implemented"))

(define (form->string z)
  ;; (-> form? string?)
  (error "[form->string] not implemented"))

(define (expr->string z)
  ;; (-> expr? string?)
  (error "[expr->string] not implemented"))

(define (wrapped->string z)
  ;; (-> wrapped? string?)
  (error "[wrapped->string] not implemented"))

(define (wrap->string z)
  ;; (-> wrap? string?)
  (error "[wrap->string] not implemented"))

(define (free-id-info->string z)
  ;; (-> free-id-info? string?)
  (error "[free-id-info->string] not implemented"))

(define (all-from-module->string z)
  ;; (-> all-from-module? string?)
  (error "[all-from-module->string] not implemented"))

(define (module-binding->string z)
  ;; (-> module-binding? string?)
  (error "[module-binding->string] not implemented"))

(define (nominal-path->string z)
  ;; (-> nominal-path? string?)
  (error "[nominal-path->string] not implemented"))
