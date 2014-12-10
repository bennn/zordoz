#lang racket/base

(provide transition)

(require compiler/zo-structs)
         ;; (only-in racket/string string-join))

;; -- API functions

(define (transition z field-name)
  ;; (-> zo? string? (values zo? boolean))
  (define nxt ;; Try to get next struct
    (cond [(compilation-top? z) (compilation-top-> z field-name)]
          [(prefix?          z) (prefix->          z field-name)]
          [(global-bucket?   z) (global-bucket->   z field-name)]
          [(module-variable? z) (module-variable-> z field-name)]
          [(stx?             z) (stx->             z field-name)]
          [(form?            z) (form->            z field-name)]
          [(expr?            z) (expr->            z field-name)]
          [(wrapped?         z) (wrapped->         z field-name)]
          [(wrap?            z) (wrap->            z field-name)]
          [(free-id-info?    z) (free-id-info->    z field-name)]
          [(all-from-module? z) (all-from-module-> z field-name)]
          [(module-binding?  z) (module-binding->  z field-name)]
          [(nominal-path?    z) (nominal-path->    z field-name)]
          [else (error (format "[transition] Unknown struct '~a'" z))]))
  ;; Check if transition failed, pack result values
  (if (zo? nxt) (values nxt #t) (values z #f)))

;; -- private getters

(define (compilation-top-> z field-name)
  ;; (-> compilation-top? string? (or/c zo? #f))
  (cond [(string=? field-name "prefix")        (compilation-top-prefix z)]
        [(string=? field-name "code")          (compilation-top-code   z)]
        [else #f]))

(define (prefix-> z field-name)
  ;; (-> prefix? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (global-bucket-> z field-name)
  ;; (-> global-bucket? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (module-variable-> z field-name)
  ;; (-> module-variable? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (stx-> z field-name)
  ;; (-> stx? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (form-> z field-name)
  ;; (-> form? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (expr-> z field-name)
  ;; (-> expr? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (wrapped-> z field-name)
  ;; (-> wrapped? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (wrap-> z field-name)
  ;; (-> wrap? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (free-id-info-> z field-name)
  ;; (-> free-id-info? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (all-from-module-> z field-name)
  ;; (-> all-from-module? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (module-binding-> z field-name)
  ;; (-> module-binding? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (nominal-path-> z field-name)
  ;; (-> nominal-path? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
