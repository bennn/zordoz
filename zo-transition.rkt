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
  (cond [(string=? field-name "prefix") (compilation-top-prefix z)]
        [(string=? field-name "code")   (compilation-top-code   z)]
        [else #f]))

(define (prefix-> z field-name)
  ;; (-> prefix? zo? string? (or/c zo? #f))
  (cond [(string=? field-name "stxs")      (begin (displayln "Cannot dive into list YET") #f)]
        [(string=? field-name "toplevels") (begin (displayln "Cannot dive into list YET") #f)]
        [else #f]))

(define (global-bucket-> z field-name)
  ;; (-> global-bucket? zo? string? (or/c zo? #f))
  #f)

(define (module-variable-> z field-name)
  ;; (-> module-variable? zo? string? (or/c zo? #f))
  #f)

(define (stx-> z field-name)
  ;; (-> stx? zo? string? (or/c zo? #f))
  (cond [(string=? field-name "encoded") (stx-encoded z)]
        [else #f]))

(define (form-> z field-name)
  ;; (-> form? zo? string? (or/c zo? #f))
  (cond [(def-values?     z) (def-values->     z field-name)]
        [(def-syntaxes?   z) (def-syntaxes->   z field-name)]
        [(seq-for-syntax? z) (seq-for-syntax-> z field-name)]
        [(req?            z) (req->            z field-name)]
        [(seq?            z) (seq->            z field-name)]
        [(splice?         z) (splice->         z field-name)]
        [(inline-variant? z) (inline-variant-> z field-name)]
        [(mod?            z) (mod->            z field-name)]
        [(provided?       z) (provided->       z field-name)]
        [(expr?           z) (expr->           z field-name)]
        [else #f]))

(define (expr-> z field-name)
  ;; (-> expr? zo? string? (or/c zo? #f))
  (cond [(lam?            z) (lam->            z field-name)]
        [(closure?        z) (closure->        z field-name)]
        [(case-lam?       z) (case-lam->       z field-name)]
        [(let-one?        z) (let-one->        z field-name)]
        [(let-void?       z) (let-void->       z field-name)]
        [(install-value?  z) (install-value->  z field-name)]
        [(let-rec?        z) (let-rec->        z field-name)]
        [(boxenv?         z) (boxenv->         z field-name)]
        [(localref?       z) (localref->       z field-name)]
        [(toplevel?       z) (toplevel->       z field-name)]
        [(topsyntax?      z) (topsyntax->      z field-name)]
        [(application?    z) (application->    z field-name)]
        [(branch?         z) (branch->         z field-name)]
        [(with-cont-mark? z) (with-cont-mark-> z field-name)]
        [(beg0?           z) (beg0->           z field-name)]
        [(varref?         z) (varref->         z field-name)]
        [(assign?         z) (assign->         z field-name)]
        [(apply-values?   z) (apply-values->   z field-name)]
        [(primval?        z) (primval->        z field-name)]
        [else #f]))

(define (wrapped-> z field-name)
  ;; (-> wrapped? zo? string? (or/c zo? #f))
  (cond [(string=? field-name "wraps") (begin (displayln "Cannot dive into list YET") #f)]
        [else #f]))

(define (wrap-> z field-name)
  ;; (-> wrap? zo? string? (or/c zo? #f))
  (cond [(top-level-rename? z) (top-level-rename-> z field-name)]
        [(mark-barrier?     z) (mark-barrier->     z field-name)]
        [(lexical-rename?   z) (lexical-rename->   z field-name)]
        [(phase-shift?      z) (phase-shift->      z field-name)]
        [(module-rename?    z) (module-rename->    z field-name)]
        [else #f]))

(define (free-id-info-> z field-name)
  ;; (-> free-id-info? zo? string? (or/c zo? #f))
  #f)

(define (all-from-module-> z field-name)
  ;; (-> all-from-module? zo? string? (or/c zo? #f))
  #f)

(define (module-binding-> z field-name)
  ;; (-> module-binding? zo? string? (or/c zo? #f))
  (cond [(simple-module-binding?           z) (simple-module-binding->           z field-name)]
        [(phased-module-binding?           z) (phased-module-binding->           z field-name)]
        [(exported-nominal-module-binding? z) (exported-nominal-module-binding-> z field-name)]
        [(nominal-module-binding?          z) (nominal-module-binding->          z field-name)]
        [(exported-module-binding?         z) (exported-module-binding->         z field-name)]
        [else #f]))

(define (nominal-path-> z field-name)
  ;; (-> nominal-path? zo? string? (or/c zo? #f))
  (cond [(simple-nominal-path?   z) (simple-nominal-path->   z field-name)]
        [(imported-nominal-path? z) (imported-nominal-path-> z field-name)]
        [(phased-nominal-path?   z) (phased-nominal-path->   z field-name)]
        [else #f]))

;; -- form

(define (def-values-> z field-name)
  ;; (-> def-values? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (def-syntaxes-> z field-name)
  ;; (-> def-syntaxes? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (seq-for-syntax-> z field-name)
  ;; (-> seq-for-syntax? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (req-> z field-name)
  ;; (-> req? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (seq-> z field-name)
  ;; (-> seq? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (splice-> z field-name)
  ;; (-> splice? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (inline-variant-> z field-name)
  ;; (-> inline-variant? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (mod-> z field-name)
  ;; (-> mod? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (provided-> z field-name)
  ;; (-> provided? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )

;; -- expr
        
(define (lam-> z field-name)
  ;; (-> lam? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (closure-> z field-name)
  ;; (-> closure? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (case-lam-> z field-name)
  ;; (-> case-lam? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (let-one-> z field-name)
  ;; (-> let-one? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (let-void-> z field-name)
  ;; (-> let-void? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (install-value-> z field-name)
  ;; (-> install-value? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (let-rec-> z field-name)
  ;; (-> (define (let-rec? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (boxenv-> z field-name)
  ;; (-> boxenv? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (localref-> z field-name)
  ;; (-> localref? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (toplevel-> z field-name)
  ;; (-> toplevel? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (topsyntax-> z field-name)
  ;; (-> topsyntax? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (application-> z field-name)
  ;; (-> application? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (branch-> z field-name)
  ;; (-> branch? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (with-cont-mark-> z field-name)
  ;; (-> with-cont-mark? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (beg0-> z field-name)
  ;; (-> beg0? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (varref-> z field-name)
  ;; (-> varref? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (assign-> z field-name)
  ;; (-> assign? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (apply-values-> z field-name)
  ;; (-> apply-values? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (primval-> z field-name)
  ;; (-> primval? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )

;; -- wrap

(define (top-level-rename-> z field-name)
  ;; (-> top-level-rename? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (mark-barrier-> z field-name)
  ;; (-> mark-barrier? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (lexical-rename-> z field-name)
  ;; (-> lexical-rename? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (phase-shift-> z field-name)
  ;; (-> phase-shift? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (module-rename-> z field-name)
  ;; (-> module-rename? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )

;; -- module-binding

(define (simple-module-binding-> z field-name)
  ;; (-> simple-module-binding? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (phased-module-binding-> z field-name)
  ;; (-> phased-module-binding? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (exported-nominal-module-binding-> z field-name)
  ;; (-> exported-nominal-module-binding? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (nominal-module-binding-> z field-name)
  ;; (-> nominal-module-binding? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (exported-module-binding-> z field-name)
  ;; (-> exported-module-binding? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )

;; -- nominal-path

(define (simple-nominal-path-> z field-name)
  ;; (-> simple-nominal-path? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (imported-nominal-path-> z field-name)
  ;; (-> imported-nominal-path? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )
(define (phased-nominal-path-> z field-name)
  ;; (-> phased-nominal-path? zo? string? (or/c zo? #f))
  (error "Not implemented")
  )

