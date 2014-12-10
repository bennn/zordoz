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
  ;; (-> prefix? string? (or/c zo? #f))
  (cond [(string=? field-name "stxs")      (begin (displayln "Cannot dive into list YET") #f)]
        [(string=? field-name "toplevels") (begin (displayln "Cannot dive into list YET") #f)]
        [else #f]))

(define (global-bucket-> z field-name)
  ;; (-> global-bucket? string? (or/c zo? #f))
  #f)

(define (module-variable-> z field-name)
  ;; (-> module-variable? string? (or/c zo? #f))
  #f)

(define (stx-> z field-name)
  ;; (-> stx? string? (or/c zo? #f))
  (cond [(string=? field-name "encoded") (stx-encoded z)]
        [else #f]))

(define (form-> z field-name)
  ;; (-> form? string? (or/c zo? #f))
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
  ;; (-> expr? string? (or/c zo? #f))
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
  ;; (-> wrapped? string? (or/c zo? #f))
  (cond [(string=? field-name "wraps") (begin (displayln "Cannot dive into list YET") #f)]
        [else #f]))

(define (wrap-> z field-name)
  ;; (-> wrap? string? (or/c zo? #f))
  (cond [(top-level-rename? z) (top-level-rename-> z field-name)]
        [(mark-barrier?     z) (mark-barrier->     z field-name)]
        [(lexical-rename?   z) (lexical-rename->   z field-name)]
        [(phase-shift?      z) (phase-shift->      z field-name)]
        [(module-rename?    z) (module-rename->    z field-name)]
        [else #f]))

(define (free-id-info-> z field-name)
  ;; (-> free-id-info? string? (or/c zo? #f))
  #f)

(define (all-from-module-> z field-name)
  ;; (-> all-from-module? string? (or/c zo? #f))
  #f)

(define (module-binding-> z field-name)
  ;; (-> module-binding? string? (or/c zo? #f))
  (cond [(simple-module-binding?           z) (simple-module-binding->           z field-name)]
        [(phased-module-binding?           z) (phased-module-binding->           z field-name)]
        [(exported-nominal-module-binding? z) (exported-nominal-module-binding-> z field-name)]
        [(nominal-module-binding?          z) (nominal-module-binding->          z field-name)]
        [(exported-module-binding?         z) (exported-module-binding->         z field-name)]
        [else #f]))

(define (nominal-path-> z field-name)
  ;; (-> nominal-path? string? (or/c zo? #f))
  (cond [(simple-nominal-path?   z) (simple-nominal-path->   z field-name)]
        [(imported-nominal-path? z) (imported-nominal-path-> z field-name)]
        [(phased-nominal-path?   z) (phased-nominal-path->   z field-name)]
        [else #f]))

;; -- form

(define (def-values-> z field-name)
  ;; (-> def-values? string? (or/c zo? #f))
  (cond [(string=? field-name "ids") (begin (displayln "Cannot dive into list YET") #f)]
        [(string=? field-name "rhs") (let ([rhs (def-values-rhs z)])
                                       (cond [(or (expr? rhs)
                                                  (seq? rhs)
                                                  (inline-variant? rhs)) rhs]
                                             [else #f]))]
        [else #f]))

(define (def-syntaxes-> z field-name)
  ;; (-> def-syntaxes? string? (or/c zo? #f))
  (cond [(string=? field-name "rhs")    (let ([rhs (def-syntaxes-rhs z)])
                                          (cond [(or (expr? rhs)
                                                     (seq?  rhs)) (def-syntaxes-rhs z)]
                                                [else #f]))]
        [(string=? field-name "prefix") (def-syntaxes-prefix z)]
        [(string=? field-name "dummy")  (let ([dm (def-syntaxes-dummy z)])
                                          (cond [(toplevel? dm) dm]
                                                [else #f]))]
        [else #f]))

(define (seq-for-syntax-> z field-name)
  ;; (-> seq-for-syntax? string? (or/c zo? #f))
  (cond [(string=? field-name "forms")  (begin (displayln "Cannot dive into list YET") #f)]
        [(string=? field-name "prefix") (seq-for-syntax-prefix z)]
        [(string=? field-name "dummy")  (let ([dm (seq-for-syntax-dummy z)])
                                          (cond [(toplevel? dm) dm]
                                                [else #f]))]
        [else #f]))

(define (req-> z field-name)
  ;; (-> req? string? (or/c zo? #f))
  (cond [(string=? field-name "reqs")  (req-reqs z)]
        [(string=? field-name "dummy") (req-dummy z)]
        [else #f]))

(define (seq-> z field-name)
  ;; (-> seq? string? (or/c zo? #f))
  (cond [(string=? field-name "forms") (begin (displayln "Cannot dive into list YET") #f)]
        [else #f]))

(define (splice-> z field-name)
  ;; (-> splice? string? (or/c zo? #f))
  (cond [(string=? field-name "forms") (begin (displayln "Cannot dive into list YET") #f)]
        [else #f]))

(define (inline-variant-> z field-name)
  ;; (-> inline-variant? string? (or/c zo? #f))
  (cond [(string=? field-name "direct") (inline-variant-direct z)]
        [(string=? field-name "inline") (inline-variant-inline z)]
        [else #f]))

(define (mod-> z field-name)
  ;; (-> mod? string? (or/c zo? #f))
  (cond [(string=? field-name "prefix")           (mod-prefix z)]
        [(string=? field-name "provides")         (begin (displayln "Cannot dive into list YET") #f)]
        [(string=? field-name "requires")         (begin (displayln "Cannot dive into list YET") #f)]
        [(string=? field-name "body")             (begin (displayln "Cannot dive into list YET") #f)]
        [(string=? field-name "syntax-bodies")    (begin (displayln "Cannot dive into list YET") #f)]
        [(string=? field-name "dummy")            (mod-dummy z)]
        [(string=? field-name "internal-context") (let ([ic (mod-internal-context z)])
                                                    (cond [(stx?    ic) ic]
                                                          [(vector? ic) (begin (displayln "Cannot dive into list YET") #f)]
                                                          [else #f]))]
        [(string=? field-name "pre-submodules")   (begin (displayln "Cannot dive into list YET") #f)]
        [(string=? field-name "post-submodules")  (begin (displayln "Cannot dive into list YET") #f)]
        [else #f]))

(define (provided-> z field-name)
  ;; (-> provided? string? (or/c zo? #f))
  #f)

;; -- expr
        
(define (lam-> z field-name)
  ;; (-> lam? string? (or/c zo? #f))
  (cond [(string=? field-name "body") (let ([bd (lam-body z)])
                                        (cond [(expr? bd) bd]
                                              [(seq?  bd) bd]
                                              [else #f]))]
        [else #f]))

(define (closure-> z field-name)
  ;; (-> closure? string? (or/c zo? #f))
  (cond [(string=? field-name "code") (closure-code z)]
        [else #f]))

(define (case-lam-> z field-name)
  ;; (-> case-lam? string? (or/c zo? #f))
  (cond [(string=? field-name "clauses") (begin (displayln "Cannot dive into list YET") #f)]
        [else #f]))

(define (let-one-> z field-name)
  ;; (-> let-one? string? (or/c zo? #f))
  (cond [(string=? field-name "rhs")  (let ([rhs  (let-one-rhs z)])
                                        (cond [(expr? rhs)  rhs]
                                              [(seq?  rhs)  rhs]
                                              [else #f]))]
        [(string=? field-name "body") (let ([body (let-one-body z)])
                                        (cond [(expr? body) body]
                                              [(seq?  body) body]
                                              [else #f]))]
        [else #f]))

(define (let-void-> z field-name)
  ;; (-> let-void? string? (or/c zo? #f))
  (cond [(string=? field-name "body") (let ([body (let-one-body z)])
                                        (cond [(expr? body) body]
                                              [(seq?  body) body]
                                              [else #f]))]
        [else #f]))

(define (install-value-> z field-name)
  ;; (-> install-value? string? (or/c zo? #f))
  (cond [(string=? field-name "rhs")  (let ([rhs  (install-value-rhs z)])
                                        (cond [(expr? rhs)  rhs]
                                              [(seq?  rhs)  rhs]
                                              [else #f]))]
        [(string=? field-name "body") (let ([body (install-value-body z)])
                                        (cond [(expr? body) body]
                                              [(seq?  body) body]
                                              [else #f]))]
        [else #f]))

(define (let-rec-> z field-name)
  ;; (-> let-rec? string? (or/c zo? #f))
  (cond [(string=? field-name "procs") (begin (displayln  "Cannot dive into list YET") #f)]
        [(string=? field-name "body")  (let ([body (let-rec-body z)])
                                         (cond [(expr? body) body]
                                               [(seq?  body) body]
                                               [else #f]))]
        [else #f]))

(define (boxenv-> z field-name)
  ;; (-> boxenv? string? (or/c zo? #f))
  (cond [(string=? field-name "body") (let ([body (boxenv-body z)])
                                        (cond [(expr? body) body]
                                              [(seq? body)  body]
                                              [else #f]))]
        [else #f]))

(define (localref-> z field-name)
  ;; (-> localref? string? (or/c zo? #f))
  #f)

(define (toplevel-> z field-name)
  ;; (-> toplevel? string? (or/c zo? #f))
  #f)

(define (topsyntax-> z field-name)
  ;; (-> topsyntax? string? (or/c zo? #f))
  #f)

(define (application-> z field-name)
  ;; (-> application? string? (or/c zo? #f))
  (cond [(string=? field-name "rator") (let ([rator (application-rator z)])
                                         (cond [(expr? rator) rator]
                                               [(seq?  rator) rator]
                                               [else #f]))]
        [(string=? field-name "rands") (begin (displayln  "Cannot dive into list YET") #f)]
        [else #f]))

(define (branch-> z field-name)
  ;; (-> branch? string? (or/c zo? #f))
  (cond [(string=? field-name "test") (let ([test (branch-test z)])
                                         (cond [(expr? test) test]
                                               [(seq?  test) test]))]
        [(string=? field-name "then") (let ([then (branch-then z)])
                                        (cond [(expr? then) then]
                                              [(seq?  then) then]))]
        [(string=? field-name "else") (let ([else (branch-else z)])
                                        (cond [(expr? else) else]
                                              [(seq?  else) else]))]
        [else #f]))

(define (with-cont-mark-> z field-name)
  ;; (-> with-cont-mark? string? (or/c zo? #f))
  (cond [(string=? field-name "key")  (let ([key  (with-cont-mark-key z)])
                                        (cond [(expr? key)  key]
                                              [(seq?  key)  key]))]
        [(string=? field-name "val")  (let ([val  (with-cont-mark-val z)])
                                        (cond [(expr? val)  val]
                                              [(seq?  val)  val]))]
        [(string=? field-name "body") (let ([body (with-cont-mark-body z)])
                                        (cond [(expr? body) body]
                                              [(seq?  body) body]))]
        [else #f]))

(define (beg0-> z field-name)
  ;; (-> beg0? string? (or/c zo? #f))
  (cond [(string=? field-name "seq") (begin (displayln  "Cannot dive into list YET") #f)]
        [else #f]))

(define (varref-> z field-name)
  ;; (-> varref? string? (or/c zo? #f))
  (cond [(string=? field-name "toplevel") (let ([tl (varref-toplevel z)])
                                            (cond [(toplevel? tl) tl]
                                                  [else #f]))]
        [(string=? field-name "dummy")    (let ([dm (varref-dummy z)])
                                            (cond [(toplevel? dm) dm]
                                                  [else #f]))]
        [else #f]))

(define (assign-> z field-name)
  ;; (-> assign? string? (or/c zo? #f))
  (cond [(string=? field-name "id")  (assign-id z)]
        [(string=? field-name "rhs") (let ([rhs (assign-rhs z)])
                                       (cond [(expr? rhs) rhs]
                                             [(seq?  rhs) rhs]
                                             [else #f]))]
        [else #f]))

(define (apply-values-> z field-name)
  ;; (-> apply-values? string? (or/c zo? #f))
  (cond [(string=? field-name "proc")      (let ([proc      (apply-values-proc z)])
                                             (cond [(expr? proc)      proc]
                                                   [(seq?  proc)      proc]
                                                   [else #f]))]
        [(string=? field-name "args-expr") (let ([args-expr (apply-values-args-expr z)])
                                             (cond [(expr? args-expr) args-expr]
                                                   [(seq?  args-expr) args-expr]
                                                   [else #f]))]
        [else #f]))

(define (primval-> z field-name)
  ;; (-> primval? string? (or/c zo? #f))
  #f)

;; -- wrap

(define (top-level-rename-> z field-name)
  ;; (-> top-level-rename? string? (or/c zo? #f))
  #f)

(define (mark-barrier-> z field-name)
  ;; (-> mark-barrier? string? (or/c zo? #f))
  #f)

(define (lexical-rename-> z field-name)
  ;; (-> lexical-rename? string? (or/c zo? #f))
  (cond [(string=? field-name "alist") (begin (displayln "Cannot dive into list YET") #f)]
        [else #f]))

(define (phase-shift-> z field-name)
  ;; (-> phase-shift? string? (or/c zo? #f))
  #f)

;; 2014-12-10: Possibly dive into 'unmarshals'
(define (module-rename-> z field-name)
  ;; (-> module-rename? string? (or/c zo? #f))
  #f)

;; -- module-binding

(define (simple-module-binding-> z field-name)
  ;; (-> simple-module-binding? string? (or/c zo? #f))
  #f)

(define (phased-module-binding-> z field-name)
  ;; (-> phased-module-binding? string? (or/c zo? #f))
  (cond [(string=? field-name "nominal-path") (phased-module-binding-nominal-path z)]
        [else #f]))

(define (exported-nominal-module-binding-> z field-name)
  ;; (-> exported-nominal-module-binding? string? (or/c zo? #f))
  (cond [(string=? field-name "nominal-path") (exported-nominal-module-binding-nominal-path z)]
        [else #f]))

(define (nominal-module-binding-> z field-name)
  ;; (-> nominal-module-binding? string? (or/c zo? #f))
  (cond [(string=? field-name "nominal-path") (nominal-module-binding-nominal-path z)]
        [else #f]))

(define (exported-module-binding-> z field-name)
  ;; (-> exported-module-binding? string? (or/c zo? #f))
  #f)

;; -- nominal-path

(define (simple-nominal-path-> z field-name)
  ;; (-> simple-nominal-path? string? (or/c zo? #f))
  #f)

(define (imported-nominal-path-> z field-name)
  ;; (-> imported-nominal-path? string? (or/c zo? #f))
  #f)

(define (phased-nominal-path-> z field-name)
  ;; (-> phased-nominal-path? string? (or/c zo? #f))
  #f)
