#lang racket/base

;; Access the fields of a struct by name at runtime.

(provide
 ;; Access "structName-fieldName myStruct" at runtime.
 transition)

(require compiler/zo-structs
         (only-in racket/list empty? empty))

;; -----------------------------------------------------------------------------

;; -- API functions

(define (transition z field-name)
  ;; (-> zo? string? (values (or/c zo? (listof zo?)) boolean))
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
          [(provided?        z) (provided->        z field-name)]
          [else (error (format "[transition] Unknown struct '~a'" z))]))
  ;; Check if transition failed, pack result values
  (if (or (zo? nxt) (list? nxt)) (values nxt #t) (values z #f)))

;; -- private getters

(define (compilation-top-> z field-name)
  ;; (-> compilation-top? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "prefix") (compilation-top-prefix z)]
        [(string=? field-name "code")   (compilation-top-code   z)]
        [else #f]))

(define (prefix-> z field-name)
  ;; (-> prefix? string? (or/c (listof zo?) zo? #f))
  (define (gb-or-mv? tl)
    (or (global-bucket? tl) (module-variable? tl)))
  (cond [(string=? field-name "toplevels") (filter gb-or-mv? (prefix-toplevels z))]
        [(string=? field-name "stxs")      (prefix-stxs z)]
        [else #f]))

(define (global-bucket-> z field-name)
  ;; (-> global-bucket? string? (or/c (listof zo?) zo? #f))
  #f)

(define (module-variable-> z field-name)
  ;; (-> module-variable? string? (or/c (listof zo?) zo? #f))
  #f)

(define (stx-> z field-name)
  ;; (-> stx? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "encoded") (stx-encoded z)]
        [else #f]))

(define (form-> z field-name)
  ;; (-> form? string? (or/c (listof zo?) zo? #f))
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
  ;; (-> expr? string? (or/c (listof zo?) zo? #f))
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
  ;; (-> wrapped? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "wraps") (wrapped-wraps z)]
        [else #f]))

(define (wrap-> z field-name)
  ;; (-> wrap? string? (or/c (listof zo?) zo? #f))
  (cond [(top-level-rename? z) (top-level-rename-> z field-name)]
        [(mark-barrier?     z) (mark-barrier->     z field-name)]
        [(lexical-rename?   z) (lexical-rename->   z field-name)]
        [(phase-shift?      z) (phase-shift->      z field-name)]
        [(module-rename?    z) (module-rename->    z field-name)]
        [(wrap-mark?        z) (wrap-mark->        z field-name)]
        [(prune?            z) (prune->            z field-name)]
        [else #f]))

(define (free-id-info-> z field-name)
  ;; (-> free-id-info? string? (or/c (listof zo?) zo? #f))
  #f)

(define (all-from-module-> z field-name)
  ;; (-> all-from-module? string? (or/c (listof zo?) zo? #f))
  #f)

(define (module-binding-> z field-name)
  ;; (-> module-binding? string? (or/c (listof zo?) zo? #f))
  (cond [(simple-module-binding?           z) (simple-module-binding->           z field-name)]
        [(phased-module-binding?           z) (phased-module-binding->           z field-name)]
        [(exported-nominal-module-binding? z) (exported-nominal-module-binding-> z field-name)]
        [(nominal-module-binding?          z) (nominal-module-binding->          z field-name)]
        [(exported-module-binding?         z) (exported-module-binding->         z field-name)]
        [else #f]))

(define (nominal-path-> z field-name)
  ;; (-> nominal-path? string? (or/c (listof zo?) zo? #f))
  (cond [(simple-nominal-path?   z) (simple-nominal-path->   z field-name)]
        [(imported-nominal-path? z) (imported-nominal-path-> z field-name)]
        [(phased-nominal-path?   z) (phased-nominal-path->   z field-name)]
        [else #f]))

;; -- form

(define (def-values-> z field-name)
  ;; (-> def-values? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "ids") (def-values-ids z)]
        [(string=? field-name "rhs") (define rhs (def-values-rhs z))
                                     (cond [(or (expr?           rhs)
                                                (seq?            rhs)
                                                (inline-variant? rhs)) rhs]
                                           [else #f])]
        [else #f]))

(define (def-syntaxes-> z field-name)
  ;; (-> def-syntaxes? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "rhs")    (define rhs (def-syntaxes-rhs z))
                                        (cond [(or (expr? rhs)
                                                   (seq?  rhs)) (def-syntaxes-rhs z)]
                                              [else #f])]
        [(string=? field-name "prefix") (def-syntaxes-prefix z)]
        [(string=? field-name "dummy")  (define dm (def-syntaxes-dummy z))
                                        (cond [(toplevel? dm) dm]
                                              [else #f])]
        [else #f]))

(define (seq-for-syntax-> z field-name)
  ;; (-> seq-for-syntax? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "forms")  (filter form? (seq-for-syntax-forms z))]
        [(string=? field-name "prefix") (seq-for-syntax-prefix z)]
        [(string=? field-name "dummy")  (define dm (seq-for-syntax-dummy z))
                                        (cond [(toplevel? dm) dm]
                                              [else #f])]
        [else #f]))

(define (req-> z field-name)
  ;; (-> req? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "reqs")  (req-reqs z)]
        [(string=? field-name "dummy") (req-dummy z)]
        [else #f]))

(define (seq-> z field-name)
  ;; (-> seq? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "forms") (filter form? (seq-forms z))]
        [else #f]))

(define (splice-> z field-name)
  ;; (-> splice? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "forms") (filter form? (splice-forms z))]
        [else #f]))

(define (inline-variant-> z field-name)
  ;; (-> inline-variant? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "direct") (inline-variant-direct z)]
        [(string=? field-name "inline") (inline-variant-inline z)]
        [else #f]))

(define (mod-> z field-name)
  ;; (-> mod? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "prefix")           (mod-prefix z)]
        [(string=? field-name "provides")         (get-provided (mod-provides z))]
        [(string=? field-name "body")             (filter form? (mod-body z))]
        [(string=? field-name "syntax-bodies")    (get-syntaxes (mod-syntax-bodies z))]
        [(string=? field-name "dummy")            (mod-dummy z)]
        [(string=? field-name "internal-context") (define ic (mod-internal-context z))
                                                  (cond [(stx?    ic) ic]
                                                        [(vector? ic) (vector->list ic)]
                                                        [else #f])]
        [(string=? field-name "pre-submodules")   (mod-pre-submodules z)]
        [(string=? field-name "post-submodules")  (mod-post-submodules z)]
        [else #f]))

(define (get-provided pds)
  ;; (-> (listof (list/c (or/c exact-integer? #f) (listof provided?) (listof provided?))) (listof provided?))
  (cond [(empty? pds) empty]
        [else (append (cadar pds)
                      (caddar pds)
                      (get-provided (cdr pds)))]))

(define (get-syntaxes sxs)
  ;; (-> (listof (cons/c exact-positive-integer? (listof (or/c def-syntaxes? seq-for-syntax?)))) (listof (or/c def-syntaxes? seq-for-syntax?)))
  (cond [(empty? sxs) empty]
        [else (append (cdar sxs)
                      (get-syntaxes (cdr sxs)))]))

(define (provided-> z field-name)
  ;; (-> provided? string? (or/c (listof zo?) zo? #f))
  #f)

;; -- expr

(define (lam-> z field-name)
  ;; (-> lam? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "body") (define bd (lam-body z))
                                      (cond [(expr-or-seq? bd) bd]
                                            [else #f])]
        [else #f]))

(define (closure-> z field-name)
  ;; (-> closure? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "code") (closure-code z)]
        [else #f]))

(define (case-lam-> z field-name)
  ;; (-> case-lam? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "clauses") (case-lam-clauses z)]
        [else #f]))

(define (let-one-> z field-name)
  ;; (-> let-one? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "rhs")  (define rhs (let-one-rhs z))
                                      (cond [(expr-or-seq? rhs) rhs]
                                            [else #f])]
        [(string=? field-name "body") (define body (let-one-body z))
                                      (cond [(expr-or-seq? body) body]
                                            [else #f])]
        [else #f]))

(define (let-void-> z field-name)
  ;; (-> let-void? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "body") (define body (let-one-body z))
                                      (cond [(expr-or-seq? body) body]
                                            [else #f])]
        [else #f]))

(define (install-value-> z field-name)
  ;; (-> install-value? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "rhs")  (define rhs  (install-value-rhs z))
                                      (cond [(expr-or-seq? rhs)  rhs]
                                            [else #f])]
        [(string=? field-name "body") (define body (install-value-body z))
                                      (cond [(expr-or-seq? body) body]
                                            [else #f])]
        [else #f]))

(define (let-rec-> z field-name)
  ;; (-> let-rec? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "procs") (let-rec-procs z)]
        [(string=? field-name "body")  (define body (let-rec-body z))
                                       (cond [(expr-or-seq? body) body]
                                             [else #f])]
        [else #f]))

(define (boxenv-> z field-name)
  ;; (-> boxenv? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "body") (define body (boxenv-body z))
                                      (cond [(expr-or-seq? body) body]
                                            [else #f])]
        [else #f]))

(define (localref-> z field-name)
  ;; (-> localref? string? (or/c (listof zo?) zo? #f))
  #f)

(define (toplevel-> z field-name)
  ;; (-> toplevel? string? (or/c (listof zo?) zo? #f))
  #f)

(define (topsyntax-> z field-name)
  ;; (-> topsyntax? string? (or/c (listof zo?) zo? #f))
  #f)

(define (application-> z field-name)
  ;; (-> application? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "rator") (define rator (application-rator z))
                                       (cond [(expr-or-seq? rator) rator]
                                             [else #f])]
        [(string=? field-name "rands") (filter expr-or-seq? (application-rands z))]
        [else #f]))

(define (branch-> z field-name)
  ;; (-> branch? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "test") (define test (branch-test z))
                                       (cond [(expr-or-seq? test) test]
                                             [else #f])]
        [(string=? field-name "then") (define then (branch-then z))
                                      (cond [(expr-or-seq? then) then]
                                            [else #f])]
        [(string=? field-name "else") (define el (branch-else z))
                                      (cond [(expr-or-seq? el) el]
                                            [else #f])]
        [else #f]))

(define (with-cont-mark-> z field-name)
  ;; (-> with-cont-mark? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "key")  (define key  (with-cont-mark-key z))
                                      (cond [(expr-or-seq? key)  key]
                                            [else #f])]
        [(string=? field-name "val")  (define val  (with-cont-mark-val z))
                                      (cond [(expr-or-seq? val)  val]
                                            [else #f])]
        [(string=? field-name "body") (define body (with-cont-mark-body z))
                                      (cond [(expr-or-seq? body) body]
                                            [else #f])]
        [else #f]))

(define (beg0-> z field-name)
  ;; (-> beg0? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "seq") (filter expr-or-seq? (beg0-seq z))]
        [else #f]))

(define (varref-> z field-name)
  ;; (-> varref? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "toplevel") (define tl (varref-toplevel z))
                                          (cond [(toplevel? tl) tl]
                                                [else #f])]
        [(string=? field-name "dummy")    (define dm (varref-dummy z))
                                          (cond [(toplevel? dm) dm]
                                                [else #f])]
        [else #f]))

(define (assign-> z field-name)
  ;; (-> assign? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "id")  (assign-id z)]
        [(string=? field-name "rhs") (define rhs (assign-rhs z))
                                     (cond [(expr-or-seq? rhs) rhs]
                                           [else #f])]
        [else #f]))

(define (apply-values-> z field-name)
  ;; (-> apply-values? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "proc")      (define proc      (apply-values-proc z))
                                           (cond [(expr-or-seq? proc) proc]
                                                 [else #f])]
        [(string=? field-name "args-expr") (define args-expr (apply-values-args-expr z))
                                           (cond [(expr-or-seq? args-expr) args-expr]
                                                 [else #f])]
        [else #f]))

(define (primval-> z field-name)
  ;; (-> primval? string? (or/c (listof zo?) zo? #f))
  #f)

;; -- wrap

(define (top-level-rename-> z field-name)
  ;; (-> top-level-rename? string? (or/c (listof zo?) zo? #f))
  #f)

(define (mark-barrier-> z field-name)
  ;; (-> mark-barrier? string? (or/c (listof zo?) zo? #f))
  #f)

(define (lexical-rename-> z field-name)
  ;; (-> lexical-rename? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "alist") (get-free-id-info (lexical-rename-alist z))]
        [else #f]))

;; 2014-12-11: omfg
(define (get-free-id-info als)
  ;; (-> (listof (cons/c symbol? (or/c symbol? (cons/c symbol? (or/c (cons/c symbol? (or/c symbol? #f)) free-id-info?))))) (listof free-id-info?))
  (cond [(empty? als) empty]
        [else (append (if (and (pair?         (cdar als))
                               (free-id-info? (cddar als)))
                          (cddar als)
                          empty)
                      (lexical-rename-alist (cdr als)))]))
  
(define (phase-shift-> z field-name)
  ;; (-> phase-shift? string? (or/c (listof zo?) zo? #f))
  #f)

;; 2014-12-10: Possibly dive into 'unmarshals'
(define (module-rename-> z field-name)
  ;; (-> module-rename? string? (or/c (listof zo?) zo? #f))
  #f)

(define (wrap-mark-> z field-name)
  ;; (-> wrap-mark? string? (or/c (listof zo?) zo? #f))
  #f)

(define (prune-> z field-name)
  ;; (-> prune? string? (or/c (listof zo?) zo? #f))
  #f)

;; -- module-binding

(define (simple-module-binding-> z field-name)
  ;; (-> simple-module-binding? string? (or/c (listof zo?) zo? #f))
  #f)

(define (phased-module-binding-> z field-name)
  ;; (-> phased-module-binding? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "nominal-path") (phased-module-binding-nominal-path z)]
        [else #f]))

(define (exported-nominal-module-binding-> z field-name)
  ;; (-> exported-nominal-module-binding? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "nominal-path") (exported-nominal-module-binding-nominal-path z)]
        [else #f]))

(define (nominal-module-binding-> z field-name)
  ;; (-> nominal-module-binding? string? (or/c (listof zo?) zo? #f))
  (cond [(string=? field-name "nominal-path") (nominal-module-binding-nominal-path z)]
        [else #f]))

(define (exported-module-binding-> z field-name)
  ;; (-> exported-module-binding? string? (or/c (listof zo?) zo? #f))
  #f)

;; -- nominal-path

(define (simple-nominal-path-> z field-name)
  ;; (-> simple-nominal-path? string? (or/c (listof zo?) zo? #f))
  #f)

(define (imported-nominal-path-> z field-name)
  ;; (-> imported-nominal-path? string? (or/c (listof zo?) zo? #f))
  #f)

(define (phased-nominal-path-> z field-name)
  ;; (-> phased-nominal-path? string? (or/c (listof zo?) zo? #f))
  #f)

;; -- helpers

(define (expr-or-seq? x)
  ;; (-> any/c boolean?)
  (or (expr? x) (seq? x)))

;; -- testing

(module+ test
  (require rackunit)
  (check-equal? #t #t)
)
