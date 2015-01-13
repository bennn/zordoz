#lang racket/base

;; Convert a zo struct to a more readable string representation.
;; Documentation for zo structs is online here: http://docs.racket-lang.org/raco/decompile.html

(provide
 ;; Return a string representation of a zo struct
 zo->string
 ;; Return a list-of-strings representation of a zo struct.
 ;; The structure of the list mirrors the structure of the original zo struct.
 zo->spec
 ;; Contract for conversion functions.
 spec/c)

;; -- string specifications

;; Contract for conversion functions.
;; A spec/c is the name of a zo struct and a list of pairs representing its fields:
;; - The car of each field is the name of that field
;; - The cdr of each field is a thunk for building a representation of the field's value.
;;   If the value is a zo-struct, the thunk should build a spec/c
;;   Otherwise, the thunk should build a string
(define spec/c
  (recursive-contract
   (cons/c string? (listof (cons/c string? (-> (or/c spec/c string?)))))))

;; Given a zo struct [z], creates a predicate that accepts only specs with the
;; same number of elements as the struct [z] has fields (+1, for the title).
(define ((specof z) res)
  (= (length res) (vector-length (struct->vector z))))

(require compiler/zo-structs
         racket/contract
         (only-in racket/list   empty?)
         (only-in racket/string string-join)
         (for-syntax racket/base racket/syntax))

;; -----------------------------------------------------------------------------

;; -- API functions

;; Convert any zo struct to a spec/c representation.
(define/contract
  (zo->spec z)
  (->i ([z zo?]) () [res (z) (and/c spec/c (specof z))])
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
        [(provided?        z) (provided->string         z)]
        [else (error (format "[zo->string] Unknown zo '~a'" z))]))

;; Convert any zo struct to a string.
;; First builds a spec, then forces the thunks in that spec to build a string.
(define/contract
  (zo->string z #:deep? [deep? #t])
  (->* (zo?) (#:deep? boolean?) string?)
  (format-struct deep? (zo->spec z)))

;; -- syntax: lazy cons to delay evaluation of tail

;; Introduces syntax (lcons a:any b:any).
;; Wraps second argument in a thunk.
(define-syntax (lcons stx)
  (syntax-case stx ()
    [(_)       (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ _)     (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ hd tl) #'(cons hd (lambda () tl))]))

;; -- private functions

(define/contract
  (compilation-top->string z)
  (-> compilation-top? spec/c)
  (list "compilation-top"
        (lcons "max-let-depth" (number->string      (compilation-top-max-let-depth z)))
        (lcons "prefix"        (prefix->string      (compilation-top-prefix z)))
        (lcons "code"          (form-or-any->string (compilation-top-code z)))))

(define/contract
  (prefix->string z)
  (-> prefix? spec/c)
  (define (tl->string tl)
    (cond [(module-variable? tl) (format-struct #f (module-variable->string tl))]
          [(global-bucket?   tl) (format-struct #f (global-bucket->string tl))]
          [(eq? #f tl)           "#f"]
          [else (symbol->string tl)]))
  (list "prefix"
        (lcons "num-lifts" (number->string                (prefix-num-lifts z)))
        (lcons "toplevels" (list->string      tl->string  (prefix-toplevels z)))
        (lcons "stxs"      (listof-zo->string stx->string (prefix-stxs z)))))

(define/contract
  (global-bucket->string  z)
  (-> global-bucket? spec/c)
  (list "global-bucket"
        (lcons "name" (symbol->string (global-bucket-name z)))))

; 2014-12-10: May want to print 'constantness' nicer. Define struct-shape->string.
(define/contract
  (module-variable->string z)
  (-> module-variable? spec/c)
  (list "module-variable"
        (lcons "modidx"       (module-path-index->string (module-variable-modidx z)))
        (lcons "sym"          (symbol->string            (module-variable-sym z)))
        (lcons "pos"          (number->string            (module-variable-pos z)))
        (lcons "phase"        (number->string            (module-variable-phase z)))
        (lcons "constantness" (constantness->string      (module-variable-constantness z)))))

;; TODO improve module-path printing.
;; http://docs.racket-lang.org/reference/Module_Names_and_Loading.html
(define/contract
  (module-path-index->string mpi)
  (-> module-path-index? string?)
  (any->string mpi))

(define/contract
  (module-path->string mp)
  (-> module-path? string?)
  (any->string mp))

(define/contract
  (constantness->string cs)
  (-> (or/c #f 'constant 'fixed function-shape? struct-shape?) string?)
  (cond [(eq? #f cs)          "#f"]
        [(symbol? cs)         (symbol->string         cs)]
        [(function-shape? cs) (function-shape->string cs)]
        [(struct-shape? cs)   (struct-shape->string   cs)]))

(define/contract
  (stx->string z)
  (-> stx? spec/c)
  (list "stx"
        (lcons "encoded" (wrapped->string (stx-encoded z)))))

(define/contract
  (form->string z)
  (-> form? spec/c)
  (cond [(def-values?     z) (def-values->string     z)]
        [(def-syntaxes?   z) (def-syntaxes->string   z)]
        [(seq-for-syntax? z) (seq-for-syntax->string z)]
        [(req?            z) (req->string            z)]
        [(seq?            z) (seq->string            z)]
        [(splice?         z) (splice->string         z)]
        [(inline-variant? z) (inline-variant->string z)]
        [(mod?            z) (mod->string            z)]
        [(provided?       z) (provided->string       z)]
        [(expr?           z) (expr->string           z)]
        [else (error (format "[form->string] Unknown form '~a'" z))]))

(define/contract
  (expr->string z)
  (-> expr? spec/c)
  (cond [(lam?            z) (lam->string            z)]
        [(closure?        z) (closure->string        z)]
        [(case-lam?       z) (case-lam->string       z)]
        [(let-one?        z) (let-one->string        z)]
        [(let-void?       z) (let-void->string       z)]
        [(install-value?  z) (install-value->string  z)]
        [(let-rec?        z) (let-rec->string        z)]
        [(boxenv?         z) (boxenv->string         z)]
        [(localref?       z) (localref->string       z)]
        [(toplevel?       z) (toplevel->string       z)]
        [(topsyntax?      z) (topsyntax->string      z)]
        [(application?    z) (application->string    z)]
        [(branch?         z) (branch->string         z)]
        [(with-cont-mark? z) (with-cont-mark->string z)]
        [(beg0?           z) (beg0->string           z)]
        [(varref?         z) (varref->string         z)]
        [(assign?         z) (assign->string         z)]
        [(apply-values?   z) (apply-values->string   z)]
        [(primval?        z) (primval->string        z)]
        [else (error (format "[expr->string] Unknown expr '~a'" z))]))

(define/contract
  (wrapped->string z)
  (-> wrapped? spec/c)
  (list "wrapped"
        (lcons "datum"         (any->string                    (wrapped-datum z)))
        (lcons "wraps"         (listof-zo->string wrap->string (wrapped-wraps z)))
        (lcons "tamper-status" (symbol->string                 (wrapped-tamper-status z)))))

(define/contract
  (wrap->string z)
  (-> wrap? spec/c)
  (cond [(top-level-rename? z) (top-level-rename->string z)]
        [(mark-barrier?     z) (mark-barrier->string     z)]
        [(lexical-rename?   z) (lexical-rename->string   z)]
        [(phase-shift?      z) (phase-shift->string      z)]
        [(module-rename?    z) (module-rename->string    z)]
        [(wrap-mark?        z) (wrap-mark->string        z)]
        [(prune?            z) (prune->string            z)]
        [else (error (format "[wrap->string] Unknown wrap '~a'" z))]))

(define/contract
  (free-id-info->string z)
  (-> free-id-info? spec/c)
  (list "free-id-info"
        (lcons "path0"                 (module-path-index->string (free-id-info-path0 z)))
        (lcons "symbol0"               (symbol->string            (free-id-info-symbol0 z)))
        (lcons "path1"                 (module-path-index->string (free-id-info-path1 z)))
        (lcons "symbol1"               (symbol->string            (free-id-info-symbol1 z)))
        (lcons "phase0"                (phase->string             (free-id-info-phase0 z)))
        (lcons "phase1"                (phase->string             (free-id-info-phase1 z)))
        (lcons "phase2"                (phase->string             (free-id-info-phase2 z)))
        (lcons "use-current-inspector" (boolean->string           (free-id-info-use-current-inspector? z)))))

(define/contract
  (phase->string ph)
  (-> (or/c exact-integer? #f) string?)
  (cond [(number? ph) (number->string ph)]
        [else "#f"]))

(define/contract
  (all-from-module->string z)
  (-> all-from-module? spec/c)
  (define (exception->string ex)
    (list->string symbol->string ex))
  (define (prefix->string px)
    (if (symbol? px)
        (symbol->string px)
        "#f"))
  (define (context->string ctx)
    (cond [(eq? #f ctx)  "#f"]
          [(list? ctx)   (list->string number->string ctx)]
          [(vector? ctx) (format-list #:sep " "
                                      (list (list->string number->string (vector-ref ctx 0))
                                            (any->string                 (vector-ref ctx 1))))]))
  (list "all-from-module"
        (lcons "path"      (module-path-index->string (all-from-module-path z)))
        (lcons "phase"     (phase->string             (all-from-module-phase z)))
        (lcons "src-phase" (phase->string             (all-from-module-src-phase z)))
        (lcons "exception" (exception->string         (all-from-module-exceptions z)))
        (lcons "prefix"    (prefix->string            (all-from-module-prefix z)))
        (lcons "context"   (context->string           (all-from-module-context z)))))

(define/contract
  (module-binding->string z)
  (-> module-binding? spec/c)
  (cond [(simple-module-binding?           z) (simple-module-binding->string           z)]
        [(phased-module-binding?           z) (phased-module-binding->string           z)]
        [(exported-nominal-module-binding? z) (exported-nominal-module-binding->string z)]
        [(nominal-module-binding?          z) (nominal-module-binding->string          z)]
        [(exported-module-binding?         z) (exported-module-binding->string         z)]
        [else (error (format "[module-binding->string] Unknown '~a'" z))]))

(define/contract
  (nominal-path->string z)
  (-> nominal-path? spec/c)
  (cond [(simple-nominal-path?   z) (simple-nominal-path->string   z)]
        [(imported-nominal-path? z) (imported-nominal-path->string z)]
        [(phased-nominal-path?   z) (phased-nominal-path->string   z)]
        [else (error (format "[nominal-path] Unknown '~a'" z))]))

;; -- form

(define/contract
  (def-values->string z)
  (-> def-values? spec/c)
  (list "def-values"
        (lcons "ids" (listof-zo->string toplevel->string (def-values-ids z)))
        (lcons "rhs" (let ([rhs (def-values-rhs z)])
                       (cond [(inline-variant? rhs) (inline-variant->string rhs)]
                             [else (expr-seq-any->string rhs)])))))

(define/contract
  (def-syntaxes->string z)
  (-> def-syntaxes? spec/c)
  (list "def-syntaxes"
        (lcons "ids"           (list->string symbol->string         (def-syntaxes-ids z)))
        (lcons "rhs"           (expr-seq-any->string                (def-syntaxes-rhs z)))
        (lcons "prefix"        (prefix->string                      (def-syntaxes-prefix z)))
        (lcons "max-let-depth" (number->string                      (def-syntaxes-max-let-depth z)))
        (lcons "dummy"         (toplevel-or-any->string             (def-syntaxes-dummy z)))))

(define/contract
  (seq-for-syntax->string z)
  (-> seq-for-syntax? spec/c)
  (list "seq-for-syntax"
        (lcons "forms"         (listof-form-or-any->string (seq-for-syntax-forms z)))
        (lcons "prefix"        (prefix->string             (seq-for-syntax-prefix z)))
        (lcons "max-let-depth" (number->string             (seq-for-syntax-max-let-depth z)))
        (lcons "dummy"         (toplevel-or-any->string    (seq-for-syntax-dummy z)))))

(define/contract
  (req->string z)
  (-> req? spec/c)
  (list "req"
        (lcons "reqs"  (stx->string      (req-reqs z)))
        (lcons "dummy" (toplevel->string (req-dummy z)))))

(define/contract
  (seq->string z)
  (-> seq? spec/c)
  (list "seq"
        (lcons "forms" (listof-form-or-any->string (seq-forms z)))))

(define/contract
  (splice->string z)
  (-> splice? spec/c)
  (list "splice"
        (lcons "forms" (listof-form-or-any->string (splice-forms z)))))

(define/contract
  (inline-variant->string z)
  (-> inline-variant? spec/c)
  (list "inline-variant"
        (lcons "direct" (expr->string (inline-variant-direct z)))
        (lcons "inline" (expr->string (inline-variant-inline z)))))

(define/contract
  (mod->string z)
  (-> mod? spec/c)
  (define (name->string nm)
    (if (symbol? nm)
        (symbol->string nm)
        (list->string  symbol->string nm)))
  (define (unexported->string ux)
    (define (elem->string e)
      (format-list #:sep " "
                   (list (number->string              (car e))
                         (list->string symbol->string (cadr e))
                         (list->string symbol->string (caddr e)))))
    (list->string elem->string ux))
  (define (lang-info->string li)
    (if (eq? #f li)
        "#f"
        (format-list #:sep " "
                     (list (module-path->string (vector-ref li 0))
                           (symbol->string      (vector-ref li 1))
                           (any->string         (vector-ref li 2))))))
  (list "mod"
        (lcons "name"             (name->string               (mod-name z)))
        (lcons "srcname"          (symbol->string             (mod-srcname z)))
        (lcons "self-modidx"      (module-path-index->string  (mod-self-modidx z)))
        (lcons "prefix"           (prefix->string             (mod-prefix z)))
        (lcons "provides"         (mod-provides->string       (mod-provides z)))
        (lcons "requires"         (mod-requires->string       (mod-requires z)))
        (lcons "body"             (listof-form-or-any->string (mod-body z)))
        (lcons "syntax-bodies"    (mod-syntax-bodies->string  (mod-syntax-bodies z)))
        (lcons "unexported"       (unexported->string         (mod-unexported z)))
        (lcons "max-let-depth"    (number->string             (mod-max-let-depth z)))
        (lcons "dummy"            (toplevel->string           (mod-dummy z)))
        (lcons "lang-info"        (lang-info->string          (mod-lang-info z)))
        (lcons "internal-context" (let ([ic (mod-internal-context z)])
                                    (cond [(stx? ic)    (stx->string                   ic)]
                                          [(vector? ic) (listof-zo->string stx->string (vector->list ic))]
                                          [else         (boolean->string               ic)])))
        (lcons "flags"            (list->string   symbol->string (mod-flags z)))
        (lcons "pre-submodules"   (listof-zo->string mod->string (mod-pre-submodules z)))
        (lcons "post-submodules"  (listof-zo->string mod->string (mod-post-submodules z)))))

(define/contract
  (mod-provides->string pds)
  (-> (listof (list/c (or/c exact-integer? #f) (listof provided?) (listof provided?))) string?)
  (define (elem->string e)
    (format-list #:sep " "
                 (list (if (number? (car e))
                           (number->string (car e))
                           "#f")
                       (listof-zo->string provided->string (cadr e))
                       (listof-zo->string provided->string (caddr e)))))
  (list->string elem->string pds))

(define/contract
  (mod-requires->string rqs)
  (-> (listof (cons/c (or/c exact-integer? #f) (listof module-path-index?))) string?)
  (define (elem->string e)
    (format-list #:sep " "
                 (list (if (number? (car e))
                           (number->string (car e))
                           "#f")
                       (list->string module-path-index->string (cdr e)))))
  (list->string elem->string rqs))

;; 2014-12-10: Ugly
(define/contract
  (mod-syntax-bodies->string sbs)
  (-> (listof (cons/c exact-positive-integer? (listof (or/c def-syntaxes? seq-for-syntax?)))) string?)
  (define (ds-or-sfs->string d)
    (cond [(def-syntaxes?   d) (def-syntaxes->string d)]
          [(seq-for-syntax? d) (seq-for-syntax->string d)]
          [else (error "[mod-syntax-bodies->string] Unexpected arg")]))
  (define (elem->string e)
    (format-list #:sep " "
                 (list (number->string                 (car e))
                       (list->string ds-or-sfs->string (cdr e)))))
  (list->string elem->string sbs))
                
(define/contract
  (provided->string z)
  (-> provided? spec/c)
  (list "provided"
        (lcons "name"      (provided-name z))
        (lcons "src"       (provided-src  z))
        (lcons "src-name"  (provided-src-name z))
        (lcons "nom-src"   (provided-nom-src z))
        (lcons "src-phase" (provided-src-phase z))
        (lcons "protected" (provided-protected? z))))

;; -- expr

(define/contract
  (lam->string z)
  (-> lam? spec/c)
  (define (closure-map->string cm)
    (format-list #:sep " " (for/list ([n cm]) (number->string n))))
  (define (toplevel-map->string tm)
    (cond [(eq? #f tm) "#f"]
          [else (format-list #:sep " " (for/list ([n tm]) (number->string n)))]))
  (list "lam"
        (lcons "name"          (lam-name->string            (lam-name z)))
        (lcons "flags"         (list->string symbol->string (lam-flags z)))
        (lcons "num-params"    (number->string              (lam-num-params z)))
        (lcons "param-types"   (list->string symbol->string (lam-param-types z)))
        (lcons "rest"          (boolean->string             (lam-rest? z)))
        (lcons "closure-map"   (closure-map->string         (lam-closure-map z)))
        (lcons "closure-types" (list->string symbol->string (lam-closure-types z)))
        (lcons "toplevel-map"  (toplevel-map->string        (lam-toplevel-map z)))
        (lcons "max-let-depth" (number->string              (lam-max-let-depth z)))
        (lcons "body"          (expr-seq-any->string        (lam-body z)))))

(define/contract
  (closure->string z)
  (-> closure? spec/c)
  (list "closure"
        (lcons "code"   (lam->string    (closure-code z)))
        (lcons "gen-id" (symbol->string (closure-gen-id z)))))

(define/contract
  (case-lam->string z)
  (-> case-lam? spec/c)
  (list "case-lam"
        (lcons "name"    (lam-name->string              (case-lam-name z)))
        (lcons "clauses" (listof-zo->string lam->string (case-lam-clauses z)))))

(define/contract
  (let-one->string z)
  (-> let-one? spec/c)
  (list "let-one"
        (lcons "rhs"    (expr-seq-any->string (let-one-rhs  z)))
        (lcons "body"   (expr-seq-any->string (let-one-body z)))
        (lcons "type"   (symbol-or-f->string  (let-one-type z)))
        (lcons "unused" (boolean->string      (let-one-unused? z)))))

(define/contract
  (let-void->string z)
  (-> let-void? spec/c)
  (list "let-void"
        (lcons "count" (number->string       (let-void-count z)))
        (lcons "boxes" (boolean->string      (let-void-boxes? z)))
        (lcons "body"  (expr-seq-any->string (let-void-body z)))))

(define/contract
  (install-value->string z)
  (-> install-value? spec/c)
  (list "install-value"
        (lcons "count" (number->string       (install-value-count z)))
        (lcons "pos"   (number->string       (install-value-pos z)))
        (lcons "boxes" (boolean->string      (install-value-boxes? z)))
        (lcons "rhs"   (expr-seq-any->string (install-value-rhs z)))
        (lcons "body"  (expr-seq-any->string (install-value-body z)))))

(define/contract
  (let-rec->string z)
  (-> let-rec? spec/c)
  (list "let-rec"
        (lcons "procs" (listof-zo->string lam->string (let-rec-procs z)))
        (lcons "body"  (expr-seq-any->string          (let-rec-body z)))))

(define/contract
  (boxenv->string z)
  (-> boxenv? spec/c)
  (list "boxenv"
        (lcons "pos"  (number->string       (boxenv-pos z)))
        (lcons "body" (expr-seq-any->string (boxenv-body z)))))

(define/contract
  (localref->string z)
  (-> localref? spec/c)
  (list "localref"
        (lcons "unbox"        (boolean->string     (localref-unbox? z)))
        (lcons "pos"          (number->string      (localref-pos z)))
        (lcons "clear"        (boolean->string     (localref-clear? z)))
        (lcons "other-clears" (boolean->string     (localref-other-clears? z)))
        (lcons "type"         (symbol-or-f->string (localref-type z)))))

(define/contract
  (toplevel->string z)
  (-> toplevel? spec/c)
  (list
        "toplevel"
        (lcons "depth" (number->string  (toplevel-depth z)))
        (lcons "pos"   (number->string  (toplevel-pos z)))
        (lcons "const" (boolean->string (toplevel-const? z)))
        (lcons "ready" (boolean->string (toplevel-ready? z)))))

(define/contract
  (topsyntax->string z)
  (-> topsyntax? spec/c)
  (list "topsyntax"
        (lcons "depth" (number->string (topsyntax-depth z)))
        (lcons "pos"   (number->string (topsyntax-pos z)))
        (lcons "midpt" (number->string (topsyntax-midpt z)))))

(define/contract
  (application->string z)
  (-> application? spec/c)
  (list "application"
        (lcons "rator" (expr-seq-any->string              (application-rator z)))
        (lcons "rands" (list->string expr-seq-any->string (application-rands z)))))

(define/contract
  (branch->string z)
  (-> branch? spec/c)
  (list "branch"
        (lcons "test" (expr-seq-any->string (branch-test z)))
        (lcons "then" (expr-seq-any->string (branch-then z)))
        (lcons "else" (expr-seq-any->string (branch-else z)))))

(define/contract
  (with-cont-mark->string z)
  (-> with-cont-mark? spec/c)
  (list "with-cont-mark"
        (lcons "key"  (expr-seq-any->string (with-cont-mark-key  z)))
        (lcons "val"  (expr-seq-any->string (with-cont-mark-val  z)))
        (lcons "body" (expr-seq-any->string (with-cont-mark-body z)))))

(define/contract
  (beg0->string z)
  (-> beg0? spec/c)
  (list "beg0"
        (lcons "seq" (list->string expr-seq-any->string (beg0-seq)))))

(define/contract
  (varref->string z)
  (-> varref? spec/c)
  (list "varref"
        (lcons "toplevel" (let ([tl (varref-toplevel z)])
                            (cond [(eq? tl #t)    "#t"]
                                  [(toplevel? tl) (toplevel->string tl)])))
        (lcons "dummy"    (let ([dm (varref-dummy z)])
                            (cond [(eq? dm #f)    "#f"]
                                  [(toplevel? dm) (toplevel->string dm)])))))

(define/contract
  (assign->string z)
  (-> assign? spec/c)
  (list "assign"
        (lcons "id"       (toplevel->string     (assign-id z)))
        (lcons "rhs"      (expr-seq-any->string (assign-rhs z)))
        (lcons "undef-ok" (boolean->string      (assign-undef-ok? z)))))

(define/contract
  (apply-values->string z)
  (-> apply-values? spec/c)
  (list "apply-values"
        (lcons "proc"      (expr-seq-any->string (apply-values-proc z)))
        (lcons "args-expr" (expr-seq-any->string (apply-values-args-expr z)))))

(define/contract
  (primval->string z)
  (-> primval? spec/c)
  (list "primval"
        (lcons "id" (number->string (primval-id z)))))

;; -- expr helpers

(define/contract
  (lam-name->string nm)
  (-> (or/c symbol? vector?) string?)
  (cond [(vector? nm) (any->string nm)]
        [else         (symbol->string nm)]))

;; -- wrap

(define/contract
  (top-level-rename->string z)
  (-> top-level-rename? spec/c)
  (list "top-level-rename"
        (lcons "flag" (boolean->string (top-level-rename z)))))

(define/contract
  (mark-barrier->string z)
  (-> mark-barrier? spec/c)
  (list "mark-barrier"
        (lcons "value" (symbol->string (mark-barrier-value z)))))

(define/contract
  (lexical-rename->string z)
  (-> lexical-rename? spec/c)
  (list "lexical-rename"
        (lcons "has-free-id-renames" (boolean->string              (lexical-rename-has-free-id-renames? z)))
        (lcons "bool2"               (boolean->string              (lexical-rename-bool2 z)))
        (lcons "alist"               (lexical-rename-alist->string (lexical-rename-alist z)))))

;; 2014-12-10: TODO Ugly!!!
(define/contract
  (lexical-rename-alist->string alst)
  (-> (listof (cons/c symbol? (or/c symbol? (cons/c symbol? (or/c (cons/c symbol? (or/c symbol? #f)) free-id-info?))))) string?)
  (format-list #:sep " "
               (for/list ([a alst])
                 (format "(~a . ~a)"
                         (car a)
                         (cond [(symbol? (cdr a)) (cdr a)]
                               [else (define a* (cdr a))
                                     (format "(~a . ~a)"
                                             (car a*)
                                             (cond [(free-id-info? (cdr a*)) (free-id-info->string (cdr a*))]
                                                   [else                     (cdr a*)]))])))))
  
(define/contract
  (phase-shift->string z)
  (-> phase-shift? spec/c)
  (list "phase-shift"
        (lcons "amt"       (number-or-f->string       (phase-shift-amt z)))
        (lcons "src"       (module-path-index->string (phase-shift-src z)))
        (lcons "dest"      (module-path-index->string (phase-shift-dest z)))
        (lcons "cancel-id" (number-or-f->string       (phase-shift-cancel-id z)))))

;; 2014-12-10: Curious about 'unmarshals'
(define/contract
  (module-rename->string z)
  (-> module-rename? spec/c)
  (list "module-rename"
        (lcons "phase"        (number->string                            (module-rename-phase z)))
        (lcons "kind"         (symbol->string                            (module-rename-kind z)))
        (lcons "set-id"       (any->string                               (module-rename-set-id z)))
        (lcons "unmarshals"   (listof-zo->string all-from-module->string (module-rename-unmarshals z)))
        (lcons "renames"      (listof-zo->string module-binding->string  (module-rename-renames z)))
        (lcons "mark-renames" (any->string                               (module-rename-mark-renames z)))
        (lcons "plus-kern"    (boolean->string                           (module-rename-plus-kern? z)))))

(define/contract
  (wrap-mark->string z)
  (-> wrap-mark? spec/c)
  (list "wrap-mark"
        (lcons "val" (wrap-mark-val z)))) ;; TODO test, break, fix

(define/contract
  (prune->string z)
  (-> prune? spec/c)
  (list "prune"
        (lcons "sym" (symbol->string (prune-sym z)))))

;; -- module-binding

(define/contract
  (simple-module-binding->string z)
  (-> simple-module-binding? spec/c)
  (list "simple-module-binding"
        (lcons "path" (module-path-index->string (simple-module-binding-path z)))))

(define/contract
  (phased-module-binding->string z)
  (-> phased-module-binding? spec/c)
  (list "phased-module-binding"
        (lcons "path"                (module-path-index->string (phased-module-binding-path z)))
        (lcons "phase"               (number->string            (phased-module-binding-phase z)))
        (lcons "export-name"         (any->string               (phased-module-binding-export-name z)))
        (lcons "nominal-path"        (nominal-path->string      (phased-module-binding-nominal-path z)))
        (lcons "nominal-export-name" (any->string               (phased-module-binding-nominal-export-name z)))))

(define/contract
  (exported-nominal-module-binding->string z)
  (-> exported-nominal-module-binding? spec/c)
  (list "exported-nominal-module-binding"
        (lcons "path"                (module-path-index->string (exported-nominal-module-binding-path z)))
        (lcons "export-name"         (any->string               (exported-nominal-module-binding-export-name z)))
        (lcons "nominal-path"        (nominal-path->string      (exported-nominal-module-binding-nominal-path z)))
        (lcons "nominal-export-name" (any->string               (exported-nominal-module-binding-nominal-export-name z)))))

(define/contract
  (nominal-module-binding->string z)
  (-> nominal-module-binding? spec/c)
  (list "nominal-module-binding"
        (lcons "path"         (module-path-index->string (nominal-module-binding-path z)))
        (lcons "nominal-path" (nominal-path->string      (nominal-module-binding-nominal-path z)))))

(define/contract
  (exported-module-binding->string z)
  (-> exported-module-binding? spec/c)
  (list "exported-module-binding"
        (lcons "path"        (module-path-index->string (exported-module-binding-path z)))
        (lcons "export-name" (any->string               (exported-module-binding-export-name z)))))

;; -- nominal-path

(define/contract
  (simple-nominal-path->string z)
  (-> simple-nominal-path? spec/c)
  (list "simple-nominal-path"
        (lcons "value" (module-path-index->string (simple-nominal-path-value z)))))

(define/contract
  (imported-nominal-path->string z)
  (-> imported-nominal-path? spec/c)
  (list "imported-nominal-path"
        (lcons "value"        (module-path-index->string (imported-nominal-path-value z)))
        (lcons "import-phase" (number->string            (imported-nominal-path-import-phase z)))))

(define/contract
  (phased-nominal-path->string z)
  (-> phased-nominal-path? spec/c)
  (list "phased-nominal-path"
        (lcons "value"        (module-path-index->string (phased-nominal-path-value z)))
        (lcons "import-phase" (number->string            (phased-nominal-path-import-phase z)))
        (lcons "phase"        (number->string            (phased-nominal-path-phase z)))))

;; -- Shapes 

(define/contract
  (function-shape->string fs)
  (-> function-shape? string?)
  (format-list #:sep " "
               (list "function-shape"
                     (format "arity : ~a"            (function-shape-arity fs))
                     (format "preserves-marks? : ~a" (function-shape-preserves-marks? fs)))))

(define/contract
  (struct-shape->string ss)
  (-> struct-shape? string?)
  (cond [(struct-type-shape?  ss) (struct-type-shape->string  ss)]
        [(constructor-shape?  ss) (constructor-shape->string  ss)]
        [(predicate-shape?    ss) (predicate-shape->string    ss)]
        [(accessor-shape?     ss) (accessor-shape->string     ss)]
        [(mutator-shape?      ss) (mutator-shape->string      ss)]
        [(struct-other-shape? ss) (struct-other-shape->string ss)]
        [else (error (format "unknown struct shape ~a" ss))]))

(define/contract
  (struct-type-shape->string sts)
  (-> struct-type-shape? string?)
  (format-list #:sep " "
               (list "struct-type-shape"
                     (format "field-count : ~a" (struct-type-shape-field-count sts)))))

(define/contract
  (constructor-shape->string cs)
  (-> constructor-shape? string?)
  (format-list #:sep " "
               (list "constructor-shape"
                     (format "arity : ~a" (constructor-shape-arity cs)))))

(define/contract
  (predicate-shape->string ps)
  (-> predicate-shape? string?)
  (format-list (list "predicate-shape")))

(define/contract
  (accessor-shape->string sts)
  (-> accessor-shape? string?)
  (format-list #:sep " "
               (list "accessor-shape"
                     (format "field-count : ~a" (accessor-shape-field-count sts)))))

(define/contract
  (mutator-shape->string sts)
  (-> mutator-shape? string?)
  (format-list #:sep " "
               (list "mutator-shape"
                     (format "field-count : ~a" (mutator-shape-field-count sts)))))

(define/contract
  (struct-other-shape->string ps)
  (-> struct-other-shape? string?)
  (format-list (list "struct-other-shape")))

;; -- helpers

(define/contract
  (any->string z)
  (-> any/c string?)
  (format "~a" z))

(define/contract
  (boolean->string b)
  (-> boolean? string?)
  (if b "#t" "#f"))

(define/contract
  (expr-seq-any->string z)
  (-> (or/c expr? seq? any/c) string?)
  (cond [(expr? z) (format-struct #f (list "expr"))]
        [(seq?  z) (format-struct #f (list "seq"))]
        [else      (any->string z)]))

(define/contract
  (form-or-any->string fm)
  (-> (or/c form? any/c) string?)
  (cond [(form? fm) (format-struct #f (form->string fm))]
        [else       (any->string   fm)]))

(define/contract
  (symbol-or-f->string sf)
  (-> (or/c symbol? #f) string?)
  (if (eq? #f sf)
      "#f"
      (symbol->string sf)))

(define/contract
  (number-or-f->string nf)
  (-> (or/c number? #f) string?)
  (if (eq? #f nf)
      "#f"
      (number->string nf)))

(define/contract
  (format-list xs #:sep [sep "\n"])
  (->* ((listof string?)) (#:sep string?) string?)
  (string-join xs sep))

(define/contract
  (format-struct deep? struct-spec)
  (-> boolean? spec/c string?)
  (define fields (cdr struct-spec))
  (define title (format "<struct:~a>" (car struct-spec)))
  (define field-name-lengths
    (for/list ([fd fields]) (string-length (car fd))))
  (define w ;; width of longest struct field name
    (if (empty? fields) 0 (apply max field-name-lengths)))
  (if (not deep?)
      title
      (format-list (cons title
                         (for/list ([fd fields])
                           (define forced ((cdr fd)))
                           (define rest   (if (string? forced)
                                              forced
                                              (format-struct #f forced)))
                           (format "  ~a : ~a" (pad (car fd) w) rest))))))

(define/contract
  (list->string f xs)
  (-> (-> any/c string?) (listof any/c) string?)
  (format "[~a]"
          (format-list #:sep " "
                       (for/list ([x xs]) (f x)))))

(define/contract
  (listof-form-or-any->string xs)
  (-> (listof (or/c form? any/c)) string?)
  (list->string form-or-any->string xs))

(define/contract
  (listof-zo->string z->spec zs)
  (-> (-> zo? spec/c) (listof zo?) string?)
  (cond [(empty? zs) "[]"]
        [else        (format "~a[~a]" (format-struct #f (z->spec (car zs))) (length zs))]))

;; True if `str` starts with `prefix`.
(define/contract
  (starts-with str prefix)
  (-> string? string? boolean?)
  (and (<= (string-length prefix) (string-length str))
       (string=? (substring str 0 (string-length prefix))
                 prefix)))

;; If [str] has fewer than [w] characters, (w - (len str)) characters to its right end
(define/contract
  (pad str w #:char [c #\space])
  (->i ([str string?] [n natural-number/c]) () [str* (str n)
                                                     (lambda (str*) (and (= n (string-length str*))
                                                                         (starts-with str* str)))])
  (define l (string-length str))
  (cond [(< l w) (format "~a~a" str (make-string (- w l) c))]
        [else    str]))

(define/contract
  (toplevel-or-any->string tl)
  (-> (or/c toplevel? any/c) string?)
  (cond [(toplevel? tl) (format-struct #f (toplevel->string tl))]
        [else           (any->string tl)]))

;; -- testing

(module+ test
  (require rackunit)
  (check-equal? #t #t)
)
