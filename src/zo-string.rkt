#lang racket/base

(provide zo->string)

(require compiler/zo-structs
         racket/contract
         (only-in racket/string string-join)
         (only-in racket/list   empty?))

;; -- API functions

(define/contract
  (zo->string z #:deep? [deep? #t])
  (->* (zo?) (#:deep? boolean?) string?)
  (define str-spec
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
  (format-struct deep? str-spec))

;; -- syntax: lazy cons to delay evaluation of tail

(require (for-syntax racket/base racket/syntax))
(define-syntax (lcons stx)
  (syntax-case stx ()
    [(_)       (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ _)     (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ hd tl) #'(cons hd (lambda () tl))]))

;; -- string specifications

(define (summary? xs)
  (and (list? xs)
       (< 0 (length xs))
       (string? (car xs))
       (for/and ([s (cdr xs)]) (string? s))
       ))
       ;; (listof (cons/c string? (-> summary?)))))

;; beware, tails are not always strings (but probably should be).
(define (summaryof z)
  ;; (-> zo? (-> (cons/c boolean? (cons/c string? (listof (cons/c string? (-> string?)))) boolean?))
  (cons/c string? (listof (cons/c string? (-> string?)))))
  ;; (lambda (str-list)
  ;;   ;; A proper zo-spec should have the title and, if deep, a string for each struct field
  ;;   (and/c (list?  str-list)
  ;;          (< 0 (length str-list))
  ;;          (string? (car str-list))
  ;;          ;; Title matches struct name
  ;;          (and (= (length str-list) (vector-length (struct->vector z)))
  ;;               (for/and ([lpair (cdr xs)]) (and (string? (car xs)) (
  ;;               #t) ;; cdr is list of (cons string? (-> summary?))
  ;;                   ;; each struct field appears in one pair
  ;;          )))

;; -- private functions

(define/contract
  (compilation-top->string z)
  (-> compilation-top? (summaryof compilation-top))
  (list "compilation-top"
        (lcons "max-let-depth" (compilation-top-max-let-depth z))
        (lcons "prefix"        (prefix->string #f (compilation-top-prefix z)))
        (lcons "code"          (form-or-any->string (compilation-top-code z)))))

(define/contract
  (prefix->string z)
  (-> prefix? (summaryof prefix))
  (list "prefix"
        (lcons "num-lifts" (prefix-num-lifts z))
        (lcons "toplevels" (prefix-toplevels->string (prefix-toplevels z)))
        (lcons "stxs"      (listof-zo->string stx->string (prefix-stxs z)))))

(define/contract
  (prefix-toplevels->string tls)
  (-> (listof (or/c module-variable? global-bucket? any/c)) (listof string?))
  (for/list ([tl tls])
    (cond [(module-variable? tl) (module-variable->string #f tl)]
          [(global-bucket?   tl) (global-bucket->string #f tl)]
          [else                  (format "~a" tl)])))

(define/contract
  (global-bucket->string  z)
  (-> global-bucket? (summaryof global-bucket))
  (list "global-bucket"
        (lcons "name" (global-bucket-name z))))

; 2014-12-10: May want to print 'constantness' nicer. Define struct-shape->string.
(define/contract
  (module-variable->string z)
  (-> module-variable? (summaryof module-variable))
  (list "module-variable"
        (lcons "modidx"       (module-variable-modidx z))
        (lcons "sym"          (module-variable-sym z))
        (lcons "pos"          (module-variable-pos z))
        (lcons "phase"        (module-variable-phase z))
        (lcons "constantness" (module-variable-constantness z))))

(define/contract
  (stx->string z)
  (-> stx? (summaryof stx))
  (list "stx"
        (lcons "encoded" (wrapped->string #f (stx-encoded z)))))

(define/contract
  (form->string z)
  (-> form? (summaryof form))
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
  (-> expr? (summaryof expr))
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
  (-> wrapped? (summaryof wrapped))
  (list "wrapped"
        (lcons "datum"         (wrapped-datum z))
        (lcons "wraps"         (listof-zo->string wrap->string (wrapped-wraps z)))
        (lcons "tamper-status" (wrapped-tamper-status z))))

(define/contract
  (wrap->string z)
  (-> wrap? (summaryof wrap))
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
  (-> free-id-info? (summaryof free-id-info))
  (list "free-id-info"
        (lcons "path0"                 (free-id-info-path0 z))
        (lcons "symbol0"               (free-id-info-symbol0 z))
        (lcons "path1"                 (free-id-info-path1 z))
        (lcons "symbol1"               (free-id-info-symbol1 z))
        (lcons "phase0"                (free-id-info-phase0 z))
        (lcons "phase1"                (free-id-info-phase1 z))
        (lcons "phase2"                (free-id-info-phase2 z))
        (lcons "use-current-inspector" (free-id-info-use-current-inspector? z))))

(define/contract
  (all-from-module->string z)
  (-> all-from-module? (summaryof all-from-module))
  (list "all-from-module"
        (lcons "path"      (all-from-module-path z))
        (lcons "phase"     (all-from-module-phase z))
        (lcons "src-phase" (all-from-module-src-phase z))
        (lcons "exception" (all-from-module-exceptions z))
        (lcons "prefix"    (all-from-module-prefix z))
        (lcons "context"   (all-from-module-context z))))

(define/contract
  (module-binding->string z)
  (-> module-binding? (summaryof module-binding))
  (cond [(simple-module-binding?           z) (simple-module-binding->string           z)]
        [(phased-module-binding?           z) (phased-module-binding->string           z)]
        [(exported-nominal-module-binding? z) (exported-nominal-module-binding->string z)]
        [(nominal-module-binding?          z) (nominal-module-binding->string          z)]
        [(exported-module-binding?         z) (exported-module-binding->string         z)]
        [else (error (format "[module-binding->string] Unknown '~a'" z))]))

(define/contract
  (nominal-path->string z)
  (-> nominal-path? (summaryof nominal-path))
  (cond [(simple-nominal-path?   z) (simple-nominal-path->string   z)]
        [(imported-nominal-path? z) (imported-nominal-path->string z)]
        [(phased-nominal-path?   z) (phased-nominal-path->string   z)]
        [else (error (format "[nominal-path] Unknown '~a'" z))]))

;; -- form

(define/contract
  (def-values->string z)
  (-> def-values? (summaryof def-values))
  (list "def-values"
        (lcons "ids" (listof-zo->string def-values->string (def-values-ids z)))
        (lcons "rhs" (let ([rhs (def-values-rhs z)])
                       (cond [(expr? rhs) (expr->string #f rhs)]
                             [(seq?  rhs) (seq->string #f rhs)]
                             [(inline-variant? rhs) (inline-variant->string #f rhs)]
                             [else                  rhs])))))

(define/contract
  (def-syntaxes->string z)
  (-> def-syntaxes? (summaryof def-syntaxes))
  (list "def-syntaxes"
        (lcons "ids"           (def-syntaxes-ids z))
        (lcons "rhs"           (expr-seq-any->string (def-syntaxes-rhs z)))
        (lcons "prefix"        (prefix->string #f (def-syntaxes-prefix z)))
        (lcons "max-let-depth" (def-syntaxes-max-let-depth z))
        (lcons "dummy"         (toplevel-or-any->string (def-syntaxes-dummy z)))))

(define/contract
  (seq-for-syntax->string z)
  (-> seq-for-syntax? (summaryof seq-for-syntax))
  (list "seq-for-syntax"
        (lcons "forms"         (listof-form-or-any->string (seq-for-syntax-forms z)))
        (lcons "prefix"        (prefix->string #f (seq-for-syntax-prefix z)))
        (lcons "max-let-depth" (seq-for-syntax-max-let-depth z))
        (lcons "dummy"         (toplevel-or-any->string (seq-for-syntax-dummy z)))))

(define/contract
  (req->string z)
  (-> req? (summaryof req))
  (list "req"
        (lcons "reqs"  (stx->string #f (req-reqs z)))
        (lcons "dummy" (toplevel->string #f (req-dummy z)))))

(define/contract
  (seq->string z)
  (-> seq? (summaryof seq))
  (list "seq"
        (lcons "forms" (listof-form-or-any->string (seq-forms z)))))

(define/contract
  (splice->string z)
  (-> splice? (summaryof splice))
  (list "splice"
        (lcons "forms" (listof-form-or-any->string (splice-forms z)))))

(define/contract
  (inline-variant->string z)
  (-> inline-variant? (summaryof inline-variant))
  (list "inline-variant"
        (lcons "direct" (expr->string #f (inline-variant-direct z)))
        (lcons "inline" (expr->string #f (inline-variant-inline z)))))

(define/contract
  (mod->string z)
  (-> mod? (summaryof mod))
  (list "mod"
        (lcons "name"             (mod-name z))
        (lcons "srcname"          (mod-srcname z))
        (lcons "self-modidx"      (mod-self-modidx z))
        (lcons "prefix"           (prefix->string #f (mod-prefix z)))
        (lcons "provides"         (mod-provides->string (mod-provides z)))
        (lcons "requires"         (mod-requires->string (mod-requires z)))
        (lcons "body"             (listof-form-or-any->string (mod-body z)))
        (lcons "syntax-bodies"    (mod-syntax-bodies->string (mod-syntax-bodies z)))
        (lcons "unexported"       (mod-unexported z))
        (lcons "max-let-depth"    (mod-max-let-depth z))
        (lcons "dummy"            (toplevel->string #f (mod-dummy z)))
        (lcons "lang-info"        (mod-lang-info z))
        (lcons "internal-context" (let ([ic (mod-internal-context z)])
                                    (cond [(stx? ic)    (stx->string #f ic)]
                                          [(vector? ic) (listof-zo->string stx->string (vector->list ic))]
                                          [else         ic])))
        (lcons "flags"            (mod-flags z))
        (lcons "pre-submodules"   (listof-zo->string mod->string (mod-pre-submodules z)))
        (lcons "post-submodules"  (listof-zo->string mod->string (mod-post-submodules z)))))

(define/contract
  (mod-provides->string pds)
  (-> (listof (list/c (or/c exact-integer? #f) (listof provided?) (listof provided?))) string?)
  (format-list #:sep " "
               (for/list ([pd pds])
                 (format "(~a ~a ~a)"
                         (car pd) ; (or/c exact-integer? #f)
                         (format "~a" (listof-zo->string provided->string (cadr pd)))
                         (format "~a" (listof-zo->string provided->string (caddr pd)))))))

(define/contract
  (mod-requires->string rqs)
  (-> (listof (cons/c (or/c exact-integer? #f) (listof module-path-index?))) string?)
  (format-list #:sep " "
               (for/list ([rq rqs])
                 (format "(~a ~a)" (car rq) (cdr rq)))))

;; 2014-12-10: Ugly
(define/contract
  (mod-syntax-bodies->string sbs)
  (-> (listof (cons/c exact-positive-integer? (listof (or/c def-syntaxes? seq-for-syntax?)))) string?)
  (format-list #:sep " "
               (for/list ([sb sbs])
                 (format "(~a . ~a)"
                         (car sb)
                         (for/list ([d (cdr sb)])
                                (cond [(def-syntaxes?   d) (def-syntaxes->string #f d)]
                                      [(seq-for-syntax? d) (seq-for-syntax->string #f d)]
                                      [else (error "[mod-syntax-bodies->string] Unexpected arg")]))))))
                
(define/contract
  (provided->string z)
  (-> provided? (summaryof provided))
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
  (-> lam? (summaryof lam))
  (list "lam"
        (lcons "name"          (lam-name z))
        (lcons "flags"         (lam-flags z))
        (lcons "num-params"    (lam-num-params z))
        (lcons "param-types"   (lam-param-types z))
        (lcons "rest"          (lam-rest? z))
        (lcons "closure-map"   (lam-closure-map z))
        (lcons "closure-types" (lam-closure-types z))
        (lcons "toplevel-map"  (lam-toplevel-map z))
        (lcons "max-let-depth" (lam-max-let-depth z))
        (lcons "body"          (expr-seq-any->string (lam-body z)))))

(define/contract
  (closure->string z)
  (-> closure? (summaryof closure))
  (list "closure"
        (lcons "code"   (lam->string #f (closure-code z)))
        (lcons "gen-id" (closure-gen-id z))))

(define/contract
  (case-lam->string z)
  (-> case-lam? (summaryof case-lam))
  (list "case-lam"
        (lcons "name"    (case-lam-name z))
        (lcons "clauses" (listof-zo->string lam->string (case-lam-clauses z)))))

(define/contract
  (let-one->string z)
  (-> let-one? (summaryof let-one))
  (list "let-one"
        (lcons "rhs"    (expr-seq-any->string (let-one-rhs  z)))
        (lcons "body"   (expr-seq-any->string (let-one-body z)))
        (lcons "type"   (let-one-type z))
        (lcons "unused" (let-one-unused? z))))

(define/contract
  (let-void->string z)
  (-> let-void? (summaryof let-void))
  (list "let-void"
        (lcons "count" (let-void-count z))
        (lcons "boxes" (let-void-boxes? z))
        (lcons "body"  (expr-seq-any->string (let-void-body z)))))

(define/contract
  (install-value->string z)
  (-> install-value? (summaryof install-value))
  (list "install-value"
        (lcons "count" (install-value-count z))
        (lcons "pos"   (install-value-pos z))
        (lcons "boxes" (install-value-boxes? z))
        (lcons "rhs"   (expr-seq-any->string (install-value-rhs z)))
        (lcons "body"  (expr-seq-any->string (install-value-body z)))))

(define/contract
  (let-rec->string z)
  (-> let-rec? (summaryof let-rec))
  (list "let-rec"
        (lcons "procs" (listof-zo->string lam->string (let-rec-procs z)))
        (lcons "body"  (expr-seq-any->string (let-rec-body z)))))

(define/contract
  (boxenv->string z)
  (-> boxenv? (summaryof boxenv))
  (list "boxenv"
        (lcons "pos"  (boxenv-pos z))
        (lcons "body" (expr-seq-any->string (boxenv-body z)))))

(define/contract
  (localref->string z)
  (-> localref? (summaryof localref))
  (list "localref"
        (lcons "unbox"        (localref-unbox? z))
        (lcons "pos"          (localref-pos z))
        (lcons "clear"        (localref-clear? z))
        (lcons "other-clears" (localref-other-clears? z))
        (lcons "type"         (localref-type z))))

(define/contract
  (toplevel->string z)
  (-> toplevel? (summaryof toplevel))
  (list
        "toplevel"
        (lcons "depth" (toplevel-depth z))
        (lcons "pos"   (toplevel-pos z))
        (lcons "const" (toplevel-const? z))
        (lcons "ready" (toplevel-ready? z))))

(define/contract
  (topsyntax->string z)
  (-> topsyntax? (summaryof topsyntax))
  (list "topsyntax"
        (lcons "depth" (topsyntax-depth z))
        (lcons "pos"   (topsyntax-pos z))
        (lcons "midpt" (topsyntax-midpt z))))

(define/contract
  (application->string z)
  (-> application? (summaryof application))
  (list "application"
        (lcons "rator" (expr-seq-any->string (application-rator z)))
        (lcons "rands" (map expr-seq-any->string (application-rands z)))))

(define/contract
  (branch->string z)
  (-> branch? (summaryof branch))
  (list "branch"
        (lcons "test" (expr-seq-any->string (branch-test z)))
        (lcons "then" (expr-seq-any->string (branch-then z)))
        (lcons "else" (expr-seq-any->string (branch-else z)))))

(define/contract
  (with-cont-mark->string z)
  (-> with-cont-mark? (summaryof with-cont-mark))
  (list "with-cont-mark"
        (lcons "key"  (expr-seq-any->string (with-cont-mark-key  z)))
        (lcons "val"  (expr-seq-any->string (with-cont-mark-val  z)))
        (lcons "body" (expr-seq-any->string (with-cont-mark-body z)))))

(define/contract
  (beg0->string z)
  (-> beg0? (summaryof beg0))
  (list "beg0"
        (lcons "seq" (map expr-seq-any->string (beg0-seq)))))

(define/contract
  (varref->string z)
  (-> varref? (summaryof varref))
  (list "varref"
        (lcons "toplevel" (let ([tl (varref-toplevel z)])
                            (cond [(toplevel? tl) (toplevel->string #f tl)]
                                  [else           tl])))
        (lcons "dummy"    (let ([dm (varref-dummy z)])
                            (cond [(toplevel? dm) (toplevel->string #f dm)]
                                  [else           dm])))))

(define/contract
  (assign->string z)
  (-> assign? (summaryof assign))
  (list "assign"
        (lcons "id"       (toplevel->string #f (assign-id z)))
        (lcons "rhs"      (expr-seq-any->string (assign-rhs z)))
        (lcons "undef-ok" (assign-undef-ok? z))))

(define/contract
  (apply-values->string z)
  (-> apply-values? (summaryof apply-values))
  (list "apply-values"
        (lcons "proc"      (expr-seq-any->string (apply-values-proc z)))
        (lcons "args-expr" (expr-seq-any->string (apply-values-args-expr z)))))

(define/contract
  (primval->string z)
  (-> primval? (summaryof primval))
  (list "primval"
        (lcons "id" (primval-id z))))

;; -- wrap

(define/contract
  (top-level-rename->string z)
  (-> top-level-rename? (summaryof top-level-rename))
  (list "top-level-rename"
        (lcons "flag" (top-level-rename z))))

(define/contract
  (mark-barrier->string z)
  (-> mark-barrier? (summaryof mark-barrier))
  (list "mark-barrier"
        (lcons "value" (mark-barrier-value z))))

(define/contract
  (lexical-rename->string z)
  (-> lexical-rename? (summaryof lexical-rename))
  (list "lexical-rename"
        (lcons "has-free-id-renames" (lexical-rename-has-free-id-renames? z))
        (lcons "bool2"               (lexical-rename-bool2 z))
        (lcons "alist"               (lexical-rename-alist->string (lexical-rename-alist z)))))

;; 2014-12-10: Ugly!!!
(define/contract
  (lexical-rename-alist->string alst)
  (-> (listof (cons/c symbol? (or/c symbol? (cons/c symbol? (or/c (cons/c symbol? (or/c symbol? #f)) free-id-info?))))) string?)
  (format-list #:sep " "
               (for/list ([a alst])
                 (format "(~a . ~a)"
                         (car a)
                         (cond [(symbol? (cdr a)) (cdr a)]
                               [else (let ([a* (cdr a)])
                                       (format "(~a . ~a)"
                                               (car a*)
                                               (cond [(free-id-info? (cdr a*)) (free-id-info->string #f (cdr a*))]
                                                     [else (cdr a*)])))])))))
  
(define/contract
  (phase-shift->string z)
  (-> phase-shift? (summaryof phase-shift))
  (list "phase-shift"
        (lcons "amt"       (phase-shift-amt z))
        (lcons "src"       (phase-shift-src z))
        (lcons "dest"      (phase-shift-dest z))
        (lcons "cancel-id" (phase-shift-cancel-id z))))

;; 2014-12-10: Curious about 'unmarshals'
(define/contract
  (module-rename->string z)
  (-> module-rename? (summaryof module-rename))
  (list "module-rename"
        (lcons "phase"        (module-rename-phase z))
        (lcons "kind"         (module-rename-kind z))
        (lcons "set-id"       (module-rename-set-id z))
        (lcons "unmarshals"   (listof-zo->string all-from-module->string (module-rename-unmarshals z)))
        (lcons "renames"      (module-rename-renames z));(listof-zo->string module-binding->string (module-rename-renames z)))
        (lcons "mark-renames" (module-rename-mark-renames z))
        (lcons "plus-kern"    (module-rename-plus-kern? z))))

(define/contract
  (wrap-mark->string z)
  (-> wrap-mark? (summaryof wrap-mark))
  (list "wrap-mark"
        (lcons "val" (wrap-mark-val z))))

(define/contract
  (prune->string z)
  (-> prune? (summaryof prune))
  (list "prune"
        (lcons "sym" (prune-sym z))))

;; -- module-binding

(define/contract
  (simple-module-binding->string z)
  (-> simple-module-binding? (summaryof simple-module-binding))
  (list "simple-module-binding"
        (lcons "path" (simple-module-binding-path z))))

(define/contract
  (phased-module-binding->string z)
  (-> phased-module-binding? (summaryof phased-module-binding))
  (list "phased-module-binding"
        (lcons "path"                (phased-module-binding-path z))
        (lcons "phase"               (phased-module-binding-phase z))
        (lcons "export-name"         (phased-module-binding-export-name z))
        (lcons "nominal-path"        (nominal-path->string #f (phased-module-binding-nominal-path z)))
        (lcons "nominal-export-name" (phased-module-binding-nominal-export-name z))))

(define/contract
  (exported-nominal-module-binding->string z)
  (-> exported-nominal-module-binding? (summaryof exported-nominal-module-binding))
  (list "exported-nominal-module-binding"
        (lcons "path"                (exported-nominal-module-binding-path z))
        (lcons "export-name"         (exported-nominal-module-binding-export-name z))
        (lcons "nominal-path"        (nominal-path->string #f (exported-nominal-module-binding-nominal-path z)))
        (lcons "nominal-export-name" (exported-nominal-module-binding-nominal-export-name z))))

(define/contract
  (nominal-module-binding->string z)
  (-> nominal-module-binding? (summaryof nominal-module-binding))
  (list "nominal-module-binding"
        (lcons "path"         (nominal-module-binding-path z))
        (lcons "nominal-path" (nominal-path->string (nominal-module-binding-nominal-path z)))))

(define/contract
  (exported-module-binding->string z)
  (-> exported-module-binding? (summaryof exported-module-binding))
  (list "exported-module-binding"
        (lcons "path"        (exported-module-binding-path z))
        (lcons "export-name" (exported-module-binding-export-name z))))

;; -- nominal-path

(define/contract
  (simple-nominal-path->string z)
  (-> simple-nominal-path? (summaryof simple-nominal-path))
  (list "simple-nominal-path"
        (lcons "value" (simple-nominal-path-value z))))

(define/contract
  (imported-nominal-path->string z)
  (-> imported-nominal-path? (summaryof imported-nominal-path))
  (list "imported-nominal-path"
        (lcons "value"        (imported-nominal-path-value z))
        (lcons "import-phase" (imported-nominal-path-import-phase z))))

(define/contract
  (phased-nominal-path->string z)
  (-> phased-nominal-path? (summaryof phased-nominal-path))
  (list "phased-nominal-path"
        (lcons "value"        (phased-nominal-path-value z))
        (lcons "import-phase" (phased-nominal-path-import-phase z))
        (lcons "phase"        (phased-nominal-path-phase z))))

;; -- helpers

(define/contract
  (any->string z)
  (-> any/c string?)
  (format "~a" z))

(define/contract
  (expr-seq-any->string z)
  (-> (or/c expr? seq? any/c) string?)
  (cond [(expr? z) (format-struct #f "expr")]
        [(seq?  z) (format-struct #f "seq")]
        [else      (any->string z)]))

(define/contract
  (form-or-any->string fm)
  (-> (or/c form? any/c) string?)
  (cond [(form? fm) (form->string #f fm)]
        [else       (any->string fm)]))

(define/contract
  (format-list xs #:sep [sep "\n"])
  (-> (listof string?) string?)
  (string-join xs sep))

(define/contract
  (format-struct deep? struct-spec)
  (-> boolean? summary? string?)
  (define fields (cdr struct-spec))
  (define title (format "<struct:~a>" (car struct-spec)))
  (define field-name-lengths
    (for/list ([fd fields]) (string-length (car fd))))
  (define w ;; width of longest struct field name
    (if (empty? fields) 0 (apply max field-name-lengths)))
  (if (not deep?) title
      (format-list (cons title
                         (for/list ([fd fields])
                           (let* ([forced ((cdr fd))]
                                  [rest   (if (summary? forced)
                                              (format-struct forced)
                                              forced)])
                             (format "  ~a : ~a" (car (pad (car fd) w)) rest)))))))

(define/contract
  (listof-form-or-any->string xs)
  (-> (listof (or/c form? any/c)) (listof string?))
  (for/list ([x xs])
    (form-or-any->string x)))

(define/contract
  (listof-zo->string z->str zs)
  (-> (-> zo? summary?) (listof zo?) string?)
  (cond [(empty? zs) "[]"]
        [else        (format "~a[~a]" (z->str #f (car zs)) (length zs))]))

(define string-with-size?
  (lambda (pair)
    (let ([str  (car pair)]
          [size (cdr pair)])
      (and (string?  str)
           (exact-positive-integer? size)
           (= size (string-length str))))))

;; If [str] has fewer than [w] characters, (w - (len str)) characters to its right end
(define/contract
  (pad str w #:char [c #\space])
  (-> string? natural-number/c string-with-size?)
  (define l (string-length str))
  (cond [(< l w) (let* ([diff (- w l)]
                        [str* (format "~a~a" str (make-string diff c))])
                   (cons str* w))]
        [else    (cons str l)]))

(define/contract
  (toplevel-or-any->string tl)
  (-> (or/c toplevel? any/c) string?)
  (cond [(toplevel? tl) (toplevel->string #f tl)]
        [else           (any->string tl)]))
