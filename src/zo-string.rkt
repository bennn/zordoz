#lang racket/base

(provide zo->string)

(require compiler/zo-structs
         racket/contract
         (only-in racket/string string-join)
         (only-in racket/list   empty?))

;; -- API functions

(define (zo->string z #:deep? [deep #t])
  ;; (-> zo? [#:deep boolean?] string?)
  (define str-spec
    (cond [(compilation-top? z) (compilation-top->string  deep z)]
          [(prefix?          z) (prefix->string           deep z)]
          [(global-bucket?   z) (global-bucket->string    deep z)]
          [(module-variable? z) (module-variable->string  deep z)]
          [(stx?             z) (stx->string              deep z)]
          [(form?            z) (form->string             deep z)]
          [(expr?            z) (expr->string             deep z)]
          [(wrapped?         z) (wrapped->string          deep z)]
          [(wrap?            z) (wrap->string             deep z)]
          [(free-id-info?    z) (free-id-info->string     deep z)]
          [(all-from-module? z) (all-from-module->string  deep z)]
          [(module-binding?  z) (module-binding->string   deep z)]
          [(nominal-path?    z) (nominal-path->string     deep z)]
          [(provided?        z) (provided->string         deep z)]
          [else (error (format "[zo->string] Unknown zo '~a'" z))]))
  (format-struct str-spec))

;; -- syntax: lazy cons to delay evaluation of tail

(require (for-syntax racket/base racket/syntax))
(define-syntax (lcons stx)
  (syntax-case stx ()
    [(_)       (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ _)     (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ hd tl) #'(cons hd (lambda () tl))]))

;; -- string specifications

;; beware, tails are not always strings (but probably should be).
(define (summaryof z)
  ;; (-> zo? (-> (cons/c boolean? (cons/c string? (listof (cons/c string? (-> string?)))) boolean?))
  (lambda (str-list)
    ;; A proper zo-spec should have the title and, if deep, a string for each struct field
    (and/c (list?  str-list)
           (< 1 (length str-list))
           (string? (cadr str-list))
           ;; Title matches struct name
           (and (= (length (cddr str-list)) (sub1 (vector-length (struct->vector z))))
                #t) ;; Each field name appears in a string
           )))

;; -- private functions

(define/contract
  (compilation-top->string deep? z)
  (-> boolean? compilation-top? (summaryof compilation-top))
  (list deep?
        "compilation-top"
        (lcons "max-let-depth" (compilation-top-max-let-depth z))
        (lcons "prefix"        (prefix->string #f (compilation-top-prefix z)))
        (lcons "code"          (form-or-any->string (compilation-top-code z)))))

(define/contract
  (prefix->string deep? z)
  (-> boolean? prefix? (summaryof prefix))
  (list deep?
        "prefix"
        (lcons "num-lifts" (prefix-num-lifts z))
        (lcons "toplevels" (prefix-toplevels->string (prefix-toplevels z)))
        (lcons "stxs"      (listof-zo->string stx->string (prefix-stxs z)))))

(define (prefix-toplevels->string tls)
  ;; (-> (listof (or/c module-variable? global-bucket? any/c)) (listof string?))
  (for/list ([tl tls])
    (cond [(module-variable? tl) (module-variable->string #f tl)]
          [(global-bucket?   tl) (global-bucket->string #f tl)]
          [else (format "~a" tl)])))

(define (global-bucket->string deep? z)
  ;; (->  boolean? global-bucket? (summaryof global-bucket))
  (list deep?
        "global-bucket"
        (lcons "name" (global-bucket-name z))))

                                        ; 2014-12-10: May want to print 'constantness' nicer. Define struct-shape->string.
(define (module-variable->string deep? z)
  ;; (-> boolean? module-variable? (summaryof module-variable))
  (list deep?
        "module-variable"
        (lcons "modidx"       (module-variable-modidx z))
        (lcons "sym"          (module-variable-sym z))
        (lcons "pos"          (module-variable-pos z))
        (lcons "phase"        (module-variable-phase z))
        (lcons "constantness" (module-variable-constantness z))))

(define (stx->string deep? z)
  ;; (-> boolean? stx? (summaryof stx))
  (list deep?
        "stx"
        (lcons "encoded" (wrapped->string #f (stx-encoded z)))))

(define (form->string deep? z)
  ;; (-> boolean? form? (summaryof form))
  (cond [(def-values?     z) (def-values->string     deep? z)]
        [(def-syntaxes?   z) (def-syntaxes->string   deep? z)]
        [(seq-for-syntax? z) (seq-for-syntax->string deep? z)]
        [(req?            z) (req->string            deep? z)]
        [(seq?            z) (seq->string            deep? z)]
        [(splice?         z) (splice->string         deep? z)]
        [(inline-variant? z) (inline-variant->string deep? z)]
        [(mod?            z) (mod->string            deep? z)]
        [(provided?       z) (provided->string       deep? z)]
        [(expr?           z) (expr->string           deep? z)]
        [else (error (format "[form->string] Unknown form '~a'" z))]))

(define (expr->string deep? z)
  ;; (-> boolean? expr? (summaryof expr))
  (cond [(lam?            z) (lam->string            deep? z)]
        [(closure?        z) (closure->string        deep? z)]
        [(case-lam?       z) (case-lam->string       deep? z)]
        [(let-one?        z) (let-one->string        deep? z)]
        [(let-void?       z) (let-void->string       deep? z)]
        [(install-value?  z) (install-value->string  deep? z)]
        [(let-rec?        z) (let-rec->string        deep? z)]
        [(boxenv?         z) (boxenv->string         deep? z)]
        [(localref?       z) (localref->string       deep? z)]
        [(toplevel?       z) (toplevel->string       deep? z)]
        [(topsyntax?      z) (topsyntax->string      deep? z)]
        [(application?    z) (application->string    deep? z)]
        [(branch?         z) (branch->string         deep? z)]
        [(with-cont-mark? z) (with-cont-mark->string deep? z)]
        [(beg0?           z) (beg0->string           deep? z)]
        [(varref?         z) (varref->string         deep? z)]
        [(assign?         z) (assign->string         deep? z)]
        [(apply-values?   z) (apply-values->string   deep? z)]
        [(primval?        z) (primval->string        deep? z)]
        [else (error (format "[expr->string] Unknown expr '~a'" z))]))

(define (wrapped->string deep? z)
  ;; (-> boolean? wrapped? (summaryof wrapped))
  (list deep?
        "wrapped"
        (lcons "datum"         (wrapped-datum z))
        (lcons "wraps"         (listof-zo->string wrap->string (wrapped-wraps z)))
        (lcons "tamper-status" (wrapped-tamper-status z))))

(define (wrap->string deep? z)
  ;; (-> boolean? wrap? (summaryof wrap))
  (cond [(top-level-rename? z) (top-level-rename->string deep? z)]
        [(mark-barrier?     z) (mark-barrier->string     deep? z)]
        [(lexical-rename?   z) (lexical-rename->string   deep? z)]
        [(phase-shift?      z) (phase-shift->string      deep? z)]
        [(module-rename?    z) (module-rename->string    deep? z)]
        [(wrap-mark?        z) (wrap-mark->string        deep? z)]
        [(prune?            z) (prune->string            deep? z)]
        [else (error (format "[wrap->string] Unknown wrap '~a'" z))]))

(define (free-id-info->string deep? z)
  ;; (-> boolean? free-id-info? (summaryof free-id-info))
  (list deep?
        "free-id-info"
        (lcons "path0"                 (free-id-info-path0 z))
        (lcons "symbol0"               (free-id-info-symbol0 z))
        (lcons "path1"                 (free-id-info-path1 z))
        (lcons "symbol1"               (free-id-info-symbol1 z))
        (lcons "phase0"                (free-id-info-phase0 z))
        (lcons "phase1"                (free-id-info-phase1 z))
        (lcons "phase2"                (free-id-info-phase2 z))
        (lcons "use-current-inspector" (free-id-info-use-current-inspector? z))))

(define (all-from-module->string deep? z)
  ;; (-> boolean? all-from-module? (summaryof all-from-module))
  (list deep?
        "all-from-module"
        (lcons "path"      (all-from-module-path z))
        (lcons "phase"     (all-from-module-phase z))
        (lcons "src-phase" (all-from-module-src-phase z))
        (lcons "exception" (all-from-module-exceptions z))
        (lcons "prefix"    (all-from-module-prefix z))
        (lcons "context"   (all-from-module-context z))))

(define (module-binding->string deep? z)
  ;; (-> boolean? module-binding? (summaryof module-binding))
  (cond [(simple-module-binding?           z) (simple-module-binding->string           deep? z)]
        [(phased-module-binding?           z) (phased-module-binding->string           deep? z)]
        [(exported-nominal-module-binding? z) (exported-nominal-module-binding->string deep? z)]
        [(nominal-module-binding?          z) (nominal-module-binding->string          deep? z)]
        [(exported-module-binding?         z) (exported-module-binding->string         deep? z)]
        [else (error (format "[module-binding->string] Unknown '~a'" z))]))

(define (nominal-path->string deep? z)
  ;; (-> boolean? nominal-path? (summaryof nominal-path))
  (cond [(simple-nominal-path?   z) (simple-nominal-path->string   deep? z)]
        [(imported-nominal-path? z) (imported-nominal-path->string deep? z)]
        [(phased-nominal-path?   z) (phased-nominal-path->string   deep? z)]
        [else (error (format "[nominal-path] Unknown '~a'" z))]))

;; -- form

(define (def-values->string deep? z)
  ;; (-> boolean? def-values? (summaryof def-values))
  (list deep?
        "def-values"
        (lcons "ids" (listof-zo->string def-values->string (def-values-ids z)))
        (lcons "rhs" (let ([rhs (def-values-rhs z)])
                       (cond [(expr? rhs) (expr->string #f rhs)]
                             [(seq?  rhs) (seq->string #f rhs)]
                             [(inline-variant? rhs) (inline-variant->string #f rhs)]
                             [else                  rhs])))))

(define (def-syntaxes->string deep? z)
  ;; (-> boolean? def-syntaxes? (summaryof def-syntaxes))
  (list deep?
        "def-syntaxes"
        (lcons "ids"           (def-syntaxes-ids z))
        (lcons "rhs"           (expr-seq-any->string (def-syntaxes-rhs z)))
        (lcons "prefix"        (prefix->string #f (def-syntaxes-prefix z)))
        (lcons "max-let-depth" (def-syntaxes-max-let-depth z))
        (lcons "dummy"         (toplevel-or-any->string (def-syntaxes-dummy z)))))

(define (seq-for-syntax->string deep? z)
  ;; (-> boolean? seq-for-syntax? (summaryof seq-for-syntax))
  (list deep?
        "seq-for-syntax"
        (lcons "forms"         (listof-form-or-any->string (seq-for-syntax-forms z)))
        (lcons "prefix"        (prefix->string #f (seq-for-syntax-prefix z)))
        (lcons "max-let-depth" (seq-for-syntax-max-let-depth z))
        (lcons "dummy"         (toplevel-or-any->string (seq-for-syntax-dummy z)))))

(define (req->string deep? z)
  ;; (-> boolean? req? (summaryof req))
  (list deep?
        "req"
        (lcons "reqs"  (stx->string #f (req-reqs z)))
        (lcons "dummy" (toplevel->string #f (req-dummy z)))))

(define (seq->string deep? z)
  ;; (-> boolean? seq? (summaryof seq))
  (list deep?
        "seq"
        (lcons "forms" (listof-form-or-any->string (seq-forms z)))))

(define (splice->string deep? z)
  ;; (-> boolean? splice? (summaryof splice))
  (list deep?
        "splice"
        (lcons "forms" (listof-form-or-any->string (splice-forms z)))))

(define (inline-variant->string deep? z)
  ;; (-> boolean? inline-variant? (summaryof inline-variant))
  (list deep?
        "inline-variant"
        (lcons "direct" (expr->string #f (inline-variant-direct z)))
        (lcons "inline" (expr->string #f (inline-variant-inline z)))))

(define/contract (mod->string deep? z)
  (-> boolean? mod? (summaryof mod))
  (list deep?
        "mod"
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

(define (mod-provides->string pds)
  ;; (-> (listof (list/c (or/c exact-integer? #f) (listof provided?) (listof provided?))) string?)
  (format-list #:sep " "
               (for/list ([pd pds])
                 (format "(~a ~a ~a)"
                         (car pd) ; (or/c exact-integer? #f)
                         (format "~a" (listof-zo->string provided->string (cadr pd)))
                         (format "~a" (listof-zo->string provided->string (caddr pd)))))))

(define (mod-requires->string rqs)
  ;; (-> (listof (cons/c (or/c exact-integer #f) (listof module-path-index?))) string?)
  (format-list #:sep " "
               (for/list ([rq rqs])
                 (format "(~a ~a)" (car rq) (cdr rq)))))

;; 2014-12-10: Ugly
(define (mod-syntax-bodies->string sbs)
  ;; (-> (listof (cons/c exact-positive-integer (listof (or/c def-syntaxes? seq-for-syntax?)))) string?)
  (format-list #:sep " "
               (for/list ([sb sbs])
                 (format "(~a . ~a)"
                         (car sb)
                         (for/list ([d (cdr sb)])
                                (cond [(def-syntaxes?   d) (def-syntaxes->string #f d)]
                                      [(seq-for-syntax? d) (seq-for-syntax->string #f d)]
                                      [else (error "[mod-syntax-bodies->string] Unexpected arg")]))))))
                
(define (provided->string deep? z)
  ;; (-> boolean? provided? (summaryof provided))
  (list deep?
        "provided"
        (lcons "name"      (provided-name z))
        (lcons "src"       (provided-src  z))
        (lcons "src-name"  (provided-src-name z))
        (lcons "nom-src"   (provided-nom-src z))
        (lcons "src-phase" (provided-src-phase z))
        (lcons "protected" (provided-protected? z))))

;; -- expr

(define (lam->string deep? z)
  ;; (-> boolean? lam? (summaryof lam))
  (list deep?
        "lam"
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

(define (closure->string deep? z)
  ;; (-> boolean? closure? (summaryof closure))
  (list deep?
        "closure"
        (lcons "code"   (lam->string #f (closure-code z)))
        (lcons "gen-id" (closure-gen-id z))))

(define (case-lam->string deep? z)
  ;; (-> boolean? case-lam? (summaryof case-lam))
  (list deep?
        "case-lam"
        (lcons "name"    (case-lam-name z))
        (lcons "clauses" (listof-zo->string lam->string (case-lam-clauses z)))))

(define (let-one->string deep? z)
  ;; (-> boolean? let-one? (summaryof let-one))
  (list deep?
        "let-one"
        (lcons "rhs"    (expr-seq-any->string (let-one-rhs  z)))
        (lcons "body"   (expr-seq-any->string (let-one-body z)))
        (lcons "type"   (let-one-type z))
        (lcons "unused" (let-one-unused? z))))

(define (let-void->string deep? z)
  ;; (-> boolean? let-void? (summaryof let-void))
  (list deep?
        "let-void"
        (lcons "count" (let-void-count z))
        (lcons "boxes" (let-void-boxes? z))
        (lcons "body"  (expr-seq-any->string (let-void-body z)))))

(define (install-value->string deep? z)
  ;; (-> boolean? install-value? (summaryof install-value))
  (list deep?
        "install-value"
        (lcons "count" (install-value-count z))
        (lcons "pos"   (install-value-pos z))
        (lcons "boxes" (install-value-boxes? z))
        (lcons "rhs"   (expr-seq-any->string (install-value-rhs z)))
        (lcons "body"  (expr-seq-any->string (install-value-body z)))))

(define (let-rec->string deep? z)
  ;; (-> boolean? let-rec? (summaryof let-rec))
  (list deep?
        "let-rec"
        (lcons "procs" (listof-zo->string lam->string (let-rec-procs z)))
        (lcons "body"  (expr-seq-any->string (let-rec-body z)))))

(define (boxenv->string deep? z)
  ;; (-> boolean? boxenv? (summaryof boxenv))
  (list deep?
        "boxenv"
        (lcons "pos"  (boxenv-pos z))
        (lcons "body" (expr-seq-any->string (boxenv-body z)))))

(define (localref->string deep? z)
  ;; (-> boolean? localref? (summaryof localref))
  (list deep?
        "localref"
        (lcons "unbox"        (localref-unbox? z))
        (lcons "pos"          (localref-pos z))
        (lcons "clear"        (localref-clear? z))
        (lcons "other-clears" (localref-other-clears? z))
        (lcons "type"         (localref-type z))))

(define (toplevel->string deep? z)
  ;; (-> boolean? toplevel? (summaryof toplevel))
  (list deep?
        "toplevel"
        (lcons "depth" (toplevel-depth z))
        (lcons "pos"   (toplevel-pos z))
        (lcons "const" (toplevel-const? z))
        (lcons "ready" (toplevel-ready? z))))

(define (topsyntax->string deep? z)
  ;; (-> boolean? topsyntax? (summaryof topsyntax))
  (list deep?
        "topsyntax"
        (lcons "depth" (topsyntax-depth z))
        (lcons "pos"   (topsyntax-pos z))
        (lcons "midpt" (topsyntax-midpt z))))

(define (application->string deep? z)
  ;; (-> boolean? application? (summaryof application))
  (list deep?
        "application"
        (lcons "rator" (expr-seq-any->string (application-rator z)))
        (lcons "rands" (map expr-seq-any->string (application-rands z)))))

(define (branch->string deep? z)
  ;; (->  boolean? branch? (summaryof branch))
  (list deep?
        "branch"
        (lcons "test" (expr-seq-any->string (branch-test z)))
        (lcons "then" (expr-seq-any->string (branch-then z)))
        (lcons "else" (expr-seq-any->string (branch-else z)))))

(define (with-cont-mark->string deep? z)
  ;; (-> boolean? with-cont-mark? (summaryof with-cont-mark))
  (list deep?
        "with-cont-mark"
        (lcons "key"  (expr-seq-any->string (with-cont-mark-key  z)))
        (lcons "val"  (expr-seq-any->string (with-cont-mark-val  z)))
        (lcons "body" (expr-seq-any->string (with-cont-mark-body z)))))

(define (beg0->string deep? z)
  ;; (-> boolean? beg0? (summaryof beg0))
  (list deep?
        "beg0"
        (lcons "seq" (map expr-seq-any->string (beg0-seq)))))

(define (varref->string deep? z)
  ;; (-> boolean? varref? (summaryof varref))
  (list deep?
        "varref"
        (lcons "toplevel" (let ([tl (varref-toplevel z)])
                            (cond [(toplevel? tl) (toplevel->string #f tl)]
                                  [else           tl])))
        (lcons "dummy"    (let ([dm (varref-dummy z)])
                            (cond [(toplevel? dm) (toplevel->string #f dm)]
                                  [else           dm])))))

(define (assign->string deep? z)
  ;; (-> boolean? assign? (summaryof assign))
  (list deep?
        "assign"
        (lcons "id"       (toplevel->string #f (assign-id z)))
        (lcons "rhs"      (expr-seq-any->string (assign-rhs z)))
        (lcons "undef-ok" (assign-undef-ok? z))))

(define (apply-values->string deep? z)
  ;; (-> boolean? apply-values? (summaryof apply-values))
  (list deep?
        "apply-values"
        (lcons "proc"      (expr-seq-any->string (apply-values-proc z)))
        (lcons "args-expr" (expr-seq-any->string (apply-values-args-expr z)))))

(define (primval->string deep? z)
  ;; (-> boolean? primval? (summaryof primval))
  (list deep?
        "primval"
        (lcons "id" (primval-id z))))

;; -- wrap

(define (top-level-rename->string deep? z)
  ;; (-> boolean? top-level-rename? (summaryof top-level-rename))
  (list deep?
        "top-level-rename"
        (lcons "flag" (top-level-rename z))))

(define (mark-barrier->string deep? z)
  ;; (-> boolean? mark-barrier? (summaryof mark-barrier))
  (list deep?
        "mark-barrier"
        (lcons "value" (mark-barrier-value z))))

(define (lexical-rename->string deep? z)
  ;; (-> boolean? lexical-rename? (summaryof lexical-rename))
  (list deep?
        "lexical-rename"
        (lcons "has-free-id-renames" (lexical-rename-has-free-id-renames? z))
        (lcons "bool2"               (lexical-rename-bool2 z))
        (lcons "alist"               (lexical-rename-alist->string (lexical-rename-alist z)))))

;; 2014-12-10: Ugly!!!
(define (lexical-rename-alist->string alst)
  ;; (-> (listof (cons/c symbol? (or/c symbol? (cons/c symbol? (or/c (cons/c symbol? (or/c symbol? #f)) free-id-info?))))) string?)
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
  
(define (phase-shift->string deep? z)
  ;; (-> boolean? phase-shift? (summaryof phase-shift))
  (list deep?
        "phase-shift"
        (lcons "amt"       (phase-shift-amt z))
        (lcons "src"       (phase-shift-src z))
        (lcons "dest"      (phase-shift-dest z))
        (lcons "cancel-id" (phase-shift-cancel-id z))))

;; 2014-12-10: Curious about 'unmarshals'
(define (module-rename->string deep? z)
  ;; (-> boolean? module-rename? (summaryof module-rename))
  (list deep?
        "module-rename"
        (lcons "phase"        (module-rename-phase z))
        (lcons "kind"         (module-rename-kind z))
        (lcons "set-id"       (module-rename-set-id z))
        (lcons "unmarshals"   (listof-zo->string all-from-module->string (module-rename-unmarshals z)))
        (lcons "renames"      (module-rename-renames z));(listof-zo->string module-binding->string (module-rename-renames z)))
        (lcons "mark-renames" (module-rename-mark-renames z))
        (lcons "plus-kern"    (module-rename-plus-kern? z))))

(define (wrap-mark->string deep? z)
  ;; (-> boolean? wrap-mark? (summaryof wrap-mark))
  (list deep?
        "wrap-mark"
        (lcons "val" (wrap-mark-val z))))

(define (prune->string deep? z)
  ;; (-> boolean? prune? (summaryof prune))
  (list deep?
        "prune"
        (lcons "sym" (prune-sym z))))

;; -- module-binding

(define (simple-module-binding->string deep? z)
  ;; (-> boolean? simple-module-binding? (summaryof simple-module-binding))
  (list deep?
        "simple-module-binding"
        (lcons "path" (simple-module-binding-path z))))

(define (phased-module-binding->string deep? z)
  ;; (-> boolean? phased-module-binding? (summaryof phased-module-binding))
  (list deep?
        "phased-module-binding"
        (lcons "path"                (phased-module-binding-path z))
        (lcons "phase"               (phased-module-binding-phase z))
        (lcons "export-name"         (phased-module-binding-export-name z))
        (lcons "nominal-path"        (nominal-path->string #f (phased-module-binding-nominal-path z)))
        (lcons "nominal-export-name" (phased-module-binding-nominal-export-name z))))

(define (exported-nominal-module-binding->string deep? z)
  ;; (-> boolean? exported-nominal-module-binding? (summaryof exported-nominal-module-binding))
  (list deep?
        "exported-nominal-module-binding"
        (lcons "path"                (exported-nominal-module-binding-path z))
        (lcons "export-name"         (exported-nominal-module-binding-export-name z))
        (lcons "nominal-path"        (nominal-path->string #f (exported-nominal-module-binding-nominal-path z)))
        (lcons "nominal-export-name" (exported-nominal-module-binding-nominal-export-name z))))

(define (nominal-module-binding->string deep? z)
  ;; (-> boolean? nominal-module-binding? (summaryof nominal-module-binding))
  (list deep?
        "nominal-module-binding"
        (lcons "path"         (nominal-module-binding-path z))
        (lcons "nominal-path" (nominal-path->string (nominal-module-binding-nominal-path z)))))

(define (exported-module-binding->string deep? z)
  ;; (-> boolean? exported-module-binding? (summaryof exported-module-binding))
  (list deep?
        "exported-module-binding"
        (lcons "path"        (exported-module-binding-path z))
        (lcons "export-name" (exported-module-binding-export-name z))))

;; -- nominal-path

(define (simple-nominal-path->string deep? z)
  ;; (-> boolean? simple-nominal-path? (summaryof simple-nominal-path))
  (list deep?
        "simple-nominal-path"
        (lcons "value" (simple-nominal-path-value z))))

(define (imported-nominal-path->string deep? z)
  ;; (-> boolean? imported-nominal-path? (summaryof imported-nominal-path))
  (list deep?
        "imported-nominal-path"
        (lcons "value"        (imported-nominal-path-value z))
        (lcons "import-phase" (imported-nominal-path-import-phase z))))

(define (phased-nominal-path->string deep? z)
  ;; (-> boolean? phased-nominal-path? (summaryof phased-nominal-path))
  (list deep?
        "phased-nominal-path"
        (lcons "value"        (phased-nominal-path-value z))
        (lcons "import-phase" (phased-nominal-path-import-phase z))
        (lcons "phase"        (phased-nominal-path-phase z))))

;; -- helpers

(define (any->string z)
  ;; (-> any/c string?)
  (format "~a" z))

(define (expr-seq-any->string z)
  ;; (-> (or/c expr? seq? any/c) string?)
  (cond [(expr? z) (format-struct #f "expr")]
        [(seq?  z) (format-struct #f "seq")]
        [else      (any->string z)]))

(define (form-or-any->string fm)
  (cond [(form? fm) (form->string #f fm)]
        [else       (any->string fm)]))

(define (format-list xs #:sep [sep "\n"])
  ;; (-> (listof string?) string?)
  (string-join xs sep))

(define (summary? xs)
  (and (list? xs)
       (< 1 (length xs))
       (boolean? (car xs))
       (string?  (cadr xs))))

(define (format-struct struct-spec)
  ;; (->* (cons/c boolean (cons/c string? (listof string?))) string?)
  (define deep? (car struct-spec))
  (define title (cadr struct-spec))
  (define fields (cddr struct-spec))
  (define title-str (format "<struct:~a>" title))
  (define field-name-lengths
    (for/list ([fd fields]) (string-length (car fd))))
  (define w ;; width of longest struct field name
    (if (empty? fields) 0 (apply max field-name-lengths)))
  (if (not deep?) title-str
      (format-list (cons title-str
                         (for/list ([fd fields])
                           (let* ([forced ((cdr fd))]
                                  [rest   (if (summary? forced) (format-struct forced) forced)])
                             (format "  ~a : ~a" (pad (car fd) w) rest)))))))

(define (listof-form-or-any->string xs)
  ;; (-> (listof (or/c form? any/c)) (listof string?))
  (for/list ([x xs])
    (form-or-any->string x)))

(define (listof-zo->string z->str zs)
  ;; (-> (-> boolean? zo? string?) (listof zo?) string?)
  (cond [(empty? zs) "[]"]
        [else        (format "~a[~a]" (z->str #f (car zs)) (length zs))]))

;; If [str] has fewer than [w] characters, (w - (len str)) characters to its right end
(define (pad str w #:char [c #\space])
  ;; (-> natural-number/c string? string?)
  (define l (string-length str))
  (cond [(< l w) (let ([diff (- w l)])
                   (format "~a~a" str (make-string diff c)))]
        [else    str]))

(define (toplevel-or-any->string tl)
  ;; (-> (or/c toplevel? any/c) string?)
  (cond [(toplevel? tl) (toplevel->string #f tl)]
        [else           (any->string tl)]))
