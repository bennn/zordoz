#lang racket/base

(provide zo->string)

(require compiler/zo-structs
         (only-in racket/string string-join)
         (only-in racket/list   empty?))

;; -- API functions

(define (zo->string z #:deep? [deep #t])
  ;; (-> zo? [#:deep boolean?] string?)
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
        [else "?zo"]))

;; -- private functions

(define (compilation-top->string deep? z)
  ;; (-> boolean? compilation-top? string?)
  (if (not deep?)
      "<struct:compilation-top>"
      (format-list (list "compilation-top"
                         (format "  max-let-depth : ~a" (compilation-top-max-let-depth z))
                         (format "  prefix        : ~a" (prefix->string #f (compilation-top-prefix z)))
                         (format "  code          : ~a" (let ([fm (compilation-top-code z)])
                                                          (cond [(form? fm) (form->string #f fm)]
                                                                [else (any->string)])))))))

(define (prefix->string deep? z)
  ;; (-> boolean? prefix? string?)
  (if (not deep?)
      "<struct:prefix>"
      (format-list (list "prefix"
                         (format "  num-lifts : ~a" (prefix-num-lifts z))
                         (format "  toplevels : ~a" (prefix-toplevels->string (prefix-toplevels z)))
                         (format "  stxs      : ~a" (listof-zo->string stx->string (prefix-stxs z)))))))

(define (prefix-toplevels->string tls)
  ;; (-> (listof (or/c module-variable? global-bucket? any/c)) (listof string?))
  (for/list ([tl tls])
    (cond [(module-variable? tl) (module-variable->string #f tl)]
          [(global-bucket?   tl) (global-bucket->string #f tl)]
          [else (format "~a" tl)])))


(define (global-bucket->string deep? z)
  ;; (->  boolean? global-bucket? string?)
  (if (not deep?)
      "<struct:global-bucket>"
      (format-list (list "global-bucket"
                         (format "  name : ~a" (global-bucket-name z))))))

; 2014-12-10: May want to print 'constantness' nicer. Define struct-shape->string.
(define (module-variable->string deep? z)
  ;; (-> boolean? module-variable? string?)
  (if (not deep?)
      "<struct:module-variable>"
      (format-list (list "module-variable"
                         (format "  modidx       : ~a" (module-variable-modidx z))
                         (format "  sym          : ~a" (module-variable-sym z))
                         (format "  pos          : ~a" (module-variable-pos z))
                         (format "  phase        : ~a" (module-variable-phase z))
                         (format "  constantness : ~a" (module-variable-constantness z))))))

(define (stx->string deep? z)
  ;; (-> boolean? stx? string?)
  (if (not deep?)
      "<struct:stx>"
      (format-list (list "stx"
                         (format "  encoded : ~a" (wrapped->string #f (stx-encoded z)))))))

(define (form->string deep? z)
  ;; (-> boolean? form? string?)
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
        [else "?form"]))

(define (expr->string deep? z)
  ;; (-> boolean? expr? string?)
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
        [else "?expr"]))

(define (wrapped->string deep? z)
  ;; (-> boolean? wrapped? string?)
  (if (not deep?)
      "<struct:wrapped>"
      (format-list (list "wrapped"
                         (format "  datum         : ~a" (wrapped-datum z))
                         (format "  wraps         : ~a" (listof-zo->string wrap->string (wrapped-wraps z)))
                         (format "  tamper-status : ~a" (wrapped-tamper-status z))))))

(define (wrap->string deep? z)
  ;; (-> boolean? wrap? string?)
  (cond [(top-level-rename? z) (top-level-rename->string deep? z)]
        [(mark-barrier?     z) (mark-barrier->string     deep? z)]
        [(lexical-rename?   z) (lexical-rename->string   deep? z)]
        [(phase-shift?      z) (phase-shift->string      deep? z)]
        [(module-rename?    z) (module-rename->string    deep? z)]
        [else "?wrap"]))

(define (free-id-info->string deep? z)
  ;; (-> boolean? free-id-info? string?)
  (if (not deep?)
      "<struct:free-id-info>"
      (format-list (list "free-id-info"
                         (format "  path0                  : ~a" (free-id-info-path0 z))
                         (format "  symbol0                : ~a" (free-id-info-symbol0 z))
                         (format "  path1                  : ~a" (free-id-info-path1 z))
                         (format "  symbol1                : ~a" (free-id-info-symbol1 z))
                         (format "  phase0                 : ~a" (free-id-info-phase0 z))
                         (format "  phase1                 : ~a" (free-id-info-phase1 z))
                         (format "  phase2                 : ~a" (free-id-info-phase2 z))
                         (format "  use-current-inspector? : ~a" (free-id-info-use-current-inspector? z))))))

(define (all-from-module->string deep? z)
  ;; (-> boolean? all-from-module? string?)
  (if (not deep?)
      "<struct:all-from-module>"
      (format-list (list "all-from-module"
                         (format "  path      : ~a" (all-from-module-path z))
                         (format "  phase     : ~a" (all-from-module-phase z))
                         (format "  src-phase : ~a" (all-from-module-src-phase z))
                         (format "  exception : ~a" (all-from-module-exceptions z))
                         (format "  prefix    : ~a" (all-from-module-prefix z))
                         (format "  context   : ~a" (all-from-module-context z))))))

(define (module-binding->string deep? z)
  ;; (-> boolean? module-binding? string?)
  (cond [(simple-module-binding?           z) (simple-module-binding->string           deep? z)]
        [(phased-module-binding?           z) (phased-module-binding->string           deep? z)]
        [(exported-nominal-module-binding? z) (exported-nominal-module-binding->string deep? z)]
        [(nominal-module-binding?          z) (nominal-module-binding->string          deep? z)]
        [(exported-module-binding?         z) (exported-module-binding->string         deep? z)]
        [else "?module-binding"]))


(define (nominal-path->string deep? z)
  ;; (-> boolean? nominal-path? string?)
  (cond [(simple-nominal-path?   z) (simple-nominal-path->string   deep? z)]
        [(imported-nominal-path? z) (imported-nominal-path->string deep? z)]
        [(phased-nominal-path?   z) (phased-nominal-path->string   deep? z)]
        [else "?nominal-path"]))

;; -- form

(define (def-values->string deep? z)
  ;; (-> boolean? def-values? string?)
  (if (not deep?)
      "<struct:def-values>"
      (format-list (list "def-values"
                         (format "  ids : ~a" (listof-zo->string def-values->string (def-values-ids z)))
                         (format "  rhs : ~a" (let ([rhs (def-values-rhs z)])
                                                (cond [(expr? rhs) (expr->string #f rhs)]
                                                      [(seq?  rhs) (seq->string #f rhs)]
                                                      [(inline-variant? rhs) (inline-variant->string #f rhs)]
                                                      [else (any->string)])))))))

(define (def-syntaxes->string deep? z)
  ;; (-> boolean? def-syntaxes? string?)
  (if (not deep?)
      "<struct:def-syntaxes>"
      (format-list (list "def-syntaxes"
                         (format "  ids           : ~a" (def-syntaxes-ids z))
                         (format "  rhs           : ~a" (expr-seq-any->string (def-syntaxes-rhs z)))
                         (format "  prefix        : ~a" (prefix->string #f (def-syntaxes-prefix z)))
                         (format "  max-let-depth : ~a" (def-syntaxes-max-let-depth z))
                         (format "  dummy         : ~a" (let ([tl (def-syntaxes-dummy z)])
                                                          (cond [(toplevel? tl) (toplevel->string #f tl)]
                                                                [else           (any->string)])))))))

(define (seq-for-syntax->string deep? z)
  ;; (-> boolean? seq-for-syntax? string?)
  (if (not deep?)
      "<struct:seq-for-syntax>"
      (format-list (list "seq-for-syntax"
                         (format "  forms         : ~a" (listof-form-or-any->string (seq-for-syntax-forms z)))
                         (format "  prefix        : ~a" (prefix->string #f (seq-for-syntax-prefix z)))
                         (format "  max-let-depth : ~a" (seq-for-syntax-max-let-depth z))
                         (format "  dummy         : ~a" (let ([dm (seq-for-syntax-dummy z)])
                                                          (cond [(toplevel? dm) (toplevel->string #f dm)]
                                                                [else           (any->string)])))))))

(define (req->string deep? z)
  ;; (-> boolean? req? string?)
  (if (not deep?)
      "<struct:req>"
      (format-list (list "req"
                         (format "  reqs  : ~a" (stx->string #f (req-reqs z)))
                         (format "  dummy : ~a" (toplevel->string #f (req-dummy z)))))))

(define (seq->string deep? z)
  ;; (-> boolean? seq? string?)
  (if (not deep?)
      "<struct:seq>"
      (format-list (list "seq"
                         (format "  forms : ~a" (listof-form-or-any->string (seq-forms z)))))))

(define (splice->string deep? z)
  ;; (-> boolean? splice? string?)
  (if (not deep?)
      "<struct:splice>"
      (format-list (list "splice"
                         (format "  forms : ~a" (listof-form-or-any->string (splice-forms z)))))))

(define (inline-variant->string deep? z)
  ;; (-> boolean? inline-variant? string?)
  (if (not deep?)
      "<struct:inline-variant>"
      (format-list (list "inline-variant"
                         (format "  direct : ~a" (expr->string #f (inline-variant-direct z)))
                         (format "  inline : ~a" (expr->string #f (inline-variant-inline z)))))))

(define (mod->string deep? z)
  ;; (-> boolean? mod? string?)
  (if (not deep?)
      "<struct:mod>"
      (format-list (list "mod"
                         (format "  name             : ~a" (mod-name z))
                         (format "  srcname          : ~a" (mod-srcname z))
                         (format "  self-modidx      : ~a" (mod-self-modidx z))
                         (format "  prefix           : ~a" (prefix->string #f (mod-prefix z)))
                         (format "  provides         : ~a" (mod-provides->string (mod-provides z)))
                         (format "  requires         : ~a" (mod-requires->string (mod-requires z)))
                         (format "  body             : ~a" (listof-form-or-any->string (mod-body z)))
                         (format "  syntax-bodies    : ~a" (mod-syntax-bodies->string (mod-syntax-bodies z)))
                         (format "  unexported       : ~a" (mod-unexported z))
                         (format "  max-let-depth    : ~a" (mod-max-let-depth z))
                         (format "  dummy            : ~a" (toplevel->string #f (mod-dummy z)))
                         (format "  lang-info        : ~a" (mod-lang-info z))
                         (format "  internal-context : ~a" (let ([ic (mod-internal-context z)])
                                                             (cond [(stx? ic)    (stx->string #f ic)]
                                                                   [(vector? ic) (listof-zo->string stx->string (vector->list ic))]
                                                                   [else         ic])))
                         (format "  flags            : ~a" (mod-flags z))
                         (format "  pre-submodules   : ~a" (listof-zo->string mod->string (mod-pre-submodules z)))
                         (format "  post-submodules  : ~a" (listof-zo->string mod->string (mod-post-submodules z)))))))

(define (mod-provides->string pds)
  ;; (-> (listof (list/c (or/c exact-integer? #f) (listof provided?) (listof provided?))) string?)
  (format-list #:sep " "
               (for/list ([pd pds])
                 (format "(~a ~a ~a)"
                         (car pd) ; (or/c exact-integer? #f)
                         (format "~a" (listof-zo->string provided->string (car (cdr pd))))
                         (format "~a" (listof-zo->string provided->string (car (cdr (cdr pd)))))))))

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
  ;; (-> boolean? provided? string?)
  (if (not deep?)
      "<struct:provided>"
      (format-list (list "provided"
                         (format "  name       : ~a" (provided-name z))
                         (format "  src        : ~a" (provided-src  z))
                         (format "  src-name   : ~a" (provided-src-name z))
                         (format "  nom-src    : ~a" (provided-nom-src z))
                         (format "  src-phase  : ~a" (provided-src-phase z))
                         (format "  protected? : ~a" (provided-protected? z))))))

;; -- expr

(define (lam->string deep? z)
  ;; (-> boolean? lam? string?)
  (if (not deep?)
      "<struct:lam>"
      (format-list (list "lam"
                         (format "  name          : ~a" (lam-name z))
                         (format "  flags         : ~a" (lam-flags z))
                         (format "  num-params    : ~a" (lam-num-params z))
                         (format "  param-types   : ~a" (lam-param-types z))
                         (format "  rest?         : ~a" (lam-rest? z))
                         (format "  closure-map   : ~a" (lam-closure-map z))
                         (format "  closure-types : ~a" (lam-closure-types z))
                         (format "  toplevel-map  : ~a" (lam-toplevel-map z))
                         (format "  max-let-depth : ~a" (lam-max-let-depth z))
                         (format "  body          : ~a" (expr-seq-any->string (lam-body z)))))))

(define (closure->string deep? z)
  ;; (-> boolean? closure? string?)
  (if (not deep?)
      "<struct:closure>"
      (format-list (list "closure"
                         (format "  code   : ~a" (lam->string #f (closure-code z)))
                         (format "  gen-id : ~a" (closure-gen-id z))))))

(define (case-lam->string deep? z)
  ;; (-> boolean? case-lam? string?)
  (if (not deep?)
      "<struct:case-lam>"
  (format-list (list "case-lam"
                     (format "  name    : ~a" (case-lam-name z))
                     (format "  clauses : ~a" (listof-zo->string lam->string (case-lam-clauses z)))))))

(define (let-one->string deep? z)
  ;; (-> boolean? let-one? string?)
  (if (not deep?)
      "<struct:let-one>"
      (format-list (list "let-one"
                         (format "  rhs     : ~a" (expr-seq-any->string (let-one-rhs  z)))
                         (format "  body    : ~a" (expr-seq-any->string (let-one-body z)))
                         (format "  type    : ~a" (let-one-type z))
                         (format "  unused? : ~a" (let-one-unused? z))))))

(define (let-void->string deep? z)
  ;; (-> boolean? let-void? string?)
  (if (not deep?)
      "<struct:let-void>"
      (format-list (list "let-void"
                         (format "  count  : ~a" (let-void-count z))
                         (format "  boxes? : ~a" (let-void-boxes? z))
                         (format "  body   : ~a" (expr-seq-any->string (let-void-body z)))))))

(define (install-value->string deep? z)
  ;; (-> boolean? install-value? string?)
  (if (not deep?)
      "<struct:install-value>"
      (format-list (list "install-value"
                         (format "  count  : ~a" (install-value-count z))
                         (format "  pos    : ~a" (install-value-pos z))
                         (format "  boxes? : ~a" (install-value-boxes? z))
                         (format "  rhs    : ~a" (expr-seq-any->string (install-value-rhs z)))
                         (format "  body   : ~a" (expr-seq-any->string (install-value-body z)))))))

(define (let-rec->string deep? z)
  ;; (-> boolean? let-rec? string?)
  (if (not deep?)
      "<struct:let-rec>"
      (format-list (list "let-rec"
                         (format "  procs : " (listof-zo->string lam->string (let-rec-procs z)))
                         (format "  body  : ~a" (expr-seq-any->string (let-rec-body z)))))))

(define (boxenv->string deep? z)
  ;; (-> boolean? boxenv? string?)
  (if (not deep?)
      "<struct:boxenv>"
      (format-list (list "boxenv"
                         (format "  pos  : ~a" (boxenv-pos z))
                         (format "  body : ~a" (expr-seq-any->string (boxenv-body z)))))))

(define (localref->string deep? z)
  ;; (-> boolean? localref? string?)
  (if (not deep?)
      "<struct:localref>"
      (format-list (list "localref"
                         (format "  unbox?        : ~a" (localref-unbox? z))
                         (format "  pos           : ~a" (localref-pos z))
                         (format "  clear?        : ~a" (localref-clear? z))
                         (format "  other-clears? : ~a" (localref-other-clears? z))
                         (format "  type          : ~a" (localref-type z))))))

(define (toplevel->string deep? z)
  ;; (-> boolean? toplevel? string?)
  (if (not deep?)
      "<struct:toplevel>"
      (format-list (list "toplevel"
                         (format "  depth  : ~a" (toplevel-depth z))
                         (format "  pos    : ~a" (toplevel-pos z))
                         (format "  const? : ~a" (toplevel-const? z))
                         (format "  ready? : ~a" (toplevel-ready? z))))))

(define (topsyntax->string deep? z)
  ;; (-> boolean? topsyntax? string?)
  (if (not deep?)
      "<struct:topsyntax>"
      (format-list (list "topsyntax"
                         (format "  depth : ~a" (topsyntax-depth z))
                         (format "  pos   : ~a" (topsyntax-pos z))
                         (format "  midpt : ~a" (topsyntax-midpt z))))))

(define (application->string deep? z)
  ;; (-> boolean? application? string?)
  (if (not deep?)
      "<struct:application>"
      (format-list (list "application"
                         (format "  rator : ~a" (expr-seq-any->string (application-rator z)))
                         (format "  rands : ~a" (map expr-seq-any->string (application-rands z)))))))

(define (branch->string deep? z)
  ;; (->  boolean? branch? string?)
  (if (not deep?)
      "<struct:branch>"
      (format-list (list "branch"
                         (format "  test : ~a" (expr-seq-any->string (branch-test z)))
                         (format "  then : ~a" (expr-seq-any->string (branch-then z)))
                         (format "  else : ~a" (expr-seq-any->string (branch-else z)))))))

(define (with-cont-mark->string deep? z)
  ;; (-> boolean? with-cont-mark? string?)
  (if (not deep?)
      "<struct:with-cont-mark>"
      (format-list (list "with-cont-mark"
                         (format "  key  : ~a" (expr-seq-any->string (with-cont-mark-key  z)))
                         (format "  val  : ~a" (expr-seq-any->string (with-cont-mark-val  z)))
                         (format "  body : ~a" (expr-seq-any->string (with-cont-mark-body z)))))))

(define (beg0->string deep? z)
  ;; (-> boolean? beg0? string?)
  (if (not deep?)
      "<struct:beg0>"
      (format-list (list "beg0"
                         (format "  seq : ~a" (map expr-seq-any->string (beg0-seq)))))))

(define (varref->string deep? z)
  ;; (-> boolean? varref? string?)
  (if (not deep?)
      "<struct:varref>"
      (format-list (list "varref"
                         (format "  toplevel : ~a" (let ([tl (varref-toplevel z)])
                                                     (cond [(toplevel? tl) (toplevel->string #f tl)]
                                                           [else           tl])))
                         (format "  dummy    : ~a" (let ([dm (varref-dummy z)])
                                                     (cond [(toplevel? dm) (toplevel->string #f dm)]
                                                           [else           dm])))))))

(define (assign->string deep? z)
  ;; (-> boolean? assign? string?)
  (if (not deep?)
      "<struct:assign>"
      (format-list (list "assign"
                         (format "  id        : ~a" (toplevel->string #f (assign-id z)))
                         (format "  rhs       : ~a" (expr-seq-any->string (assign-rhs z)))
                         (format "  undef-ok? : ~a" (assign-undef-ok? z))))))

(define (apply-values->string deep? z)
  ;; (-> boolean? apply-values? string?)
  (if (not deep?)
      "<struct:apply-values>"
      (format-list (list "apply-values"
                         (format "  proc      : ~a" (expr-seq-any->string (apply-values-proc z)))
                         (format "  args-expr : ~a" (expr-seq-any->string (apply-values-args-expr z)))))))

(define (primval->string deep? z)
  ;; (-> boolean? primval? string?)
  (if (not deep?)
      "<struct:primval>"
      (format-list (list "primval"
                         (format "  id : ~a" (primval-id z))))))

;; -- wrap

(define (top-level-rename->string deep? z)
  ;; (-> boolean? top-level-rename? string?)
  (if (not deep?)
      "<struct:top-level-rename>"
      (format-list (list "top-level-rename"
                         (format "  flag : ~a" (top-level-rename z))))))

(define (mark-barrier->string deep? z)
  ;; (-> boolean? mark-barrier? string?)
  (if (not deep?)
      "<struct:mark-barrier>"
      (format-list (list "mark-barrier"
                         (format "  value : ~a" (mark-barrier-value z))))))

(define (lexical-rename->string deep? z)
  ;; (-> boolean? lexical-rename? string?)
  (if (not deep?)
      "<struct:lexical-rename>"
      (format-list (list "lexical-rename"
                         (format "  has-free-id-renames? : ~a" (lexical-rename-has-free-id-renames? z))
                         (format "  bool2                : ~a" (lexical-rename-bool2 z))
                         (format "  alist                : ~a" (lexical-rename-alist->string (lexical-rename-alist z)))))))

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
  ;; (-> boolean? phase-shift? string?)
  (if (not deep?)
      "<struct:phase-shift>"
      (format-list (list "phase-shift"
                         (format "  amt       : ~a" (phase-shift-amt z))
                         (format "  src       : ~a" (phase-shift-src z))
                         (format "  dest      : ~a" (phase-shift-dest z))
                         (format "  cancel-id : ~a" (phase-shift-cancel-id z))))))

;; 2014-12-10: Curious about 'unmarshals'
(define (module-rename->string deep? z)
  ;; (-> boolean? module-rename? string?)
  (if (not deep?)
      "<struct:module-rename>"
      (format-list (list "module-rename"
                         (format "  phase        : ~a" (module-rename-phase z))
                         (format "  kind         : ~a" (module-rename-kind z))
                         (format "  set-id       : ~a" (module-rename-set-id z))
                         (format "  unmarshals   : ~a" (listof-zo->string all-from-module->string (module-rename-unmarshals z)))
                         (format "  renames      : ~a" (listof-zo->string module-binding->string (module-rename-renames z)))
                         (format "  mark-renames : ~a" (module-rename-mark-renames z))
                         (format "  plus-kern?   : ~a" (module-rename-plus-kern? z))))))

;; -- module-binding

(define (simple-module-binding->string deep? z)
  ;; (-> boolean? simple-module-binding? string?)
  (if (not deep?)
      "<struct:simple-module-binding>"
      (format-list (list "simple-module-binding"
                         (format "  path : ~a" (simple-module-binding-path z))))))

(define (phased-module-binding->string deep? z)
  ;; (-> boolean? phased-module-binding? string?)
  (if (not deep?)
      "<struct:phased-module-binding>"
      (format-list (list "phased-module-binding"
                         (format "  path                : ~a" (phased-module-binding-path z))
                         (format "  phase               : ~a" (phased-module-binding-phase z))
                         (format "  export-name         : ~a" (phased-module-binding-export-name z))
                         (format "  nominal-path        : ~a" (nominal-path->string #f (phased-module-binding-nominal-path z)))
                         (format "  nominal-export-name : ~a" (phased-module-binding-nominal-export-name z))))))

(define (exported-nominal-module-binding->string deep? z)
  ;; (-> boolean? exported-nominal-module-binding? string?)
  (if (not deep?)
      "<struct:exported-nominal-module-binding>"
      (format-list (list "exported-nominal-module-binding"
                         (format "  path                : ~a" (exported-nominal-module-binding-path z))
                         (format "  export-name         : ~a" (exported-nominal-module-binding-export-name z))
                         (format "  nominal-path        : ~a" (nominal-path->string #f (exported-nominal-module-binding-nominal-path z)))
                         (format "  nominal-export-name : ~a" (exported-nominal-module-binding-nominal-export-name z))))))

(define (nominal-module-binding->string deep? z)
  ;; (-> boolean? nominal-module-binding? string?)
  (if (not deep?)
      "<struct:nominal-module-binding>"
      (format-list (list "nominal-module-binding"
                         (format "  path         : ~a" (nominal-module-binding-path z))
                         (format "  nominal-path : ~a" (nominal-path->string (nominal-module-binding-nominal-path z)))))))

(define (exported-module-binding->string deep? z)
  ;; (-> boolean? exported-module-binding? string?)
  (if (not deep?)
      "<struct:exported-module-binding>"
      (format-list (list "exported-module-binding"
                         (format "  path        : ~a" (exported-module-binding-path z))
                         (format "  export-name : ~a" (exported-module-binding-export-name z))))))

;; -- nominal-path

(define (simple-nominal-path->string deep? z)
  ;; (-> boolean? simple-nominal-path? string?)
  (if (not deep?)
      "<struct:simple-nominal-path>"
      (format-list (list "simple-nominal-path"
                         (format "  value : ~a" (simple-nominal-path-value z))))))

(define (imported-nominal-path->string deep? z)
  ;; (-> boolean? imported-nominal-path? string?)
  (if (not deep?)
      "<struct:imported-nominal-path>"
      (format-list (list "imported-nominal-path"
                         (format "  value        : ~a" (imported-nominal-path-value z))
                         (format "  import-phase : ~a" (imported-nominal-path-import-phase z))))))

(define (phased-nominal-path->string deep? z)
  ;; (-> boolean? phased-nominal-path? string?)
  (if (not deep?)
      "<struct:phased-nominal-path>"
      (format-list (list "phased-nominal-path"
                         (format "  value        : ~a" (phased-nominal-path-value z))
                         (format "  import-phase : ~a" (phased-nominal-path-import-phase z))
                         (format "  phase        : ~a" (phased-nominal-path-phase z))))))

;; -- helpers

(define (any->string)
  ;; (-> string?)
  "<any>")

(define (expr-seq-any->string z)
  ;; (-> (or/c expr? seq? any/c) string?)
  (cond [(expr? z) "<struct:expr>"]
        [(seq?  z) "<struct:seq>"]
        [else      (any->string)]))

(define (format-list xs #:sep [sep "\n"])
  ;; (-> (listof string?) string?)
  (string-join xs sep))

(define (listof-form-or-any->string xs)
  ;; (-> (listof (or/c form? any/c)) (listof string?))
  (for/list ([x xs])
    (cond [(form? x) (form->string #f x)]
          [else      (any->string)])))

(define (listof-zo->string z->str zs)
  ;; (-> (-> boolean? zo? string?) (listof zo?) string?)
  (cond [(empty? zs) "[]"]
        [else        "~a[~a]" (z->str #f (car zs)) (length zs)]))

