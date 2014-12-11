#lang racket/base

(provide zo->string)

(require compiler/zo-structs
         (only-in racket/string string-join))

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

;; 2014-12-10: Should replace all 'map' with a more specific formatting function.
(define (format-list xs)
  ;; (-> (listof string?) string?)
  (string-join xs "\n"))

(define (compilation-top->string z)
  ;; (-> compilation-top? string?)
  (format-list (list "compilation-top"
                     (format "  max-let-depth : ~a" (compilation-top-max-let-depth z))
                     (format "  prefix        : <struct:prefix>")
                     (format "  code          : <~a>" (if (form? (compilation-top-code z))
                                                          "struct:form"
                                                          "any")))))

(define (prefix->string z)
  ;; (-> prefix? string?)
  (format-list (list "prefix"
                     (format "  num-lifts : ~a" (prefix-num-lifts z))
                     (format "  toplevels : ~a" (map (lambda (tl)
                                                       (cond [(module-variable? tl) "<struct:module-variable>"]
                                                             [(global-bucket?   tl) "<struct:global-bucket>"]
                                                             [else (format "~a" tl)]))
                                                     (prefix-toplevels z)))
                     (format "  stxs      : [list of ~a <struct:stx>]" (length (prefix-stxs z))))))

(define (global-bucket->string z)
  ;; (->  global-bucket? string?)
  (format-list (list "global-bucket"
                     (format "  name : ~a" (global-bucket-name z)))))

; 2014-12-10: May want to print 'constantness' nicer. Define struct-shape->string.
(define (module-variable->string z)
  ;; (-> module-variable? string?)
  (format-list (list "module-variable"
                     (format "  modidx       : ~a" (module-variable-modidx z))
                     (format "  sym          : ~a" (module-variable-sym z))
                     (format "  pos          : ~a" (module-variable-pos z))
                     (format "  phase        : ~a" (module-variable-phase z))
                     (format "  constantness : ~a" (module-variable-constantness z)))))

(define (stx->string z)
  ;; (-> stx? string?)
  (format-list (list "stx"
                     (format "  encoded : <struct:wrapped>"))))

(define (form->string z)
  ;; (-> form? string?)
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
        [else "form"]))

(define (expr->string z)
  ;; (-> expr? string?)
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
        [else "expr"]))

(define (wrapped->string z)
  ;; (-> wrapped? string?)
  (format-list (list "wrapped"
                     (format "  datum         : ~a" (wrapped-datum z))
                     (format "  wraps         : [list of ~a <struct:wrap>]" (length (wrapped-wraps z)))
                     (format "  tamper-status : ~a" (wrapped-tamper-status z)))))

(define (wrap->string z)
  ;; (-> wrap? string?)
  (cond [(top-level-rename? z) (top-level-rename->string z)]
        [(mark-barrier?     z) (mark-barrier->string     z)]
        [(lexical-rename?   z) (lexical-rename->string   z)]
        [(phase-shift?      z) (phase-shift->string      z)]
        [(module-rename?    z) (module-rename->string    z)]
        [else "wrap"]))

(define (free-id-info->string z)
  ;; (-> free-id-info? string?)
  (format-list (list "free-id-info"
                     (format "  path0                  : ~a" (free-id-info-path0 z))
                     (format "  symbol0                : ~a" (free-id-info-symbol0 z))
                     (format "  path1                  : ~a" (free-id-info-path1 z))
                     (format "  symbol1                : ~a" (free-id-info-symbol1 z))
                     (format "  phase0                 : ~a" (free-id-info-phase0 z))
                     (format "  phase1                 : ~a" (free-id-info-phase1 z))
                     (format "  phase2                 : ~a" (free-id-info-phase2 z))
                     (format "  use-current-inspector? : ~a" (free-id-info-use-current-inspector? z)))))

(define (all-from-module->string z)
  ;; (-> all-from-module? string?)
  (format-list (list "all-from-module"
                     (format "  path      : ~a" (all-from-module-path z))
                     (format "  phase     : ~a" (all-from-module-phase z))
                     (format "  src-phase : ~a" (all-from-module-src-phase z))
                     (format "  exception : ~a" (all-from-module-exceptions z))
                     (format "  prefix    : ~a" (all-from-module-prefix z))
                     (format "  context   : ~a" (all-from-module-context z)))))

(define (module-binding->string z)
  ;; (-> module-binding? string?)
  (cond [(simple-module-binding?           z) (simple-module-binding->string           z)]
        [(phased-module-binding?           z) (phased-module-binding->string           z)]
        [(exported-nominal-module-binding? z) (exported-nominal-module-binding->string z)]
        [(nominal-module-binding?          z) (nominal-module-binding->string          z)]
        [(exported-module-binding?         z) (exported-module-binding->string         z)]
        [else "module-binding"]))


(define (nominal-path->string z)
  ;; (-> nominal-path? string?)
  (cond [(simple-nominal-path?   z) (simple-nominal-path->string   z)]
        [(imported-nominal-path? z) (imported-nominal-path->string z)]
        [(phased-nominal-path?   z) (phased-nominal-path->string   z)]
        [else "nominal-path"]))

;; -- form

(define (def-values->string z)
  ;; (-> def-values? string?)
  (format-list (list "def-values"
                     (format "  ids : [list of ~a <struct:toplevel>]" (length (def-values-ids z)))
                     (format "  rhs : ~a" (let ([rhs (def-values-rhs z)])
                                            (cond [(expr? rhs) "<struct:expr>"]
                                                  [(seq?  rhs) "<struct:seq>"]
                                                  [(inline-variant? rhs) "<struct:inline-variant>"]
                                                  [else "any"]))))))

(define (def-syntaxes->string z)
  ;; (-> def-syntaxes? string?)
  (format-list (list "def-syntaxes"
                     (format "  ids           : ~a" (def-syntaxes-ids z))
                     (format "  rhs           : ~a" (expr-seq-any->string (def-syntaxes-rhs z)))
                     (format "  prefix        : <struct:prefix")
                     (format "  max-let-depth : ~a" (def-syntaxes-max-let-depth z))
                     (format "  dummy         : ~a" (let ([tl (def-syntaxes-dummy z)])
                                                      (cond [(toplevel? tl) "<struct:toplevel>"]
                                                            [else           "any"]))))))

(define (seq-for-syntax->string z)
  ;; (-> seq-for-syntax? string?)
  (format-list (list "seq-for-syntax"
                     (format "  forms         : ~a" (map (lambda (fm)
                                                           (cond [(form? fm) "<struct:form>"]
                                                                 [else       "any"]))
                                                         (seq-for-syntax-forms z)))
                     (format "  prefix        : ~a" (seq-for-syntax-prefix z))
                     (format "  max-let-depth : ~a" (seq-for-syntax-max-let-depth z))
                     (format "  dummy         : ~a" (let ([dm (seq-for-syntax-dummy z)])
                                                      (cond [(toplevel? dm) "<struct:toplevel>"]
                                                            [else           "any"]))))))

(define (req->string z)
  ;; (-> req? string?)
  (format-list (list "req"
                     (format "  reqs  : <struct:stx>")
                     (format "  dummy : <struct:toplevel>"))))

(define (seq->string z)
  ;; (-> seq? string?)
  (format-list (list "seq"
                     (format "  forms : ~a" (map (lambda (fm)
                                                   (cond [(form? fm) "<struct:form>"]
                                                         [else       "any"]))
                                                 (seq-forms z))))))

(define (splice->string z)
  ;; (-> splice? string?)
  (format-list (list "splice"
                     (format "  forms : ~a" (map (lambda (fm)
                                                   (cond [(form? fm) "<struct:form>"]
                                                         [else       "any"]))
                                                 (splice-forms z))))))

(define (inline-variant->string z)
  ;; (-> inline-variant? string?)
  (format-list (list "inline-variant"
                     (format "  direct : <struct:expr>")
                     (format "  inline : <struct:expr>"))))

(define (mod->string z)
  ;; (-> mod? string?)
  (format-list (list "mod"
                     (format "  name             : ~a" (mod-name z))
                     (format "  srcname          : ~a" (mod-srcname z))
                     (format "  self-modidx      : ~a" (mod-self-modidx z))
                     (format "  prefix           : <struct:prefix>")
                     (format "  provides         : ~a" (mod-provides->string (mod-provides z)))
                     (format "  requires         : ~a" (mod-requires->string (mod-requires z)))
                     (format "  body             : ~a" (map (lambda (fm)
                                                              (cond [(form? fm) "<struct:form>"]
                                                                    [else       "any"]))
                                                            (mod-body z)))
                     (format "  syntax-bodies    : ~a" (mod-syntax-bodies->string (mod-syntax-bodies z)))
                     (format "  unexported       : ~a" (mod-unexported z))
                     (format "  max-let-depth    : ~a" (mod-max-let-depth z))
                     (format "  dummy            : <struct:toplevel>")
                     (format "  lang-info        : ~a" (mod-lang-info z))
                     (format "  internal-context : ~a" (let ([ic (mod-internal-context z)])
                                                         (cond [(stx? ic)    "<struct:stx>"]
                                                               [(vector? ic) (format "[vector of ~a <struct:stx>]" (vector-length ic))]
                                                               [else         ic])))
                     (format "  flags            : ~a" (mod-flags z))
                     (format "  pre-submodules   : [list of ~a <struct:mod>]" (length (mod-pre-submodules z)))
                     (format "  post-submodules  : [list of ~a <struct:mod>]" (length (mod-post-submodules z))))))

;; 2014-12-10: Ugly
(define (mod-provides->string pds)
  ;; (-> (listof (list/c (or/c exact-integer? #f) (listof provided?) (listof provided?))) string?)
  (string-join (map (lambda (pd)
                      (format "(~a ~a ~a)"
                              (car pd) ; (or/c exact-integer? #f)
                              (format "[list of ~a <struct:provided> (exported vars)]" (length (car (cdr pd))))
                              (format "[list of ~a <struct:provided> (exported syntax)]" (length (car (cdr (cdr pd)))))))
                    pds)
               " "))

;; 2014-12-10: Ugly
(define (mod-requires->string rqs)
  ;; (-> (listof (cons/c (or/c exact-integer #f) (listof module-path-index?))) string?)
  (string-join (map (lambda (rq)
                      (format "(~a ~a)"
                              (car rq)
                              (cdr rq)))
                    rqs)
               " "))

;; 2014-12-10: Ugly
(define (mod-syntax-bodies->string sbs)
  ;; (-> (listof (cons/c exact-positive-integer (listof (or/c def-syntaxes? seq-for-syntax?)))) string?)
  (string-join (map (lambda (sb)
                      (format "(~a . ~a)"
                              (car sb)
                              (map (lambda (d)
                                     (cond [(def-syntaxes?   d) "<struct:def-syntaxes>"]
                                           [(seq-for-syntax? d) "<struct:seq-for-syntax>"]))
                                   (cdr sb))))
                    sbs)
               " "))
                
(define (provided->string z)
  ;; (-> provided? string?)
  (format-list (list "provided"
                     (format "  name       : ~a" (provided-name z))
                     (format "  src        : ~a" (provided-src  z))
                     (format "  src-name   : ~a" (provided-src-name z))
                     (format "  nom-src    : ~a" (provided-nom-src z))
                     (format "  src-phase  : ~a" (provided-src-phase z))
                     (format "  protected? : ~a" (provided-protected? z)))))

;; -- expr

(define (lam->string z)
  ;; (-> lam? string?)
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
                     (format "  body          : ~a" (expr-seq-any->string (lam-body z))))))

(define (closure->string z)
  ;; (-> closure? string?)
  (format-list (list "closure"
                     (format "  code   : <struct:lam")
                     (format "  gen-id : ~a" (closure-gen-id z)))))

(define (case-lam->string z)
  ;; (-> case-lam? string?)
  (format-list (list "case-lam"
                     (format "  name    : ~a" (case-lam-name z))
                     (format "  clauses : [list of ~a <struct:lam>]" (length (case-lam-clauses z))))))

(define (let-one->string z)
  ;; (-> let-one? string?)
  (format-list (list "let-one"
                     (format "  rhs     : ~a" (expr-seq-any->string (let-one-rhs  z)))
                     (format "  body    : ~a" (expr-seq-any->string (let-one-body z)))
                     (format "  type    : ~a" (let-one-type z))
                     (format "  unused? : ~a" (let-one-unused? z)))))

(define (let-void->string z)
  ;; (-> let-void? string?)
  (format-list (list "let-void"
                     (format "  count  : ~a" (let-void-count z))
                     (format "  boxes? : ~a" (let-void-boxes? z))
                     (format "  body   : ~a" (expr-seq-any->string (let-void-body z))))))

(define (install-value->string z)
  ;; (-> install-value? string?)
  (format-list (list "install-value"
                     (format "  count  : ~a" (install-value-count z))
                     (format "  pos    : ~a" (install-value-pos z))
                     (format "  boxes? : ~a" (install-value-boxes? z))
                     (format "  rhs    : ~a" (expr-seq-any->string (install-value-rhs z)))
                     (format "  body   : ~a" (expr-seq-any->string (install-value-body z))))))

(define (let-rec->string z)
  ;; (-> let-rec? string?)
  (format-list (list "let-rec"
                     (format "  procs : [list of ~a <struct:lam>]" (length (let-rec-procs z)))
                     (format "  body  : ~a" (expr-seq-any->string (let-rec-body z))))))

(define (boxenv->string z)
  ;; (-> boxenv? string?)
  (format-list (list "boxenv"
                     (format "  pos  : ~a" (boxenv-pos z))
                     (format "  body : ~a" (expr-seq-any->string (boxenv-body z))))))

(define (localref->string z)
  ;; (-> localref? string?)
  (format-list (list "localref"
                     (format "  unbox?        : ~a" (localref-unbox? z))
                     (format "  pos           : ~a" (localref-pos z))
                     (format "  clear?        : ~a" (localref-clear? z))
                     (format "  other-clears? : ~a" (localref-other-clears? z))
                     (format "  type          : ~a" (localref-type z)))))

(define (toplevel->string z)
  ;; (-> toplevel? string?)
  (format-list (list "toplevel"
                     (format "  depth  : ~a" (toplevel-depth z))
                     (format "  pos    : ~a" (toplevel-pos z))
                     (format "  const? : ~a" (toplevel-const? z))
                     (format "  ready? : ~a" (toplevel-ready? z)))))

(define (topsyntax->string z)
  ;; (-> topsyntax? string?)
  (format-list (list "topsyntax"
                     (format "  depth : ~a" (topsyntax-depth z))
                     (format "  pos   : ~a" (topsyntax-pos z))
                     (format "  midpt : ~a" (topsyntax-midpt z)))))

(define (application->string z)
  ;; (-> application? string?)
  (format-list (list "application"
                     (format "  rator : ~a" (expr-seq-any->string (application-rator z)))
                     (format "  rands : ~a" (map expr-seq-any->string (application-rands z))))))

(define (branch->string z)
  ;; (->  (branch? string?)
  (format-list (list "branch"
                     (format "  test : ~a" (expr-seq-any->string (branch-test z)))
                     (format "  then : ~a" (expr-seq-any->string (branch-then z)))
                     (format "  else : ~a" (expr-seq-any->string (branch-else z))))))

(define (with-cont-mark->string z)
  ;; (-> with-cont-mark? string?)
  (format-list (list "with-cont-mark"
                     (format "  key  : ~a" (expr-seq-any->string (with-cont-mark-key  z)))
                     (format "  val  : ~a" (expr-seq-any->string (with-cont-mark-val  z)))
                     (format "  body : ~a" (expr-seq-any->string (with-cont-mark-body z))))))

(define (beg0->string z)
  ;; (-> beg0? string?)
  (format-list (list "beg0"
                     (format "  seq : ~a" (map expr-seq-any->string (beg0-seq))))))

(define (varref->string z)
  ;; (-> varref? string?)
  (format-list (list "varref"
                     (format "  toplevel : ~a" (let ([tl (varref-toplevel z)])
                                                 (cond [(toplevel? tl) "<struct:toplevel>"]
                                                       [else           tl])))
                     (format "  dummy    : ~a" (let ([dm (varref-dummy z)])
                                                 (cond [(toplevel? dm) "<struct:toplevel>"]
                                                       [else           dm]))))))

(define (assign->string z)
  ;; (-> assign? string?)
  (format-list (list "assign"
                     (format "  id        : <struct:toplevel>")
                     (format "  rhs       : ~a" (expr-seq-any->string (assign-rhs z)))
                     (format "  undef-ok? : ~a" (assign-undef-ok? z)))))

(define (apply-values->string z)
  ;; (-> apply-values? string?)
  (format-list (list "apply-values"
                     (format "  proc      : ~a" (expr-seq-any->string (apply-values-proc z)))
                     (format "  args-expr : ~a" (expr-seq-any->string (apply-values-args-expr z))))))

(define (primval->string z)
  ;; (-> primval? string?)
  (format-list (list "primval"
                     (format "  id : ~a" (primval-id z)))))

;; -- wrap

(define (top-level-rename->string z)
  ;; (-> top-level-rename? string?)
  (format-list (list "top-level-rename"
                     (format "  flag : ~a" (top-level-rename z)))))

(define (mark-barrier->string z)
  ;; (-> mark-barrier? string?)
  (format-list (list "mark-barrier"
                     (format "  value : ~a" (mark-barrier-value z)))))

(define (lexical-rename->string z)
  ;; (-> lexical-rename? string?)
  (format-list (list "lexical-rename"
                     (format "  has-free-id-renames? : ~a" (lexical-rename-has-free-id-renames? z))
                     (format "  bool2                : ~a" (lexical-rename-bool2 z))
                     (format "  alist                : ~a" (lexical-rename-alist->string (lexical-rename-alist z))))))

;; 2014-12-10: Ugly
(define (lexical-rename-alist->string alst)
  ;; (-> (listof (cons/c symbol? (or/c symbol? (cons/c symbol? (or/c (cons/c symbol? (or/c symbol? #f)) free-id-info?))))) string?)
  (string-join (map (lambda (a)
                      (format "(~a . ~a)"
                              (car a)
                              (cond [(symbol? (cdr a)) (cdr a)]
                                    [else (let ([a* (cdr a)])
                                            (format "(~a . ~a)"
                                                    (car a*)
                                                    (cond [(free-id-info? (cdr a*)) "<struct:free-id-info>"]
                                                          [else (cdr a*)])))])))
                    alst)
               " "))
  
(define (phase-shift->string z)
  ;; (-> phase-shift? string?)
  (format-list (list "phase-shift"
                     (format "  amt       : ~a" (phase-shift-amt z))
                     (format "  src       : ~a" (phase-shift-src z))
                     (format "  dest      : ~a" (phase-shift-dest z))
                     (format "  cancel-id : ~a" (phase-shift-cancel-id z)))))

;; 2014-12-10: Curious about 'unmarshals'
(define (module-rename->string z)
  ;; (-> module-rename? string?)
  (format-list (list "module-rename"
                     (format "  phase        : ~a" (module-rename-phase z))
                     (format "  kind         : ~a" (module-rename-kind z))
                     (format "  set-id       : ~a" (module-rename-set-id z))
                     (format "  unmarshals   : [list of ~a <struct:make-all-from-module>]" (length (module-rename-unmarshals z)))
                     (format "  renames      : [list of ~a <struct:module-binding>]" (length (module-rename-renames z)))
                     (format "  mark-renames : ~a" (module-rename-mark-renames z))
                     (format "  plus-kern?   : ~a" (module-rename-plus-kern? z)))))

;; -- module-binding

(define (simple-module-binding->string z)
  ;; (-> simple-module-binding? string?)
  (format-list (list "simple-module-binding"
                     (format "  path : ~a" (simple-module-binding-path z)))))

(define (phased-module-binding->string z)
  ;; (-> phased-module-binding? string?)
  (format-list (list "phased-module-binding"
                     (format "  path                : ~a" (phased-module-binding-path z))
                     (format "  phase               : ~a" (phased-module-binding-phase z))
                     (format "  export-name         : ~a" (phased-module-binding-export-name z))
                     (format "  nominal-path        : <struct:nominal-path>")
                     (format "  nominal-export-name : ~a" (phased-module-binding-nominal-export-name z)))))

(define (exported-nominal-module-binding->string z)
  ;; (-> exported-nominal-module-binding? string?)
  (format-list (list "exported-nominal-module-binding"
                     (format "  path                : ~a" (exported-nominal-module-binding-path z))
                     (format "  export-name         : ~a" (exported-nominal-module-binding-export-name z))
                     (format "  nominal-path        : <struct:nominal-path>")
                     (format "  nominal-export-name : ~a" (exported-nominal-module-binding-nominal-export-name z)))))

(define (nominal-module-binding->string z)
  ;; (-> nominal-module-binding? string?)
  (format-list (list "nominal-module-binding"
                     (format "  path         : ~a" (nominal-module-binding-path z))
                     (format "  nominal-path : <struct:nominal-path>"))))

(define (exported-module-binding->string z)
  ;; (-> exported-module-binding? string?)
  (format-list (list "exported-module-binding"
                     (format "  path        : ~a" (exported-module-binding-path z))
                     (format "  export-name : ~a" (exported-module-binding-export-name z)))))

;; -- nominal-path

(define (simple-nominal-path->string z)
  ;; (-> simple-nominal-path? string?)
  (format-list (list "simple-nominal-path"
                     (format "  value : ~a" (simple-nominal-path-value z)))))

(define (imported-nominal-path->string z)
  ;; (-> imported-nominal-path? string?)
  (format-list (list "imported-nominal-path"
                     (format "  value        : ~a" (imported-nominal-path-value z))
                     (format "  import-phase : ~a" (imported-nominal-path-import-phase z)))))

(define (phased-nominal-path->string z)
  ;; (-> phased-nominal-path? string?)
  (format-list (list "phased-nominal-path"
                     (format "  value        : ~a" (phased-nominal-path-value z))
                     (format "  import-phase : ~a" (phased-nominal-path-import-phase z))
                     (format "  phase        : ~a" (phased-nominal-path-phase z)))))

;; -- helpers

(define (expr-seq-any->string z)
  ;; (-> (or/c expr? seq? any/c) string?)
  (cond [(expr? z) "<struct:expr>"]
        [(seq?  z) "<struct:seq>"]
        [else      "any"]))
