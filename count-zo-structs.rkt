#lang racket/base

;; Count the AST nodes that appear in a bytecode file
(provide count-structs
         zsc->string)

;; --- imports & data

(require compiler/zo-structs
         racket/string)

;; [zsc] (Short for "zo-struct-counter")
;; Each field names a zo-struct.
;; Field names look like "num-X" where "X" is the name of the zo-struct.
;; Each value is a natural number representing the number of times this struct appeared.
(struct zsc
  (num-compilation-top
   num-form
   num-expr
   ;; Shapes
   num-function-shape
   num-struct-shape
   num-constructor-shape ;unused
   num-predicate-shape   ;unused
   num-accessor-shape    ;unused
   num-mutator-shape     ;unused
   num-type-shape        ;unused
   num-other-shape       ;unused
   ;; Top level variables
   num-global-bucket
   num-module-variable
   ;; Syntax
   num-wrap
   num-wrapped
   num-stx
   num-prefix
   ;; Expressions
   num-provided
   num-toplevel
   num-seq
   num-seq-for-syntax
   num-inline-variant
   num-def-values
   num-def-syntaxes
   num-mod
   num-lam
   num-closure
   num-case-lam
   num-let-one
   num-let-void
   num-install-value
   num-let-rec
   num-boxenv
   num-localref
   num-topsyntax
   num-application
   num-branch
   num-with-cont-mark
   num-beg0
   num-splice
   num-varref
   num-assign
   num-apply-values
   num-primval
   ;; Requires
   num-req
   num-free-id-info
   num-lexical-rename
   num-phase-shift
   num-wrap-mark ;unused
   num-prune     ;unused
   num-all-from-module
   ;; Paths
   num-nominal-path
   num-simple-nominal-path
   num-imported-nominal-path
   num-phased-nominal-path
   num-module-binding
   ;; Module names
   num-phased-module-binding
   num-exported-nominal-module-binding
   num-nominal-module-binding
   num-exported-module-binding
   num-simple-module-binding
   num-module-rename
   num-top-level-rename
   num-mark-barrier
  )
  #:mutable)

;; Syntax to increment a [zsc] field.
;; "(zsc++ s f)" desugars to "(set-zsc-f! s (add1 (zsc-f s)))"
(require (for-syntax racket/base racket/syntax))
(define-syntax (zsc++ stx)
  (syntax-case stx ()
    [(_)       (raise-syntax-error #f "[zsc++] Expected (struct field-name)")]
    [(_ _)     (raise-syntax-error #f "[zsc++] Expected (struct field-name)")]
    [(_ st fd) (with-syntax* ([setter (format-id stx "set-zsc-~a!" #'fd)]
                              [getter (format-id stx "zsc-~a"      #'fd)])
                             #'(setter st (add1 (getter st))))]))

;; --- API functions

;; Analyze input from zo-parse. Count the number of structs appearing in the bytecode.
(define (count-structs ct)
  ;; (-> compilation-top? zsc?)
  (define z (zsc-init))
  (begin (count-compilation-top z ct)
         z))

;; Format the results of [count-structs]. Converts each struct field to a string.
;; Aggregates all strings into tab-separated format.
(define (zsc->string z)
  ;; (-> zsc? string?)
  (define field-names (list "compilation-top" "form" "function-shape" "struct-shape" "constructor-shape" "predicate-shape" "accessor-shape" "mutator-shape" "type-shape" "other-shape" "global-bucket" "module-variable" "wrap" "wrapped" "stx" "prefix" "expr" "provided" "toplevel" "seq" "seq-for-syntax" "inline-variant" "def-values" "def-syntaxes" "mod" "lam" "closure" "case-lam" "let-one" "let-void" "install-value" "let-rec" "boxenv" "localref" "topsyntax" "application" "branch" "with-cont-mark" "beg0" "splice" "varref" "assign" "apply-values" "primval" "req" "free-id-info" "lexical-rename" "phase-shift" "wrap-mark" "prune" "all-from-module" "nominal-path" "simple-nominal-path" "imported-nominal-path" "phased-nominal-path" "module-binding" "phased-module-binding" "exported-nominal-module-binding" "nominal-module-binding" "exported-module-binding" "simple-module-binding" "module-rename" "top-level-rename" "mark-barrier"))
  (define counts      (list (zsc-num-compilation-top z) (zsc-num-form z) (zsc-num-function-shape z) (zsc-num-struct-shape z) (zsc-num-constructor-shape z) (zsc-num-predicate-shape z) (zsc-num-accessor-shape z) (zsc-num-mutator-shape z) (zsc-num-type-shape z) (zsc-num-other-shape z) (zsc-num-global-bucket z) (zsc-num-module-variable z) (zsc-num-wrap z) (zsc-num-wrapped z) (zsc-num-stx z) (zsc-num-prefix z) (zsc-num-expr z) (zsc-num-provided z) (zsc-num-toplevel z) (zsc-num-seq z) (zsc-num-seq-for-syntax z) (zsc-num-inline-variant z) (zsc-num-def-values z) (zsc-num-def-syntaxes z) (zsc-num-mod z) (zsc-num-lam z) (zsc-num-closure z) (zsc-num-case-lam z) (zsc-num-let-one z) (zsc-num-let-void z) (zsc-num-install-value z) (zsc-num-let-rec z) (zsc-num-boxenv z) (zsc-num-localref z) (zsc-num-topsyntax z) (zsc-num-application z) (zsc-num-branch z) (zsc-num-with-cont-mark z) (zsc-num-beg0 z) (zsc-num-splice z) (zsc-num-varref z) (zsc-num-assign z) (zsc-num-apply-values z) (zsc-num-primval z) (zsc-num-req z) (zsc-num-free-id-info z) (zsc-num-lexical-rename z) (zsc-num-phase-shift z) (zsc-num-wrap-mark z) (zsc-num-prune z) (zsc-num-all-from-module z) (zsc-num-nominal-path z) (zsc-num-simple-nominal-path z) (zsc-num-imported-nominal-path z) (zsc-num-phased-nominal-path z) (zsc-num-module-binding z) (zsc-num-phased-module-binding z) (zsc-num-exported-nominal-module-binding z) (zsc-num-nominal-module-binding z) (zsc-num-exported-module-binding z) (zsc-num-simple-module-binding z) (zsc-num-module-rename z) (zsc-num-top-level-rename z) (zsc-num-mark-barrier z)))
  (string-join (map (lambda (f c) (format "~a\t~a" f c)) field-names counts) "\n"))

;; Create a new zo-struct-counter. Initialize each field to 0.
(define (zsc-init)
  ;; (-> zsc?)
  (zsc 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;; --- Structural recursion

(define (count-compilation-top z ct)
  ;; (-> zsc? compilation-top? void?)
  (begin
    (zsc++ z num-compilation-top)
    (count-prefix z (compilation-top-prefix ct))
    (define fm (compilation-top-code ct))
    (when (form? fm) (count-form z fm))
  ))

(define (count-prefix z px)
  ;; (-> zsc? prefix? void?)
  (begin
    (zsc++ z num-prefix)
    ;; [prefix-toplevels] is not just a list of [toplevel] structs.
    (for ([tp (prefix-toplevels px)])
      (cond [(global-bucket?   tp) (count-global-bucket z tp)]
            [(module-variable? tp) (count-module-variable z tp)]
            [else (void)]))
    (for ([sx (prefix-stxs px)]) (count-stx z sx))
  ))

(define (count-global-bucket z gb)
  ;; (-> zsc? global-bucket? void?)
  (zsc++ z num-global-bucket))

;; represents a top-level variable
(define (count-module-variable z mv)
  ;; (-> zsc? module-variable? void?)
  (begin
    (zsc++ z num-module-variable)
    (define cs (module-variable-constantness mv))
    (cond [(function-shape? cs) (count-function-shape z cs)]
          [(struct-shape?   cs) (count-struct-shape   z cs)])
  ))

(define (count-function-shape z fs)
  ;; (-> zsc? function-shape? void?)
  (zsc++ z num-function-shape))

(define (count-struct-shape z ss)
  ;; (-> zsc? struct-shape? void?)
  (zsc++ z num-struct-shape))

(define (count-stx z sx)
  ;; (-> zsc? syx? void?)
  (begin
    (zsc++ z num-stx)
    (count-wrapped z (stx-encoded sx))
  ))

(define (count-form z fm)
  ;; (-> zsc? form? void?)
  (begin
    (zsc++ z num-form)
    (cond [(def-values?     fm) (count-def-values     z fm)]
          [(def-syntaxes?   fm) (count-def-syntaxes   z fm)]
          [(seq-for-syntax? fm) (count-seq-for-syntax z fm)]
          [(req?            fm) (count-req            z fm)]
          [(seq?            fm) (count-seq            z fm)]
          [(splice?         fm) (count-splice         z fm)]
          [(inline-variant? fm) (count-inline-variant z fm)]
          [(mod?            fm) (count-mod            z fm)]
          [(provided?       fm) (count-provided       z fm)]
          [(expr?           fm) (count-expr           z fm)]
          [else                 (error (format "[count-form] Unknown form '~a'" fm))])
  ))

(define (count-def-values z dv)
  ;; (-> zsc? def-values? void?)
  (begin
    (zsc++ z num-def-values)
    (for ([tl (def-values-ids dv)]) (count-toplevel z tl))
    ;; A [rhs] might be anything
    (define rhs (def-values-rhs dv))
    (cond [(expr?           rhs) (count-expr           z rhs)]
          [(seq?            rhs) (count-seq            z rhs)]
          [(inline-variant? rhs) (count-inline-variant z rhs)]
          [else                  (void)])
  ))

(define (count-def-syntaxes z ds)
  ;; (-> zsc? def-syntaxes? void?)
  (begin
    (zsc++ z num-def-syntaxes)
    (count-expr-or-seq z (def-syntaxes-rhs ds))
    (count-prefix z (def-syntaxes-prefix ds))
    (define dummy (def-syntaxes-dummy ds)) ;; used to access enclosing namespace
    (when (toplevel? dummy) (count-toplevel z dummy))
  ))

(define (count-seq-for-syntax z sfs)
  ;; (-> zsc? seq-for-syntax? void?)
  (begin
    (zsc++ z num-seq-for-syntax)
    (for ([fm (seq-for-syntax-forms sfs)])
      (when (form? fm) (count-form z fm)))
    (count-prefix z (seq-for-syntax-prefix sfs))
    (define dummy (seq-for-syntax-dummy sfs))
    (when (toplevel? dummy)
      (count-toplevel z dummy))
  ))

(define (count-req z rq)
  ;; (-> zsc? req? void?)
  (begin
    (zsc++ z num-req)
    (count-stx      z (req-reqs  rq))
    (count-toplevel z (req-dummy rq))
  ))

(define (count-seq z sq)
  ;; (-> zsc? seq? void?)
  (begin
    (zsc++ z num-seq)
    (for ([fm (seq-forms sq)])
      (when (form? fm) (count-form z fm)))
  ))

(define (count-splice z se)
  ;; (-> zsc? splice? void?)
  (begin
    (zsc++ z num-splice)
    (for ([fm (splice-forms se)])
      (when (form? fm) (count-form z fm)))
  ))

(define (count-inline-variant z iv)
  ;; (-> zsc? inline-variant? void?)
  (begin
    (zsc++ z num-inline-variant)
    (count-expr z (inline-variant-direct iv))
    (count-expr z (inline-variant-inline iv))
  ))

(define (count-mod z md)
  ;; (-> zsc? mod? void?)
  (begin
    (zsc++ z num-mod)
    ;; Ignoring [name] [srcname] [self-modidx] [requires] [unexported] [max-let-depth]
    (count-prefix z (mod-prefix md))
    (for ([ps (mod-provides md)])
      (begin (count-provided z (car (cdr ps)))
             (count-provided z (car (cdr (cdr ps))))))
    (for ([bd (mod-body md)])
      (when (form? bd) (count-form z bd)))
    (for ([sb (mod-syntax-bodies md)])
      (for ([sx (cdr sb)])
        (cond [(def-syntaxes?   sx) (count-def-syntaxes   z sx)]
              [(seq-for-syntax? sx) (count-seq-for-syntax z sx)]
              [else                 (void)])))
    (count-toplevel z (mod-dummy md))
    (define ic (mod-internal-context md))
    (cond [(stx?    ic) (count-stx z ic)]
          [(vector? ic) (for ([i ic]) (when (stx? i) (count-stx z i)))]
          [else         (void)])
    (for ([pre  (mod-pre-submodules  md)]) (count-mod z  pre))
    (for ([post (mod-post-submodules md)]) (count-mod z post))
  ))

(define (count-provided z pd)
  ;; (-> zsc? provided? void?)
  (zsc++ z num-provided))
        
(define (count-expr z er)
  ;; (-> zsc? expr? void?)
  (begin
    (zsc++ z num-expr)
    (cond [(lam?            er) (count-lam            z er)]
          [(closure?        er) (count-closure        z er)]
          [(case-lam?       er) (count-case-lam       z er)]
          [(let-one?        er) (count-let-one        z er)]
          [(let-void?       er) (count-let-void       z er)]
          [(install-value?  er) (count-install-value  z er)]
          [(let-rec?        er) (count-let-rec        z er)]
          [(boxenv?         er) (count-boxenv         z er)]
          [(localref?       er) (count-localref       z er)]
          [(toplevel?       er) (count-toplevel       z er)]
          [(topsyntax?      er) (count-topsyntax      z er)]
          [(application?    er) (count-application    z er)]
          [(branch?         er) (count-branch         z er)]
          [(with-cont-mark? er) (count-with-cont-mark z er)]
          [(beg0?           er) (count-beg0           z er)]
          [(varref?         er) (count-varref         z er)]
          [(assign?         er) (count-assign         z er)]
          [(apply-values?   er) (count-apply-values   z er)]
          [(primval?        er) (count-primval        z er)]
          [else                 (error (format "[count-expr] Unknown expression form '~a'" er))])
  ))

;; Doesn't count, just dispatches to the right counter
(define (count-expr-or-seq z eos)
  ;; (-> zsc? (or/c expr? seq? any/c) void?)
  (cond [(expr? eos) (count-expr z eos)]
        [(seq?  eos) (count-seq  z eos)]
        [else        (void)]))

(define (count-lam z lm)
  ;; (-> zsc? lam? void?)
  (begin
    (zsc++ z num-lam)
    (count-expr-or-seq z (lam-body lm))
  ))

(define (count-closure z ce)
  ;; (-> zsc? closure? void?)
  (begin
    (zsc++ z num-closure)
    (count-lam z (closure-code ce))
  ))

(define (count-case-lam z cl)
  ;; (-> zsc? case-lam? void?)
  (begin
    (zsc++ z num-case-lam)
    (for ([lm (case-lam-clauses cl)]) (count-lam z lm))
  ))

(define (count-let-one z lo)
  ;; (-> zsc? let-one? void?)
  (begin
    (zsc++ z num-let-one)
    (count-expr-or-seq z (let-one-rhs  lo))
    (count-expr-or-seq z (let-one-body lo))
  ))

(define (count-let-void z lv)
  ;; (-> zsc? let-void? void?)
  (begin
    (zsc++ z num-let-void)
    (count-expr-or-seq z (let-void-body lv))
  ))

(define (count-install-value z iv)
  ;; (-> zsc? install-value? void?)
  (begin
    (zsc++ z num-install-value)
    (count-expr-or-seq z (install-value-rhs  iv))
    (count-expr-or-seq z (install-value-body iv))
  ))

(define (count-let-rec z lr)
  ;; (-> zsc? let-rec? void?)
  (begin
    (zsc++ z num-let-rec)
    (for ([pc (let-rec-procs lr)]) (count-lam z pc))
    (count-expr-or-seq z (let-rec-body lr))
  ))

(define (count-boxenv z bv)
  ;; (-> zsc? boxenv? void?)
  (begin
    (zsc++ z num-boxenv)
    (count-expr-or-seq z (boxenv-body bv))
  ))

(define (count-localref z lf)
  ;; (-> zsc? localref? void?)
  (zsc++ z num-localref))

(define (count-toplevel z tl)
  ;; (-> zsc? toplevel? void?)
  (zsc++ z num-toplevel))

(define (count-topsyntax z tx)
  ;; (-> zsc? topsyntax? void?)
  (zsc++ z num-topsyntax))

(define (count-application z an)
  ;; (-> zsc? application? void?)
  (begin
    (zsc++ z num-application)
    (count-expr-or-seq z (application-rator an))
    (for ([rs (application-rands an)]) (count-expr-or-seq z rs)) 
  ))

(define (count-branch z bh)
  ;; (-> zsc? branch? void?)
  (begin
    (zsc++ z num-branch)
    (count-expr-or-seq z (branch-test bh))
    (count-expr-or-seq z (branch-then bh))
    (count-expr-or-seq z (branch-else bh))
  ))

(define (count-with-cont-mark z wcm)
  ;; (-> zsc? with-cont-mark? void?)
  (begin
    (zsc++ z num-with-cont-mark)
    (count-expr-or-seq z (with-cont-mark-key  wcm))
    (count-expr-or-seq z (with-cont-mark-val  wcm))
    (count-expr-or-seq z (with-cont-mark-body wcm))
  ))

(define (count-beg0 z b0)
  ;; (-> zsc? beg0? void?)
  (begin
    (zsc++ z num-beg0)
    (for ([sq (beg0-seq b0)]) (count-expr-or-seq z sq))
  ))

(define (count-varref z vf)
  ;; (-> zsc? varref? void?)
  (begin
    (zsc++ z num-varref)
    (define tl (varref-toplevel vf))
    (when (toplevel? tl) (count-toplevel z tl))
    (define dm (varref-dummy    vf))
    (when (toplevel? dm) (count-toplevel z dm))
  ))

(define (count-assign z an)
  ;; (-> zsc? assign? void?)
  (begin
    (zsc++ z num-assign)
    (count-toplevel    z (assign-id  an))
    (count-expr-or-seq z (assign-rhs an))
  ))

(define (count-apply-values z av)
  ;; (-> zsc? apply-values? void?)
  (begin
    (zsc++ z num-apply-values)
    (count-expr-or-seq z (apply-values-proc      av))
    (count-expr-or-seq z (apply-values-args-expr av))
  ))

(define (count-primval z pl)
  ;; (-> zsc? primval? void?)
  (zsc++ z num-primval))

(define (count-wrapped z wd)
  ;; (-> zsc? wrapped? void?)
  (begin
    (zsc++ z num-wrapped)
    (for ([wp (wrapped-wraps wd)]) (count-wrap z wp))
  ))

(define (count-wrap z wp)
  ;; (-> zsc? wrap? void?)
  (begin
    (zsc++ z num-wrap)
    (cond [(top-level-rename? wp) (count-top-level-rename z wp)]
          [(mark-barrier?     wp) (count-mark-barrier     z wp)]
          [(lexical-rename?   wp) (count-lexical-rename   z wp)]
          [(phase-shift?      wp) (count-phase-shift      z wp)]
          [(module-rename?    wp) (count-module-rename    z wp)]
          [else                   (error (format "[count-wrap] Unknown wrap struct '~a'" wp))])
  ))

(define (count-top-level-rename z tlr)
  ;; (-> zsc? top-level-rename? void?)
  (zsc++ z num-top-level-rename))

(define (count-mark-barrier z mb)
  ;; (-> zsc? mark-barrier? void?)
  (zsc++ z num-mark-barrier))

(define (count-free-id-info z fii)
  ;; (-> zsc? free-id-info? void?)
  (zsc++ z num-free-id-info))

(define (count-lexical-rename z lr)
  ;; (-> zsc? lexical-rename? void?)
  (begin
    (zsc++ z num-lexical-rename)
    (for ([as (lexical-rename-alist lr)])
      (when (and (pair? (cdr as))
                 (free-id-info? (cdr (cdr as))))
        (count-free-id-info z (cdr (cdr as)))))
  ))

(define (count-phase-shift z ps)
  ;; (-> zsc? phase-shift? void?)
  (zsc++ z num-phase-shift))

(define (count-module-rename z mr)
  ;; (-> zsc? module-rename? void?)
  ; make-all-from-module ?
  (zsc++ z num-module-rename))

(define (count-all-from-module z afm)
  ;; (-> zsc? all-from-module? void?)
  (zsc++ z num-all-from-module))

;; 2014-12-09: I am ignoring all fields here
(define (count-module-binding z mb)
  ;; (-> zsc? module-binding? void?)
  (begin
    (zsc++ z num-module-binding)
    (cond [(simple-module-binding?           mb) (count-simple-module-binding           z mb)]
          [(phased-module-binding?           mb) (count-phased-module-binding           z mb)]
          [(exported-nominal-module-binding? mb) (count-exported-nominal-module-binding z mb)]
          [(nominal-module-binding?          mb) (count-nominal-module-binding          z mb)]
          [(exported-module-binding?         mb) (count-exported-module-binding         z mb)]
          [else                                  (error (format "[count-module-binding] Unknown module struct '~a'" mb))])
  ))

(define (count-simple-module-binding z smb)
  ;; (-> zsc? simple-module-binding? void?)
  (zsc++ z num-simple-module-binding))

(define (count-phased-module-binding z pmb)
  ;; (-> zsc? phased-module-binding? void?)
  (begin
    (zsc++ z num-phased-module-binding)
    (count-nominal-path z (phased-module-binding-nominal-path pmb))
  ))

(define (count-exported-nominal-module-binding z enmb)
  ;; (-> zsc? exported-nominal-module-binding? void?)
  (begin
    (zsc++ z num-exported-nominal-module-binding)
    (count-nominal-path (exported-nominal-module-binding-nominal-path enmb))
  ))

(define (count-nominal-module-binding z nmb)
  ;; (-> zsc? nominal-module-binding? void?)
  (begin
    (zsc++ z num-nominal-module-binding)
    (count-nominal-path (nominal-module-binding-nominal-path nmb))
  ))

(define (count-exported-module-binding z emb)
  ;; (-> zsc? exported-module-binding? void?)
  (zsc++ z num-exported-module-binding))

(define (count-nominal-path z np)
  ;; (-> zsc? nominal-path? void?
  (begin
    (zsc++ z num-nominal-path)
    (cond [(simple-nominal-path?   np) (count-simple-nominal-path   z np)]
          [(imported-nominal-path? np) (count-imported-nominal-path z np)]
          [(phased-nominal-path?   np) (count-phased-nominal-path   z np)]
          [else                        (error (format "[nominal-path] Unknown struct '~a'" np))])
  ))

(define (count-simple-nominal-path z snp)
  ;; (-> zsc? simple-nominal-path? void?
  (zsc++ z num-simple-nominal-path))

(define (count-imported-nominal-path z inp)
  ;; (-> zsc? imported-nominal-path? void?)
  (zsc++ z num-imported-nominal-path))

(define (count-phased-nominal-path z pnp)
  ;; (-> zsc? phased-nominal-path? void?)
  (zsc++ z num-phased-nominal-path))
