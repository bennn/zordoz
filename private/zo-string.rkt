#lang racket/base

;; Convert a zo struct to a more readable string representation.

;; Uses predicates to guess which struct we have, then convert the known
;; fields to strings.
;; Printing a field recursively is potentially expensive,
;; so we wrap the computation in a thunk.
;; The macro `lcons` makes thunk creation a little prettier.
;; The function `format-spec` forces these thunks.

;; Documentation for zo structs is online:
;; http://docs.racket-lang.org/raco/decompile.html

(provide
 ;; (->* (zo?) (#:deep? boolean?) string?)
 ;; Return a string representation of a zo struct
 zo->string
 ;; (->i ([z zo?]) () [res (z) (and/c spec/c (specof z))])
 ;; Return a list-of-strings representation of a zo struct.
 ;; The structure of the list mirrors the structure of the original zo struct.
 zo->spec
 ;; Contracts for conversion functions.
 spec/c
 specof)

;; --- string specifications

;; Contract for conversion functions.
;; A spec/c is the name of a zo struct and a list of pairs representing its fields:
;; - The car of each field is the name of that field
;; - The cdr of each field is a thunk for building a representation of the field's value.
;;   If the value is a zo-struct, the thunk should build a spec/c
;;   Otherwise, the thunk should build a string
(define spec/c
  (recursive-contract
   (cons/c string? (listof (cons/c string? (-> (or/c spec/c string?)))))))

;; Given a zo struct `z`, creates a predicate that accepts only specs with the
;; same number of elements as the struct `z` has fields (+1, for the title).
(define ((specof z) res)
  (= (length res) (vector-length (struct->vector z))))

(require compiler/zo-structs
         racket/contract
         racket/match
         (only-in racket/list   empty?)
         (only-in racket/string string-join)
         (for-syntax racket/base racket/syntax)
         (only-in "dispatch-table.rkt" make-table))

;; -----------------------------------------------------------------------------

;; --- API functions

;; Convert any zo struct to a spec/c representation.
(define/contract
  (zo->spec z)
  (->i ([z zo?]) () [res (z) (and/c spec/c (specof z))])
  (define z* (try-spec z))
  (if z*
      z*
      (error (format "Cannot format unknown struct ~e" z))))

;; Convert any zo struct to a string.
;; First builds a spec, then forces the thunks in that spec to build a string.
;; If `deep` is `#f`, only formats the name of the struct `z`.
(define/contract
  (zo->string z #:deep? [deep? #t])
  (->* (zo?) (#:deep? boolean?) string?)
  (format-spec deep? (zo->spec z)))

;; --- syntax: lazy cons to delay evaluation of tail

;; Introduces syntax (lcons a:any b:any).
;; Wraps second argument in a thunk.
(define-syntax (lcons stx)
  (syntax-case stx ()
    [(_)       (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ _)     (raise-syntax-error #f "[lcons] Expected two arguments.")]
    [(_ hd tl) #'(cons hd (lambda () tl))]))

;; --- dispatch tables

(define try-spec
  (make-table
   #:action ->spec
   compilation-top
   prefix
   global-bucket
   module-variable
   stx
   form
   expr
   stx-obj
   wrap
   module-shift
   scope
   multi-scope
   binding
   all-from-module))

(define form->spec
  (make-table
   #:action ->spec
   def-values
   def-syntaxes
   seq-for-syntax
   req
   seq
   splice
   inline-variant
   mod
   provided
   expr))

(define expr->spec
  (make-table
   #:action ->spec
   lam
   closure
   case-lam
   let-one
   let-void
   install-value
   let-rec
   boxenv
   localref
   toplevel
   topsyntax
   application
   branch
   with-cont-mark
   beg0
   varref
   assign
   apply-values
   primval))

(define binding->spec
  (make-table
   #:action ->spec
   module-binding
   decoded-module-binding
   local-binding
   free-id=?-binding))

;; --- private functions

(define
  (compilation-top->spec z)
  (list "compilation-top"
        (lcons "max-let-depth" (number->string      (compilation-top-max-let-depth z)))
        (lcons "prefix"        (prefix->spec        (compilation-top-prefix z)))
        (lcons "code"          (form-or-any->string (compilation-top-code z)))))

(define
  (prefix->spec z)
  (define (tl->spec tl)
    (match tl
      [(? module-variable?)
       (format-spec #f (module-variable->spec tl))]
      [(? global-bucket?)
       (format-spec #f (global-bucket->spec tl))]
      [(? symbol?)
       (symbol->string tl)]
      [#f "#f"]))
  (list "prefix"
        (lcons "num-lifts" (number->string              (prefix-num-lifts z)))
        (lcons "toplevels" (list->string      tl->spec  (prefix-toplevels z)))
        (lcons "stxs"      (listof-zo->string stx->spec (prefix-stxs z)))
        (lcons "src-inspector-desc" (symbol->string (prefix-src-inspector-desc z)))))

(define
  (global-bucket->spec  z)
  (list "global-bucket"
        (lcons "name" (symbol->string (global-bucket-name z)))))

(define
  (module-variable->spec z)
  (define (constantness->spec cs)
    (cond [(symbol? cs)         (symbol->string         cs)]
          [(function-shape? cs) (function-shape->spec cs)]
          [(struct-shape? cs)   (struct-shape->spec   cs)]
          [else                 "#f"]))
  (list "module-variable"
        (lcons "modidx"       (module-path-index->string (module-variable-modidx z)))
        (lcons "sym"          (symbol->string            (module-variable-sym z)))
        (lcons "pos"          (number->string            (module-variable-pos z)))
        (lcons "phase"        (number->string            (module-variable-phase z)))
        (lcons "constantness" (constantness->spec      (module-variable-constantness z)))))

(define
  (stx->spec z)
  (list "stx"
        (lcons "content" (stx-obj->spec (stx-content z)))))

(define
  (all-from-module->spec z)
  (list "all-from-module"
        (lcons "path"      (module-path-index->string (all-from-module-path z)))
        (lcons "phase"     (number-or-f->string (all-from-module-phase z)))
        (lcons "src-phase" (number-or-f->string (all-from-module-src-phase z)))
        (lcons "inspector-desc" (symbol->string (all-from-module-inspector-desc z)))
        (lcons "exceptions" (list->string symbol->string (all-from-module-exceptions z)))
        (lcons "prefix"    (symbol-or-f->string (all-from-module-prefix z)))))

;; --- form

(define
  (def-values->spec z)
  (list "def-values"
        (lcons "ids" (list->string toplevel-or-symbol->string (def-values-ids z)))
        (lcons "rhs" (let ([rhs (def-values-rhs z)])
                       (cond [(inline-variant? rhs) (inline-variant->spec rhs)]
                             [else (expr-seq-any->string rhs)])))))

(define
  (def-syntaxes->spec z)
  (list "def-syntaxes"
        (lcons "ids"           (list->string toplevel-or-symbol->string (def-syntaxes-ids z)))
        (lcons "rhs"           (expr-seq-any->string                    (def-syntaxes-rhs z)))
        (lcons "prefix"        (prefix->spec                            (def-syntaxes-prefix z)))
        (lcons "max-let-depth" (number->string                          (def-syntaxes-max-let-depth z)))
        (lcons "dummy"         (toplevel-or-any->string                 (def-syntaxes-dummy z)))))

(define
  (seq-for-syntax->spec z)
  (list "seq-for-syntax"
        (lcons "forms"         (listof-form-or-any->string (seq-for-syntax-forms z)))
        (lcons "prefix"        (prefix->spec             (seq-for-syntax-prefix z)))
        (lcons "max-let-depth" (number->string             (seq-for-syntax-max-let-depth z)))
        (lcons "dummy"         (toplevel-or-any->string    (seq-for-syntax-dummy z)))))

(define
  (req->spec z)
  (list "req"
        (lcons "reqs"  (stx->spec      (req-reqs z)))
        (lcons "dummy" (toplevel->spec (req-dummy z)))))

(define
  (seq->spec z)
  (list "seq"
        (lcons "forms" (listof-form-or-any->string (seq-forms z)))))

(define
  (splice->spec z)
  (list "splice"
        (lcons "forms" (listof-form-or-any->string (splice-forms z)))))

(define
  (inline-variant->spec z)
  (list "inline-variant"
        (lcons "direct" (expr->spec (inline-variant-direct z)))
        (lcons "inline" (expr->spec (inline-variant-inline z)))))

(define
  (mod->spec z)
  (define (name->spec nm)
    (match nm
      [(? list?)
       (list->string  symbol->string nm)]
      [(? symbol?)
       (symbol->string nm)]))
  (define (unexported->spec ux)
    (define (elem->spec e)
      (format-list
       #:sep " "
       (list (number->string              (car e))
             (list->string symbol->string (cadr e))
             (list->string symbol->string (caddr e)))))
    (list->string elem->spec ux))
  (define (lang-info->spec li)
    (match li
      [(vector mp sym any)
        (format-list
         #:sep " "
         (list (module-path->spec mp)
               (symbol->string    sym)
               (any->string       any)))]
      [#f "#f"]))
  (define;/contract
    (provides->spec pds)
    ;(-> (listof (list/c (or/c exact-integer? #f) (listof provided?) (listof provided?))) string?)
    (define (elem->spec e)
      (format-list
       #:sep " "
       (list (if (number? (car e))
                 (number->string (car e))
                 "#f")
             (listof-zo->string provided->spec (cadr e))
             (listof-zo->string provided->spec (caddr e)))))
    (list->string elem->spec pds))
  (define;/contract
    (requires->spec rqs)
    ;(-> (listof (cons/c (or/c exact-integer? #f) (listof module-path-index?))) string?)
    (define (elem->spec e)
      (format-list
       #:sep " "
       (list (if (number? (car e))
                 (number->string (car e))
                 "#f")
             (list->string module-path-index->string (cdr e)))))
    (list->string elem->spec rqs))
  (define;/contract
    (syntax-bodies->spec sbs)
    ;(-> (listof (cons/c exact-positive-integer? (listof (or/c def-syntaxes? seq-for-syntax?)))) string?)
    (define (ds-or-sfs->spec d)
      (cond [(def-syntaxes?   d) (format-spec #f (def-syntaxes->spec d))]
            [(seq-for-syntax? d) (format-spec #f (seq-for-syntax->spec d))]))
    (define (elem->spec e)
      (format-list
       #:sep " "
       (list (number->string                 (car e))
             (list->string ds-or-sfs->spec (cdr e)))))
    (list->string elem->spec sbs))
  (define (internal-context->string ic)
    (match ic
      [(? stx? ic)
       (stx->spec ic)]
      [(? vector? ic)
       (listof-zo->string stx->spec (vector->list ic))]
      [(? boolean? ic)
       (boolean->string ic)]))
  (define
    (binding-names->string bns)
    ;(-> (hash/c exact-integer? (hash/c symbol (or/c #t stx?))))
    (any->string bns))
  (list "mod"
        (lcons "name"             (name->spec               (mod-name z)))
        (lcons "srcname"          (symbol->string             (mod-srcname z)))
        (lcons "self-modidx"      (module-path-index->string  (mod-self-modidx z)))
        (lcons "prefix"           (prefix->spec             (mod-prefix z)))
        (lcons "provides"         (provides->spec       (mod-provides z)))
        (lcons "requires"         (requires->spec       (mod-requires z)))
        (lcons "body"             (listof-form-or-any->string (mod-body z)))
        (lcons "syntax-bodies"    (syntax-bodies->spec  (mod-syntax-bodies z)))
        (lcons "unexported"       (unexported->spec         (mod-unexported z)))
        (lcons "max-let-depth"    (number->string             (mod-max-let-depth z)))
        (lcons "dummy"            (toplevel->spec           (mod-dummy z)))
        (lcons "lang-info"        (lang-info->spec          (mod-lang-info z)))
        (lcons "internal-context" (internal-context->string (mod-internal-context z)))
        (lcons "binding-names"    (binding-names->string (mod-binding-names z)))
        (lcons "flags"            (list->string   symbol->string (mod-flags z)))
        (lcons "pre-submodules"   (listof-zo->string mod->spec (mod-pre-submodules z)))
        (lcons "post-submodules"  (listof-zo->string mod->spec (mod-post-submodules z)))))

(define
  (provided->spec z)
  (define (mpi-or-f->string x)
    (if (eq? #f x)
        "#f"
        (module-path-index->string x)))
  (list "provided"
        (lcons "name"      (symbol->string (provided-name z)))
        (lcons "src"       (mpi-or-f->string (provided-src  z)))
        (lcons "src-name"  (symbol->string (provided-src-name z)))
        (lcons "nom-src"   (any->string (provided-nom-src z)))
        (lcons "src-phase" (number->string (provided-src-phase z)))
        (lcons "protected?" (boolean->string (provided-protected? z)))))

;; --- expr

;; Helper for `lam` and `case-lam`.
(define (lam-name->spec nm)
  (match nm
    [(? vector?)
     (any->string nm)]
    [(? empty?)
     "()"]
    [(? symbol?)
     (symbol->string nm)]))

(define
  (lam->spec z)
  (define (closure-map->spec cm)
    (list->string number->string (vector->list cm)))
  (define (toplevel-map->spec tm)
    (cond [(eq? #f tm) "#f"]
          [else (format-list #:sep " " (for/list ([n tm]) (number->string n)))]))
  (list "lam"
        (lcons "name"          (lam-name->spec                  (lam-name z)))
        (lcons "flags"         (list->string symbol->string (lam-flags z)))
        (lcons "num-params"    (number->string              (lam-num-params z)))
        (lcons "param-types"   (list->string symbol->string (lam-param-types z)))
        (lcons "rest?"         (boolean->string             (lam-rest? z)))
        (lcons "closure-map"   (closure-map->spec           (lam-closure-map z)))
        (lcons "closure-types" (list->string symbol->string (lam-closure-types z)))
        (lcons "toplevel-map"  (toplevel-map->spec          (lam-toplevel-map z)))
        (lcons "max-let-depth" (number->string              (lam-max-let-depth z)))
        (lcons "body"          (expr-seq-any->string        (lam-body z)))))

(define
  (closure->spec z)
  (list "closure"
        (lcons "code"   (lam->spec    (closure-code z)))
        (lcons "gen-id" (symbol->string (closure-gen-id z)))))

(define
  (case-lam->spec z)
  (list "case-lam"
        (lcons "name"    (lam-name->spec              (case-lam-name z)))
        (lcons "clauses" (list->string (lambda (x) (format-spec #f (expr->spec x))) (case-lam-clauses z)))))

(define
  (let-one->spec z)
  (list "let-one"
        (lcons "rhs"    (expr-seq-any->string (let-one-rhs  z)))
        (lcons "body"   (expr-seq-any->string (let-one-body z)))
        (lcons "type"   (symbol-or-f->string  (let-one-type z)))
        (lcons "unused?" (boolean->string      (let-one-unused? z)))))

(define
  (let-void->spec z)
  (list "let-void"
        (lcons "count" (number->string       (let-void-count z)))
        (lcons "boxes" (boolean->string      (let-void-boxes? z)))
        (lcons "body"  (expr-seq-any->string (let-void-body z)))))

(define
  (install-value->spec z)
  (list "install-value"
        (lcons "count"  (number->string       (install-value-count z)))
        (lcons "pos"    (number->string       (install-value-pos z)))
        (lcons "boxes?" (boolean->string      (install-value-boxes? z)))
        (lcons "rhs"    (expr-seq-any->string (install-value-rhs z)))
        (lcons "body"   (expr-seq-any->string (install-value-body z)))))

(define
  (let-rec->spec z)
  (list "let-rec"
        (lcons "procs" (listof-zo->string lam->spec (let-rec-procs z)))
        (lcons "body"  (expr-seq-any->string          (let-rec-body z)))))

(define
  (boxenv->spec z)
  (list "boxenv"
        (lcons "pos"  (number->string       (boxenv-pos z)))
        (lcons "body" (expr-seq-any->string (boxenv-body z)))))

(define
  (localref->spec z)
  (list "localref"
        (lcons "unbox?"        (boolean->string     (localref-unbox? z)))
        (lcons "pos"           (number->string      (localref-pos z)))
        (lcons "clear?"        (boolean->string     (localref-clear? z)))
        (lcons "other-clears?" (boolean->string     (localref-other-clears? z)))
        (lcons "type"          (symbol-or-f->string (localref-type z)))))

(define
  (toplevel->spec z)
  (list
        "toplevel"
        (lcons "depth"  (number->string  (toplevel-depth z)))
        (lcons "pos"    (number->string  (toplevel-pos z)))
        (lcons "const?" (boolean->string (toplevel-const? z)))
        (lcons "ready?" (boolean->string (toplevel-ready? z)))))

(define
  (topsyntax->spec z)
  (list "topsyntax"
        (lcons "depth" (number->string (topsyntax-depth z)))
        (lcons "pos"   (number->string (topsyntax-pos z)))
        (lcons "midpt" (number->string (topsyntax-midpt z)))))

(define
  (application->spec z)
  (list "application"
        (lcons "rator" (expr-seq-any->string              (application-rator z)))
        (lcons "rands" (list->string expr-seq-any->string (application-rands z)))))

(define
  (branch->spec z)
  (list "branch"
        (lcons "test" (expr-seq-any->string (branch-test z)))
        (lcons "then" (expr-seq-any->string (branch-then z)))
        (lcons "else" (expr-seq-any->string (branch-else z)))))

(define
  (with-cont-mark->spec z)
  (list "with-cont-mark"
        (lcons "key"  (expr-seq-any->string (with-cont-mark-key  z)))
        (lcons "val"  (expr-seq-any->string (with-cont-mark-val  z)))
        (lcons "body" (expr-seq-any->string (with-cont-mark-body z)))))

(define
  (beg0->spec z)
  (list "beg0"
        (lcons "seq" (list->string expr-seq-any->string (beg0-seq z)))))

(define
  (varref->spec z)
  (list "varref"
        (lcons "toplevel" (match (varref-toplevel z)
                            [(? toplevel? tl) (toplevel->spec tl)]
                            [#t    "#t"]))
        (lcons "dummy"    (match (varref-dummy z)
                            [(? toplevel? dm) (toplevel->spec dm)]
                            [#f "#f"]))))

(define
  (assign->spec z)
  (list "assign"
        (lcons "id"        (toplevel->spec     (assign-id z)))
        (lcons "rhs"       (expr-seq-any->string (assign-rhs z)))
        (lcons "undef-ok?" (boolean->string      (assign-undef-ok? z)))))

(define
  (apply-values->spec z)
  (list "apply-values"
        (lcons "proc"      (expr-seq-any->string (apply-values-proc z)))
        (lcons "args-expr" (expr-seq-any->string (apply-values-args-expr z)))))

(define
  (primval->spec z)
  (list "primval"
        (lcons "id" (number->string (primval-id z)))))

;; --- stx-obj

(define
  (stx-obj->spec so)
  (list "stx-obj"
        (lcons "datum" (any->string (stx-obj-datum so)))
        (lcons "wrap" (wrap->spec (stx-obj-wrap so)))
        (lcons "tamper-status" (symbol->string (stx-obj-tamper-status so)))))

;; --- wrap

(define
  (wrap->spec wp)
  (define (ms->string ms+id)
    (format "(~a ~a)" (format-spec #f (multi-scope->spec (car ms+id)))
                      (number-or-f->string (cadr ms+id))))
  (list "wrap"
        (lcons "shifts" (listof-zo->string module-shift->spec (wrap-shifts wp)))
        (lcons "simple-scopes" (listof-zo->string scope->spec (wrap-simple-scopes wp)))
        (lcons "multi-scopes" (list->string ms->string (wrap-multi-scopes wp)))))

;; --- misc. syntax

(define
  (module-shift->spec ms)
  (list "module-shift"
        (lcons "from" (module-path-index->string (module-shift-from ms)))
        (lcons "to" (module-path-index->string (module-shift-to ms)))
        (lcons "from-inspector-desc" (symbol-or-f->string (module-shift-from-inspector-desc ms)))
        (lcons "to-inspector-desc" (symbol-or-f->string (module-shift-to-inspector-desc ms)))))

(define
  (scope->spec sc)
  (define (sym+scope+binding->string ssbs)
    (format "(~a ~a ~a)" (symbol->string (car ssbs))
                         (listof-zo->string scope->spec (cadr ssbs))
                         (format-spec #f (binding->spec (caddr ssbs)))))
  (define (scope+all-from-module->string bbs)
    (format "(~a ~a)" (listof-zo->string scope->spec (car bbs))
                      (format-spec #f (all-from-module->spec (cadr bbs)))))
  (list "scope"
        (lcons "name" (let ([name (scope-name sc)])
                        (cond [(eq? 'root name) "root"]
                              [else (number->string name)])))
        (lcons "kind" (symbol->string (scope-kind sc)))
        (lcons "bindings" (list->string sym+scope+binding->string (scope-bindings sc)))
        (lcons "bulk-bindings" (list->string scope+all-from-module->string (scope-bulk-bindings sc)))
        (lcons "multi-owner" (cond [(scope-multi-owner sc) => multi-scope->spec]
                                   [else "#f"]))))

(define
  (multi-scope->spec ms)
  (define (sc->string id+scope)
    (format "(~a ~a)" (number-or-f->string (car id+scope))
                      (format-spec #f (scope->spec (cadr id+scope)))))
  (list "multi-scope"
        (lcons "name" (number->string (multi-scope-name ms)))
        (lcons "src-name" (any->string (multi-scope-src-name ms)))
        (lcons "scopes" (list->string sc->string (multi-scope-scopes ms)))))

;; --- binding

(define
  (module-binding->spec mb)
  (list "module-binding"
        (lcons "encoded" (any->string (module-binding-encoded mb)))))

(define
  (decoded-module-binding->spec dmb)
  (list "decoded-module-binding"
        (lcons "path" (module-path-index->string (decoded-module-binding-path dmb)))
        (lcons "name" (symbol->string (decoded-module-binding-name dmb)))
        (lcons "phase" (number->string (decoded-module-binding-phase dmb)))
        (lcons "nominal-path" (module-path-index->string (decoded-module-binding-nominal-path dmb)))
        (lcons "nominal-export-name" (symbol->string (decoded-module-binding-nominal-export-name dmb)))
        (lcons "nominal-phase" (number-or-f->string (decoded-module-binding-nominal-phase dmb)))
        (lcons "import-phase" (number-or-f->string (decoded-module-binding-import-phase dmb)))
        (lcons "inspector-desc" (symbol-or-f->string (decoded-module-binding-inspector-desc dmb)))))

(define
  (local-binding->spec lb)
  (list "local-binding"
        (lcons "name" (symbol->string (local-binding-name lb)))))

(define
  (free-id=?-binding->spec fib)
  (list "free-id=?-binding"
        (lcons "base" (binding->spec (free-id=?-binding-base fib)))
        (lcons "id" (stx-obj->spec (free-id=?-binding-id fib)))
        (lcons "phase" (number-or-f->string (free-id=?-binding-phase fib)))))

;; --- Shapes

;; Shapes are not zo structs per se, but they are documented in the
;; decompile guide and do not seem to have a nice formatting method.

(define
  (function-shape->spec fs)
  (format-list #:sep " "
               (list "function-shape"
                     (format "arity : ~a"            (function-shape-arity fs))
                     (format "preserves-marks? : ~a" (function-shape-preserves-marks? fs)))))

(define
  (struct-shape->spec ss)
  (cond [(struct-type-shape?  ss) (struct-type-shape->spec  ss)]
        [(constructor-shape?  ss) (constructor-shape->spec  ss)]
        [(predicate-shape?    ss) (predicate-shape->spec    ss)]
        [(accessor-shape?     ss) (accessor-shape->spec     ss)]
        [(mutator-shape?      ss) (mutator-shape->spec      ss)]
        [(struct-other-shape? ss) (struct-other-shape->spec ss)]
        [else (error (format "unknown struct shape ~a" ss))]))

(define
  (struct-type-shape->spec sts)
  (format-list #:sep " "
               (list "struct-type-shape"
                     (format "field-count : ~a" (struct-type-shape-field-count sts)))))

(define
  (constructor-shape->spec cs)
  (format-list #:sep " "
               (list "constructor-shape"
                     (format "arity : ~a" (constructor-shape-arity cs)))))

(define
  (predicate-shape->spec ps)
  (format-list (list "predicate-shape")))

(define
  (accessor-shape->spec sts)
  (format-list #:sep " "
               (list "accessor-shape"
                     (format "field-count : ~a" (accessor-shape-field-count sts)))))

(define
  (mutator-shape->spec sts)
  (format-list #:sep " "
               (list "mutator-shape"
                     (format "field-count : ~a" (mutator-shape-field-count sts)))))

(define
  (struct-other-shape->spec ps)
  (format-list (list "struct-other-shape")))

;; --- helpers

;; Turn any value into a string.
(define
  (any->string z)
  (format "~a" z))

;; Turn a boolean value into a string.
(define
  (boolean->string b)
  (any->string b))

;; Turn an 'expr' struct or a 'seq' struct or any other value into a string.
(define
  (expr-seq-any->string z)
  (cond [(expr? z) (format-spec #f (expr->spec z))]
        [(seq?  z) (format-spec #f (seq->spec z))]
        [else      (any->string z)]))

;; Turn a 'form' struct or anything else into a string.
(define
  (form-or-any->string fm)
  (cond [(form? fm) (format-spec #f (form->spec fm))]
        [else       (any->string   fm)]))

;; Alternate syntax for `string-join` -- the `sep` argument appears as a label
;; and defaults to a newline character.
(define
  (format-list xs #:sep [sep "\n"])
  (string-join xs sep))

;; Turn a spec into a string.
;; If `deep?` is false, only format the title (ignore the field names + thunks).
(define
  (format-spec deep? struct-spec)
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
                                              (format-spec #f forced)))
                           (format "  ~a : ~a" (pad (car fd) w) rest))))))

;; Turn a list into a string.
(define
  (list->string f xs)
  (format "[~a]"
          (format-list #:sep " "
                       (for/list ([x xs]) (f x)))))

;; Turn a list of things that might be 'form' structs into a list of strings.
(define
  (listof-form-or-any->string xs)
  (list->string form-or-any->string xs))

;; Turn a list of zo structs into a list of strings using the helper function
;; `z->spec`.
(define
  (listof-zo->string z->spec zs)
  (cond [(empty? zs) "[]"]
        [else        (format "~a[~a]" (format-spec #f (z->spec (car zs))) (length zs))]))

;; Turn a module-path-index into a string
;; TODO I think we can do better than ~a
;; http://docs.racket-lang.org/reference/Module_Names_and_Loading.html
(define
  (module-path-index->string mpi)
  (any->string mpi))

;; Turn a module path into a string
;; TODO can probably improve on ~a
(define
  (module-path->spec mp)
  (any->string mp))

;; Turn a number or #f into a string.
(define
  (number-or-f->string nf)
  (if (eq? #f nf)
      "#f"
      (number->string nf)))

;; Turn a symbol or #f into a string.
(define
  (symbol-or-f->string sf)
  (if (eq? #f sf)
      "#f"
      (symbol->string sf)))

;; Turn something that might be a 'toplevel' struct into a string.
(define
  (toplevel-or-any->string tl)
  (cond [(toplevel? tl) (format-spec #f (toplevel->spec tl))]
        [else           (any->string tl)]))

(define
  (toplevel-or-symbol->string tl)
  (match tl
    [(? toplevel?)
     (format-spec #f (toplevel->spec tl))]
    [(? symbol?)
     (symbol->string tl)]))

;; --- misc

;; If `str` has fewer than `w` characters,
;; append `(w - (len str))` characters to its right end.
(define
  (pad str w #:char [c #\space])
  (define l (string-length str))
  (cond [(< l w) (format "~a~a" str (make-string (- w l) c))]
        [else    str]))

;; -----------------------------------------------------------------------------
;; --- testing

(module+ test
  (require rackunit
           compiler/zo-structs)

  ; Helper: force lazy tails so we can compare them.
  (define (force-spec sp)
    (cons (car sp) (for/list ([xy (cdr sp)]) (cons (car xy)
                                                   (let ([tl ((cdr xy))])
                                                     (if (string? tl)
                                                         tl
                                                         (format-spec #f tl)))))))

  ;; --- API functions
  ;; zo->spec
  (check-exn exn:fail? (lambda () (zo->spec (zo))))
  (check-equal? (force-spec (zo->spec (branch #t #f #t)))
                (list "branch"
                      (cons "test" "#t")
                      (cons "then" "#f")
                      (cons "else" "#t")))

  ;; zo->string
  (check-exn exn:fail? (lambda () (zo->string (zo))))
  (check-equal? (zo->string (toplevel 1 1 #t #t)) "<struct:toplevel>\n  depth  : 1\n  pos    : 1\n  const? : #t\n  ready? : #t")
  (check-equal? (zo->string #:deep? #t (toplevel 1 1 #t #t)) "<struct:toplevel>\n  depth  : 1\n  pos    : 1\n  const? : #t\n  ready? : #t")
  (check-equal? (zo->string #:deep? #f (toplevel 1 1 #t #t)) "<struct:toplevel>")

  ;; --- private

  ;; compilation-top->spec
  (let* ([px (prefix 0 '() '() 'd)]
         [cd (seq '())]
         [z  (compilation-top 0 px cd)])
    (check-equal? (force-spec (compilation-top->spec z))
                  (cons "compilation-top"
                        (list (cons "max-let-depth" "0")
                              (cons "prefix" "<struct:prefix>")
                              (cons "code" "<struct:seq>")))))

  ;; prefix->spec
  (let* ([mpi (module-path-index-join #f #f)]
         [gb (global-bucket 'NAME)]
         [mv (module-variable mpi 'SYM 0 0 #f)]
         [sx (stx (stx-obj 'x (wrap '() '() '()) 'tainted))]
         [z  (prefix 0
                    (list gb mv)
                    (list sx)
                    'yes)])
    (check-equal? (force-spec (prefix->spec z))
                  (cons "prefix"
                        (list (cons "num-lifts" "0")
                              (cons "toplevels" "[<struct:global-bucket> <struct:module-variable>]")
                              (cons "stxs" "<struct:stx>[1]")
                              (cons "src-inspector-desc" "yes")))))

  ;; global-bucket->spec
  (let* ([z (global-bucket 'arbitrary-symbol)])
    (check-equal? (force-spec (global-bucket->spec z))
                  (cons "global-bucket"
                        (list (cons "name" "arbitrary-symbol")))))

  ;; module-variable->spec
  (let* ([mpi (module-path-index-join #f #f)]
         [fs  (function-shape 1 #f)]
         [ss  (struct-shape)] 
         [z   (module-variable mpi 'arbitrary 999 9001 fs)])
    (check-equal? (force-spec (module-variable->spec z))
                  (cons "module-variable"
                        (list (cons "modidx" "#<module-path-index:()>")
                              (cons "sym" "arbitrary")
                              (cons "pos" "999")
                              (cons "phase" "9001")
                              (cons "constantness" "function-shape arity : 1 preserves-marks? : #f")))))

  ;; stx->spec
  (let* ([s (stx-obj 'a (wrap '() '() '()) 'clean)]
         [z (stx s)])
    (check-equal? (force-spec (stx->spec z))
                  (cons "stx"
                        (list (cons "content" "<struct:stx-obj>")))))

  ;; form->spec (see below)
  (let* ([z (form)])
    (check-equal? (form->spec z) #f))

  ;; expr->spec (see below)
  (let* ([z (expr)])
    (check-equal? (expr->spec z) #f))

  ;; stx-obj->spec
  (let* ([z (stx-obj 'datum (wrap '() '() '()) 'clean)])
    (check-equal? (force-spec (stx-obj->spec z))
                  (cons "stx-obj"
                        (list (cons "datum" "datum")
                              (cons "wrap"  "<struct:wrap>")
                              (cons "tamper-status" "clean")))))

  ;; wrap->spec
  (let* ([ms (module-shift #f #f 'a 'b)]
         [sc (scope 3 'b '() '() #f)]
         [msc (multi-scope 1 '2 '())]
         [z (wrap (list ms) (list sc) (list (list msc 5)))])
    (check-equal? (force-spec (wrap->spec z))
                  (cons "wrap"
                        (list (cons "shifts" "<struct:module-shift>[1]")
                              (cons "simple-scopes" "<struct:scope>[1]")
                              (cons "multi-scopes" "[(<struct:multi-scope> 5)]")))))

  ;; module-shift->spec
  (let* ([mpi (module-path-index-join #f #f)]
         [z (module-shift mpi mpi 'from 'to)])
    (check-equal? (force-spec (module-shift->spec z))
                  (cons "module-shift"
                        (list (cons "from" "#<module-path-index:()>")
                              (cons "to"   "#<module-path-index:()>")
                              (cons "from-inspector-desc" "from")
                              (cons "to-inspector-desc" "to")))))

  ;; scope->spec
  (let* ([mpi (module-path-index-join #f #f)]
         [afm (all-from-module mpi #f #f 'x '() #f)]
         [b (binding)]
         [s (scope 5 'blah '() '() #f)]
         [ms (multi-scope 6 'name '())]
         [z (scope 'root 'kind `((sym (,s) ,(module-binding #t))) `(((,s) ,afm)) ms)])
    (check-equal? (force-spec (scope->spec z))
                  (cons "scope"
                        (list (cons "name" "root")
                          (cons "kind" "kind")
                          (cons "bindings" "[(sym <struct:scope>[1] <struct:module-binding>)]")
                          (cons "bulk-bindings" "[(<struct:scope>[1] <struct:all-from-module>)]")
                          (cons "multi-owner" "<struct:multi-scope>")))))

  ;; multi-scope->spec
  (let* ([s (scope 1 'a '() '() #f)]
         [z (multi-scope 4 'yolo `((#f ,s) (1 ,s)))])
    (check-equal? (force-spec (multi-scope->spec z))
                  (cons "multi-scope"
                        (list (cons "name" "4")
                              (cons "src-name" "yolo")
                              (cons "scopes" "[(#f <struct:scope>) (1 <struct:scope>)]")))))

  ;; binding->spec (see below)
  (let* ([z (binding)])
    (check-equal? (binding->spec z) #f))

  ;; all-from-module->spec
  (let* ([mpi (module-path-index-join #f #f)]
         [z (all-from-module mpi 2 3 'gadget '(ex1 ex2) 'pre)])
    (check-equal? (force-spec (all-from-module->spec z))
                  (cons "all-from-module"
                        (list (cons "path" "#<module-path-index:()>")
                              (cons "phase" "2")
                              (cons "src-phase" "3")
                              (cons "inspector-desc" "gadget")
                              (cons "exceptions" "[ex1 ex2]")
                              (cons "prefix" "pre")))))

  ;; module-binding->spec (see below)
  (let* ([z (module-binding 'any)])
    (check-equal? (force-spec (module-binding->spec z))
                  (cons "module-binding"
                        (list (cons "encoded" "any")))))

  ;; def-values->spec
  (let* ([ids (list (toplevel 1 2 #t #f))]
         [rhs (beg0 '())]
         [z (def-values ids rhs)])
    (check-equal? (force-spec (def-values->spec z))
                  (cons "def-values"
                        (list (cons "ids" "[<struct:toplevel>]")
                              (cons "rhs" "<struct:beg0>")))))

  ;; def-syntaxes->
  (let* ([ids (list (toplevel 1 2 #t #f))]
         [rhs (beg0 '())]
         [px  (prefix 0 '() '() 'yay)]
         [dm  (toplevel 1 1 #t #t)]
         [z   (def-syntaxes ids rhs px 42 dm)])
    (check-equal? (force-spec (def-syntaxes->spec z))
                  (cons "def-syntaxes"
                        (list (cons "ids" "[<struct:toplevel>]")
                              (cons "rhs" "<struct:beg0>")
                              (cons "prefix" "<struct:prefix>")
                              (cons "max-let-depth" "42")
                              (cons  "dummy" "<struct:toplevel>")))))

  ;; seq-for-syntax->spec
  (let* ([fms (list (seq '()))]
         [px  (prefix 0 '() '() 'hi)]
         [dm  (toplevel 9 9 #t #t)]
         [z   (seq-for-syntax fms px 8 dm)])
    (check-equal? (force-spec (seq-for-syntax->spec z))
                  (cons "seq-for-syntax"
                        (list (cons "forms" "[<struct:seq>]")
                              (cons "prefix" "<struct:prefix>")
                              (cons "max-let-depth" "8")
                              (cons "dummy" "<struct:toplevel>")))))

  ;; req->spec
  (let* ([sx (stx (stx-obj 'XXX (wrap '() '() '()) 'clean))]
         [dm (toplevel 1 1 #t #t)]
         [z  (req sx dm)])
    (check-equal? (force-spec (req->spec z))
                  (cons "req"
                        (list (cons "reqs" "<struct:stx>")
                              (cons "dummy" "<struct:toplevel>")))))

  ;; seq->spec
  (let* ([fms (list (seq '()) (seq '()) (seq '()))]
         [z   (seq fms)])
    (check-equal? (force-spec (seq->spec z))
                  (cons "seq"
                        (list (cons "forms" "[<struct:seq> <struct:seq> <struct:seq>]")))))


  ;; splice->
  (let* ([fms (list (seq '()) (seq '()))]
         [z   (splice fms)])
    (check-equal? (force-spec (splice->spec z))
                  (cons "splice"
                        (list (cons "forms" "[<struct:seq> <struct:seq>]")))))

  ;; inline-variant->spec
  (let* ([dr (beg0 '())]
         [il (beg0 '())]
         [z  (inline-variant dr il)])
    (check-equal? (force-spec (inline-variant->spec z))
                  (cons "inline-variant"
                        (list (cons "direct" "<struct:beg0>")
                              (cons "inline" "<struct:beg0>")))))

  ;; mod->spec
  (let* ([mpi (module-path-index-join #f #f)]
         [px  (prefix 0 '() '() 'a)]
         [pd1 (provided 'p1 #f 'B 'C 13 #t)]
         [pd2 (provided 'p2 #f 'asdf 'C 6 #f)]
         [pd3 (provided 'p3 #f 'B 'lol 832 #t)]
         [pd4 (provided 'p4 #f 'R 'xx 1 #t)]
         [pvs (list (list #f (list pd1 pd2) (list pd3))
                    (list #f (list pd4) '()))]
         [bd  (list (seq '()) 'any)]
         [ds  (def-syntaxes '() (beg0 '()) (prefix 0 '() '() 'b) 1 #f)]
         [sfs (seq-for-syntax '() (prefix 0 '() '() 'c) 999 (toplevel 9 9 #t #t))]
         [sb  (list (cons 7 (list ds))
                    (cons 8 (list sfs)))]
         [dm  (toplevel 1 1 #f #f)]
         [ic  (stx (stx-obj 'dirty (wrap '() '() '()) 'clean))]
         [m1  (mod 'm1 'm1src mpi px pvs '() bd sb '() 0 dm #f ic (hash) '() '() '())]
         [m2  (mod 'm2 'm2src mpi px pvs '() bd sb '() 0 dm #f ic (hash) '() '() '())]
         [m3  (mod 'm3 'm3src mpi px pvs '() bd sb '() 0 dm #f ic (hash) '() '() '())]
         [bn  (hash 1 (hash 'something #t))]
         [prs (list m1 m2)]
         [pts (list m3)]
         [z   (mod 'name 'srcname mpi px pvs '() bd sb '() 0 dm #f ic bn '() prs pts)])
    (check-equal? (force-spec (mod->spec z))
                  (cons "mod"
                        (list (cons "name" "name")
                              (cons "srcname" "srcname")
                              (cons "self-modidx" "#<module-path-index:()>")
                              (cons "prefix" "<struct:prefix>")
                              (cons "provides" "[#f <struct:provided>[2] <struct:provided>[1] #f <struct:provided>[1] []]")
                              (cons "requires" "[]")
                              (cons "body" "[<struct:seq> any]")
                              (cons "syntax-bodies" "[7 [<struct:def-syntaxes>] 8 [<struct:seq-for-syntax>]]")
                              (cons "unexported" "[]")
                              (cons "max-let-depth" "0")
                              (cons "dummy" "<struct:toplevel>")
                              (cons "lang-info" "#f")
                              (cons "internal-context" "<struct:stx>")
                              (cons "binding-names" "#hash((1 . #hash((something . #t))))")
                              (cons "flags" "[]")
                              (cons "pre-submodules" "<struct:mod>[2]")
                              (cons "post-submodules" "<struct:mod>[1]")))))

  ;; provided->spec
  (let* ([z (provided 'name #f 'srcname 'nomnom 12 #t)])
    (check-equal? (force-spec (provided->spec z))
                  (cons "provided"
                        (list (cons "name" "name")
                              (cons "src" "#f")
                              (cons "src-name" "srcname")
                              (cons "nom-src" "nomnom")
                              (cons "src-phase" "12")
                              (cons "protected?" "#t")))))

  ;; lam->spec
  (let* ([bd (beg0 '())]
         [z  (lam 'name '() 3 '() #f '#() '() #f 1 bd)])
    (check-equal? (force-spec (lam->spec z))
                  (cons "lam"
                        (list (cons "name" "name")
                              (cons "flags" "[]")
                              (cons "num-params" "3")
                              (cons "param-types" "[]")
                              (cons "rest?" "#f")
                              (cons "closure-map" "[]")
                              (cons "closure-types" "[]")
                              (cons "toplevel-map" "#f")
                              (cons "max-let-depth" "1")
                              (cons "body" "<struct:beg0>")))))

  ;; closure->spec
  (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
         [z  (closure lm 'genid)])
    (check-equal? (force-spec (closure->spec z))
                  (cons "closure"
                        (list (cons "code" "<struct:lam>")
                              (cons "gen-id" "genid")))))

  ;; case-lam->spec
  (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
         [cl (closure lm 'id)]
         [cls (list lm cl lm)]
         [z   (case-lam 'name cls)])
    (check-equal? (force-spec (case-lam->spec z))
                  (cons "case-lam"
                        (list (cons "name" "name")
                              (cons "clauses" "[<struct:lam> <struct:closure> <struct:lam>]")))))

  ;; let-one->spec
  (let* ([rhs (beg0 '())]
         [bdy (beg0 '())]
         [z   (let-one rhs bdy #f #f)])
    (check-equal? (force-spec (let-one->spec z))
                  (cons "let-one"
                        (list (cons "rhs" "<struct:beg0>")
                              (cons "body" "<struct:beg0>")
                              (cons "type" "#f")
                              (cons "unused?" "#f")))))

  ;; let-void->spec
  (let* ([bdy (beg0 '())]
         [z   (let-void 1 #f bdy)])
    (check-equal? (force-spec (let-void->spec z))
                  (cons "let-void"
                        (list (cons "count" "1")
                              (cons "boxes" "#f")
                              (cons "body" "<struct:beg0>")))))

  ;; install-value->spec
  (let* ([rhs (branch #t #t #t)]
         [bdy (beg0 '())]
         [z   (install-value 2 3 #f rhs bdy)])
    (check-equal? (force-spec (install-value->spec z))
                  (cons "install-value"
                        (list (cons "count" "2")
                              (cons "pos" "3")
                              (cons "boxes?" "#f")
                              (cons "rhs" "<struct:branch>")
                              (cons "body" "<struct:beg0>")))))

  ;; let-rec->spec
  (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
         [pcs (list lm lm)]
         [bdy (beg0 '())]
         [z   (let-rec pcs bdy)])
    (check-equal? (force-spec (let-rec->spec z))
                  (cons "let-rec"
                        (list (cons "procs" "<struct:lam>[2]")
                              (cons "body" "<struct:beg0>")))))

  ;; boxenv->spec
  (let* ([bdy (beg0 '())]
         [z   (boxenv 2 bdy)])
    (check-equal? (force-spec (boxenv->spec z))
                  (cons "boxenv"
                        (list (cons "pos" "2")
                              (cons "body" "<struct:beg0>")))))

  ;; localref->spec
  (let ([z (localref #t 1 #t #t #f)])
    (check-equal? (force-spec (localref->spec z))
                  (cons "localref"
                        (list (cons "unbox?" "#t")
                              (cons "pos" "1")
                              (cons "clear?" "#t")
                              (cons "other-clears?" "#t")
                              (cons "type" "#f")))))

  ;; toplevel->spec
  (let ([z (toplevel 1 2 #f #f)])
    (check-equal? (force-spec (toplevel->spec z))
                  (cons "toplevel"
                        (list (cons "depth" "1")
                              (cons "pos" "2")
                              (cons "const?" "#f")
                              (cons "ready?" "#f")))))

  ;; topsyntax->spec
  (let ([z (topsyntax 1 2 3)])
    (check-equal? (force-spec (topsyntax->spec z))
                  (cons "topsyntax"
                        (list (cons "depth" "1")
                              (cons "pos" "2")
                              (cons "midpt" "3")))))

  ;; application->spec
  (let* ([e (beg0 '())]
         [s (seq '())]
         [z (application s (list e s s '() 'any 54 e))])
    (check-equal? (force-spec (application->spec z))
                  (cons "application"
                        (list (cons "rator" "<struct:seq>")
                              (cons "rands" "[<struct:beg0> <struct:seq> <struct:seq> () any 54 <struct:beg0>]")))))

  ;; branch->spec
  (let* ([z (branch #t (beg0 '()) #f)])
    (check-equal? (force-spec (branch->spec z))
                  (cons "branch"
                        (list (cons "test" "#t")
                              (cons "then" "<struct:beg0>")
                              (cons "else" "#f")))))

  ;; with-cont-mark->spec
  (let ([z (with-cont-mark (beg0 '()) (branch #t #t #t) (topsyntax 1 1 1))])
    (check-equal? (force-spec (with-cont-mark->spec z))
                  (cons "with-cont-mark"
                        (list (cons "key" "<struct:beg0>")
                              (cons "val" "<struct:branch>")
                              (cons "body" "<struct:topsyntax>")))))

  ;; beg0->spec
  (let ([z (beg0 (list (beg0 '()) 'asdf (beg0 (list (expr)))))])
    (check-equal? (force-spec (beg0->spec z))
                  (cons "beg0"
                        (list (cons "seq" "[<struct:beg0> asdf <struct:beg0>]")))))

  ;; varref->spec
  (let* ([tl (toplevel 1 1 #f #f)]
         [z  (varref tl #f)])
    (check-equal? (force-spec (varref->spec z))
                  (cons "varref"
                        (list (cons "toplevel" "<struct:toplevel>")
                              (cons "dummy" "#f")))))

  ;; assign->spec
  (let* ([id  (toplevel 1 1 #f #f)]
         [rhs (beg0 '())]
         [z   (assign id rhs #t)])
    (check-equal? (force-spec (assign->spec z))
                  (cons "assign"
                        (list (cons "id" "<struct:toplevel>")
                              (cons "rhs" "<struct:beg0>")
                              (cons "undef-ok?" "#t")))))

  ;; apply-values->spec
  (let ([z (apply-values (beg0 '()) (topsyntax 1 2 8))])
    (check-equal? (force-spec (apply-values->spec z))
                  (cons "apply-values"
                        (list (cons "proc" "<struct:beg0>")
                              (cons "args-expr" "<struct:topsyntax>")))))

  ;; primval->spec
  (let ([z (primval 420)])
    (check-equal? (force-spec (primval->spec z))
                  (cons "primval"
                        (list (cons "id" "420")))))

  ;; module-binding->spec
  (let ([z (module-binding 'any)])
    (check-equal? (force-spec (module-binding->spec z))
                  (cons "module-binding"
                        (list (cons "encoded" "any")))))

  ;; decoded-module-binding->spec
  (let* ([mpi (module-path-index-join #f #f)]
        [z (decoded-module-binding mpi 'foo 1 mpi 'ex 0 0 'hi)])
    (check-equal? (force-spec (decoded-module-binding->spec z))
                  (cons "decoded-module-binding"
                        (list (cons "path" "#<module-path-index:()>")
                              (cons "name" "foo")
                              (cons "phase" "1")
                              (cons "nominal-path" "#<module-path-index:()>")
                              (cons "nominal-export-name" "ex")
                              (cons "nominal-phase" "0")
                              (cons "import-phase" "0")
                              (cons "inspector-desc" "hi")))))

  ;; local-binding->spec
  (let ([z (local-binding 'hello)])
    (check-equal? (force-spec (local-binding->spec z))
                  (cons "local-binding"
                        (list (cons "name" "hello")))))

  ;; free-id=?-binding->spec
  (let* ([lb (local-binding 'hi)]
         [stx (stx-obj 0 (wrap '() '() '()) 'armed)]
         [z (free-id=?-binding lb stx 3)])
    (check-equal? (force-spec (free-id=?-binding->spec z))
                  (cons "free-id=?-binding"
                        (list (cons "base" "<struct:local-binding>")
                              (cons "id" "<struct:stx-obj>")
                              (cons "phase" "3")))))

  ;; --- helpers
  ;; any->string
  (check-equal? (any->string 'any) "any")
  (check-equal? (any->string "any") "any")
  (check-equal? (any->string #t) "#t")
  (check-equal? (any->string (vector 1 2 3)) "#(1 2 3)")

  ;; boolean->string
  (check-equal? (boolean->string #t) "#t")
  (check-equal? (boolean->string #f) "#f")

  ;; expr-seq-any->string
  (check-equal? (expr-seq-any->string (beg0 '())) "<struct:beg0>")
  (check-equal? (expr-seq-any->string (branch #t (expr) (expr))) "<struct:branch>")
  (check-equal? (expr-seq-any->string (seq '(blah))) "<struct:seq>")
  (check-equal? (expr-seq-any->string 420) "420")
  (check-equal? (expr-seq-any->string +) "#<procedure:+>")

  ;; form-or-any->string
  (check-equal? (form-or-any->string (def-values '() (expr))) "<struct:def-values>")
  (check-equal? (form-or-any->string (lam 'name '() 3 '() #f '#() '() #f 1 (expr))) "<struct:lam>")
  (check-equal? (form-or-any->string (zo)) "#s(zo)")
  (check-equal? (form-or-any->string "()") "()")
  (check-equal? (form-or-any->string #\H) "H")

  ;; format-list
  ; (this is just string-join)
  (check-equal? (format-list '()) "")
  (check-equal? (format-list (list "a" "bear" "man")) "a\nbear\nman")
  (check-equal? (format-list #:sep "---" (list "racket" "eering")) "racket---eering")

  ;; format-spec
  ; No fields
  (check-equal? (format-spec #f (cons "hello" '())) "<struct:hello>")
  (check-equal? (format-spec #t (cons "hello" '())) "<struct:hello>")
  ; String fields
  (check-equal? (format-spec #f (cons "str" (list (cons "hello" (lambda () "there"))))) "<struct:str>")
  (check-equal? (format-spec #t (cons "str" (list (cons "hello" (lambda () "there"))))) "<struct:str>\n  hello : there")
  ; Nested struct fields
  (check-equal? (format-spec #f (cons "pika" (list (cons "f1" (lambda () "val1"))
                                                   (cons "f2" (lambda () (list "nested" (cons "n1" (lambda () "wepa")))))))) "<struct:pika>")
  (check-equal? (format-spec #t (cons "pika" (list (cons "f1" (lambda () "val1"))
                                                   (cons "f2" (lambda () (list "nested" (cons "n1" (lambda () "wepa")))))))) "<struct:pika>\n  f1 : val1\n  f2 : <struct:nested>")
  ; Padding
  (check-equal? (format-spec #t (cons "pika" (list (cons "long-name" (lambda () "v1"))
                                                   (cons "name" (lambda () "v2"))))) "<struct:pika>\n  long-name : v1\n  name      : v2")

  ;; list->string
  (check-equal? (list->string (lambda (x) "blah") '()) "[]")
  (check-equal? (list->string number->string (list 1 2 3 4)) "[1 2 3 4]")
  (check-equal? (list->string (lambda (x) (format-spec #f (expr->spec x))) (list (branch #t #t #t))) "[<struct:branch>]")

  ;; listof-form-or-any->string
  (check-equal? (listof-form-or-any->string (list (seq '()) 'cat 53)) "[<struct:seq> cat 53]")

  ;; listof-zo->string
  (check-equal? (listof-zo->string toplevel->spec (list (toplevel 1 1 #f #f))) "<struct:toplevel>[1]")

  ;; module-path-index->string
  (check-equal? (module-path-index->string (module-path-index-join #f #f)) "#<module-path-index:()>")

  ;; module-path->spec
  (check-equal? (module-path->spec 'lalala) "lalala")

  ;; number-or-f->string
  (check-equal? (number-or-f->string #f) "#f")
  (check-equal? (number-or-f->string 0) "0")
  (check-equal? (number-or-f->string -1) "-1")
  (check-equal? (number-or-f->string 98) "98")

  ;; symbol-or-f->string
  (check-equal? (symbol-or-f->string #f) "#f")
  (check-equal? (symbol-or-f->string '#f) "#f")
  (check-equal? (symbol-or-f->string 'foobar) "foobar")
  (check-equal? (symbol-or-f->string 'wunderbar) "wunderbar")

  ;; toplevel-or-any->string
  (check-equal? (toplevel-or-any->string (toplevel 19 462 #t #t)) "<struct:toplevel>")
  (check-equal? (toplevel-or-any->string (toplevel 0 0 #f #f)) "<struct:toplevel>")
  ; Only toplevel zo structs get pretty-printed
  (check-equal? (toplevel-or-any->string (branch #t #t (beg0 '()))) "#s((branch expr 0 form 0 zo 0) #t #t #s((beg0 expr 0 form 0 zo 0) ()))")
  (check-equal? (toplevel-or-any->string "help") "help")

  ;; pad
  (check-equal? (pad "str" 3) "str")
  (check-equal? (pad "str" 4) "str ")
  (check-equal? (pad "str" 5 #:char #\X) "strXX")
)
