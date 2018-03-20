#lang racket/base

;; TODO unit tests for all functions (i.e. the new ones)

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
  zo->string
  ;; (->* (zo?) (#:deep? boolean?) string?)
  ;; Return a string representation of a zo struct

  zo->spec
  ;; (->i ([z zo?]) () [res (z) (and/c spec/c (specof z))])
  ;; Return a list-of-strings representation of a zo struct.
  ;; The structure of the list mirrors the structure of the original zo struct.

  specof spec/c
  ;; Contracts for conversion functions.
)

(require
  compiler/zo-structs
  ;zordoz/typed/zo-structs ;; For testing
  racket/contract
  racket/match
  (only-in racket/string string-join)
  (for-syntax racket/base racket/syntax)
  (only-in zordoz/private/dispatch-table make-table)
)

;; -----------------------------------------------------------------------------

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

;; =============================================================================

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
   linkl-directory
   linkl-bundle
   linkl
   form
   expr))

(define form->spec
  (make-table
   #:action ->spec
   def-values
   seq
   inline-variant
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
   application
   branch
   with-cont-mark
   seq
   beg0
   varref
   assign
   apply-values
   with-immed-mark
   primval))

(define (false? x)
  (eq? #f x))

(define (false->spec x)
  (boolean->string x))

(define constantness->spec
  (make-table
    #:action ->spec
    symbol
    false
    function-shape
    struct-shape
    struct-type-shape
    constructor-shape
    predicate-shape
    accessor-shape
    mutator-shape
    struct-type-property-shape
    property-predicate-shape
    property-accessor-shape
    struct-other-shape))

(define (constantness->string c)
  (define x (constantness->spec c))
  (if (string? x) x (format-spec #f x)))

;; --- private functions

(define
  (linkl-directory->spec ld)
  (list "linkl-directory"
        (lcons "table" (hash->spec any->string linkl-bundle->string (linkl-directory-table ld)))))

(define (linkl-bundle->string lb)
  (format-spec #f (linkl-bundle->spec lb)))

(define (linkl-or-any->string lx)
  (if (linkl? lx)
    (format-spec #f (linkl->spec lx))
    (any->string lx)))

(define
  (linkl-bundle->spec lb)
  (list "linkl-bundle"
        (lcons "table" (hash->spec any->string linkl-or-any->string (linkl-bundle-table lb)))))

(define
  (linkl->spec l)
  (list "linkl"
        (lcons "name" (symbol->spec (linkl-name l)))
        (lcons "importss" (any->string (linkl-importss l)))
        (lcons "import-shapess" (map import-shapes->spec (linkl-import-shapess l)))
        (lcons "exports" (any->string (linkl-exports l)))
        (lcons "internals" (any->string (linkl-internals l)))
        (lcons "lifts" (any->string (linkl-lifts l)))
        (lcons "source-names" (any->string (linkl-source-names l)))
        (lcons "body" (listof-form-or-any->string (linkl-body l)))
        (lcons "max-let-depth"  (number->string (linkl-max-let-depth l)))
        (lcons "need-instance-access?"  (boolean->string (linkl-need-instance-access? l)))))

(define (import-shapes->spec is)
  (list->string constantness->string is))

;; --- form

(define
  (def-values->spec z)
  (list "def-values"
        (lcons "ids" (list->string toplevel-or-symbol->string (def-values-ids z)))
        (lcons "rhs" (let ([rhs (def-values-rhs z)])
                       (cond [(inline-variant? rhs) (inline-variant->spec rhs)]
                             [else (expr-seq-any->string rhs)])))))

(define
  (seq->spec z)
  (list "seq"
        (lcons "forms" (listof-form-or-any->string (seq-forms z)))))

(define
  (inline-variant->spec z)
  (list "inline-variant"
        (lcons "direct" (expr->spec (inline-variant-direct z)))
        (lcons "inline" (expr->spec (inline-variant-inline z)))))

;; --- expr

;; Helper for `lam` and `case-lam`.
(define (lam-name->spec nm)
  (match nm
    [(? vector?)
     (any->string nm)]
    ['()
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
                            [#f "#f"]))
        (lcons "constant?" (boolean->string (varref-constant? z)))
        (lcons "from-unsafe?" (boolean->string (varref-from-unsafe? z)))))

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
  (with-immed-mark->spec z)
  (list "with-immed-mark"
        (lcons "key" (expr-seq-any->string (with-immed-mark-key z)))
        (lcons "def-val" (expr-seq-any->string (with-immed-mark-def-val z)))
        (lcons "body" (expr-seq-any->string (with-immed-mark-body z)))))

(define
  (primval->spec z)
  (list "primval"
        (lcons "id" (number->string (primval-id z)))))

;; --- Shapes

;; Shapes are not zo structs per se, but they are documented in the
;; decompile guide and do not seem to have a nice formatting method.

(define (symbol->spec s)
  (symbol->string s))

(define
  (function-shape->spec fs)
  (format-list #:sep " "
               (list "function-shape"
                     (format "arity : ~a"            (function-shape-arity fs))
                     (format "preserves-marks? : ~a" (function-shape-preserves-marks? fs)))))

(define struct-shape->spec
  (make-table
    #:action ->spec
    struct-type-shape
    constructor-shape
    predicate-shape
    accessor-shape
    mutator-shape
    struct-type-property-shape
    property-predicate-shape
    property-accessor-shape
    struct-other-shape))

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
  (struct-type-property-shape->spec stps)
  (format-list (list "struct-type-property-shape"
                     (format "has-guard? : ~a" (struct-type-property-shape-has-guard? stps)))))

(define
  (property-predicate-shape->spec stps)
  (format-list (list "property-predicate-shape")))

(define
  (property-accessor-shape->spec stps)
  (format-list (list "property-accessor-shape")))

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
  (define title (format "<zo:~a>" (car struct-spec)))
  (define field-name-lengths
    (for/list ([fd fields]) (string-length (car fd))))
  (define w ;; width of longest struct field name
    (if (null? fields) 0 (apply max field-name-lengths)))
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
  (cond [(null? zs) "[]"]
        [else        (format "~a[~a]" (format-spec #f (z->spec (car zs))) (length zs))]))

(define
  (hash->spec k->str v->str h)
  (format "{~a}"
    (string-join
      (for/list ([(k v) (in-hash h)])
        (list (k->str k) (v->str v)))
      "~n ")))

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

(define
  (hash->string h)
  (format "~a" h))

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
  (require rackunit compiler/zo-structs)

  ; Helper: force lazy tails so we can compare them.
  (define (force-spec sp)
    (cons (car sp) (for/list ([xy (cdr sp)]) (cons (car xy)
                                                   (let ([tl ((cdr xy))])
                                                     (if (string? tl)
                                                         tl
                                                         (format-spec #f tl)))))))

  ;; --- API functions
  (test-case "zo->spec"
    (check-exn exn:fail? (lambda () (zo->spec (zo))))
    (check-equal? (force-spec (zo->spec (branch #t #f #t)))
                  (list "branch"
                        (cons "test" "#t")
                        (cons "then" "#f")
                        (cons "else" "#t"))))

  (test-case "zo->string"
    (check-exn exn:fail? (lambda () (zo->string (zo))))
    (check-equal? (zo->string (toplevel 1 1 #t #t)) "<zo:toplevel>\n  depth  : 1\n  pos    : 1\n  const? : #t\n  ready? : #t")
    (check-equal? (zo->string #:deep? #t (toplevel 1 1 #t #t)) "<zo:toplevel>\n  depth  : 1\n  pos    : 1\n  const? : #t\n  ready? : #t")
    (check-equal? (zo->string #:deep? #f (toplevel 1 1 #t #t)) "<zo:toplevel>"))

  ;; --- private
  ;(test-case "linkl-directory->spec"
  ;  TODO)

  (test-case "form->spec"
    (let* ([z (form)])
      (check-equal? (form->spec z) #f)))

  (test-case "expr->spec"
    (let* ([z (expr)])
      (check-equal? (expr->spec z) #f)))

  (test-case "def-values->spec"
    (let* ([ids (list (toplevel 1 2 #t #f))]
           [rhs (beg0 '())]
           [z (def-values ids rhs)])
      (check-equal? (force-spec (def-values->spec z))
                    (cons "def-values"
                          (list (cons "ids" "[<zo:toplevel>]")
                                (cons "rhs" "<zo:beg0>"))))))

  (test-case "seq->spec"
    (let* ([fms (list (seq '()) (seq '()) (seq '()))]
           [z   (seq fms)])
      (check-equal? (force-spec (seq->spec z))
                    (cons "seq"
                          (list (cons "forms" "[<zo:seq> <zo:seq> <zo:seq>]"))))))


  (test-case "inline-variant->spec"
    (let* ([dr (beg0 '())]
           [il (beg0 '())]
           [z  (inline-variant dr il)])
      (check-equal? (force-spec (inline-variant->spec z))
                    (cons "inline-variant"
                          (list (cons "direct" "<zo:beg0>")
                                (cons "inline" "<zo:beg0>"))))))

  (test-case "lam->spec"
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
                                (cons "body" "<zo:beg0>"))))))

  (test-case "closure->spec"
    (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
           [z  (closure lm 'genid)])
      (check-equal? (force-spec (closure->spec z))
                    (cons "closure"
                          (list (cons "code" "<zo:lam>")
                                (cons "gen-id" "genid"))))))

  (test-case "case-lam->spec"
    (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
           [cl (closure lm 'id)]
           [cls (list lm cl lm)]
           [z   (case-lam 'name cls)])
      (check-equal? (force-spec (case-lam->spec z))
                    (cons "case-lam"
                          (list (cons "name" "name")
                                (cons "clauses" "[<zo:lam> <zo:closure> <zo:lam>]"))))))

  (test-case "let-one->spec"
    (let* ([rhs (beg0 '())]
           [bdy (beg0 '())]
           [z   (let-one rhs bdy #f #f)])
      (check-equal? (force-spec (let-one->spec z))
                    (cons "let-one"
                          (list (cons "rhs" "<zo:beg0>")
                                (cons "body" "<zo:beg0>")
                                (cons "type" "#f")
                                (cons "unused?" "#f"))))))

  (test-case "let-void->spec"
    (let* ([bdy (beg0 '())]
           [z   (let-void 1 #f bdy)])
      (check-equal? (force-spec (let-void->spec z))
                    (cons "let-void"
                          (list (cons "count" "1")
                                (cons "boxes" "#f")
                                (cons "body" "<zo:beg0>"))))))

  (test-case "install-value->spec"
    (let* ([rhs (branch #t #t #t)]
           [bdy (beg0 '())]
           [z   (install-value 2 3 #f rhs bdy)])
      (check-equal? (force-spec (install-value->spec z))
                    (cons "install-value"
                          (list (cons "count" "2")
                                (cons "pos" "3")
                                (cons "boxes?" "#f")
                                (cons "rhs" "<zo:branch>")
                                (cons "body" "<zo:beg0>"))))))

  (test-case "let-rec->spec"
    (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
           [pcs (list lm lm)]
           [bdy (beg0 '())]
           [z   (let-rec pcs bdy)])
      (check-equal? (force-spec (let-rec->spec z))
                    (cons "let-rec"
                          (list (cons "procs" "<zo:lam>[2]")
                                (cons "body" "<zo:beg0>"))))))

  (test-case "boxenv->spec"
    (let* ([bdy (beg0 '())]
           [z   (boxenv 2 bdy)])
      (check-equal? (force-spec (boxenv->spec z))
                    (cons "boxenv"
                          (list (cons "pos" "2")
                                (cons "body" "<zo:beg0>"))))))

  (test-case "localref->spec"
    (let ([z (localref #t 1 #t #t #f)])
      (check-equal? (force-spec (localref->spec z))
                    (cons "localref"
                          (list (cons "unbox?" "#t")
                                (cons "pos" "1")
                                (cons "clear?" "#t")
                                (cons "other-clears?" "#t")
                                (cons "type" "#f"))))))

  (test-case "toplevel->spec"
    (let ([z (toplevel 1 2 #f #f)])
      (check-equal? (force-spec (toplevel->spec z))
                    (cons "toplevel"
                          (list (cons "depth" "1")
                                (cons "pos" "2")
                                (cons "const?" "#f")
                                (cons "ready?" "#f"))))))

  (test-case "application->spec"
    (let* ([e (beg0 '())]
           [s (seq '())]
           [z (application s (list e s s '() 'any 54 e))])
      (check-equal? (force-spec (application->spec z))
                    (cons "application"
                          (list (cons "rator" "<zo:seq>")
                                (cons "rands" "[<zo:beg0> <zo:seq> <zo:seq> () any 54 <zo:beg0>]"))))))

  (test-case "branch->spec"
    (let* ([z (branch #t (beg0 '()) #f)])
      (check-equal? (force-spec (branch->spec z))
                    (cons "branch"
                          (list (cons "test" "#t")
                                (cons "then" "<zo:beg0>")
                                (cons "else" "#f"))))))

  (test-case "beg0->spec"
    (let ([z (beg0 (list (beg0 '()) 'asdf (beg0 (list (expr)))))])
      (check-equal? (force-spec (beg0->spec z))
                    (cons "beg0"
                          (list (cons "seq" "[<zo:beg0> asdf <zo:beg0>]"))))))

  (test-case "varref->spec"
    (let* ([tl (toplevel 1 1 #f #f)]
           [z  (varref tl #f #f #f)])
      (check-equal? (force-spec (varref->spec z))
                    (cons "varref"
                          (list (cons "toplevel" "<zo:toplevel>")
                                (cons "dummy" "#f")
                                (cons "constant?" "#f")
                                (cons "from-unsafe?" "#f"))))))

  (test-case "assign->spec"
    (let* ([id  (toplevel 1 1 #f #f)]
           [rhs (beg0 '())]
           [z   (assign id rhs #t)])
      (check-equal? (force-spec (assign->spec z))
                    (cons "assign"
                          (list (cons "id" "<zo:toplevel>")
                                (cons "rhs" "<zo:beg0>")
                                (cons "undef-ok?" "#t"))))))

  (test-case "apply-values->spec"
    (let ([z (apply-values (beg0 '()) (beg0 '(1 2 8)))])
      (check-equal? (force-spec (apply-values->spec z))
                    (cons "apply-values"
                          (list (cons "proc" "<zo:beg0>")
                                (cons "args-expr" "<zo:beg0>"))))))

  (test-case "with-immed-mark->spec"
    (let ([z (with-immed-mark (beg0 '()) (beg0 '()) (beg0 '(1 2 8)))])
      (check-equal? (force-spec (with-immed-mark->spec z))
                    (cons "with-immed-mark"
                          (list (cons "key" "<zo:beg0>")
                                (cons "def-val" "<zo:beg0>")
                                (cons "body" "<zo:beg0>"))))))

  (test-case "primval->spec"
    (let ([z (primval 420)])
      (check-equal? (force-spec (primval->spec z))
                    (cons "primval"
                          (list (cons "id" "420"))))))

  ;; --- helpers
  (test-case "any->string"
    (check-equal? (any->string 'any) "any")
    (check-equal? (any->string "any") "any")
    (check-equal? (any->string #t) "#t")
    (check-equal? (any->string (vector 1 2 3)) "#(1 2 3)"))

  (test-case "boolean->string"
    (check-equal? (boolean->string #t) "#t")
    (check-equal? (boolean->string #f) "#f"))

  (test-case "expr-seq-any->string"
    (check-equal? (expr-seq-any->string (beg0 '())) "<zo:beg0>")
    (check-equal? (expr-seq-any->string (branch #t (expr) (expr))) "<zo:branch>")
    (check-equal? (expr-seq-any->string (seq '(blah))) "<zo:seq>")
    (check-equal? (expr-seq-any->string 420) "420")
    (check-equal? (expr-seq-any->string +) "#<procedure:+>"))

  (test-case "form-or-any->string"
    (check-equal? (form-or-any->string (def-values '() (expr))) "<zo:def-values>")
    (check-equal? (form-or-any->string (lam 'name '() 3 '() #f '#() '() #f 1 (expr))) "<zo:lam>")
    (check-equal? (form-or-any->string (zo)) "#s(zo)")
    (check-equal? (form-or-any->string "()") "()")
    (check-equal? (form-or-any->string #\H) "H"))

  (test-case "format-list"
    ; (this is just string-join)
    (check-equal? (format-list '()) "")
    (check-equal? (format-list (list "a" "bear" "man")) "a\nbear\nman")
    (check-equal? (format-list #:sep "---" (list "racket" "eering")) "racket---eering"))

  (test-case "format-spec"
    ; No fields
    (check-equal? (format-spec #f (cons "hello" '())) "<zo:hello>")
    (check-equal? (format-spec #t (cons "hello" '())) "<zo:hello>")
    ; String fields
    (check-equal? (format-spec #f (cons "str" (list (cons "hello" (lambda () "there"))))) "<zo:str>")
    (check-equal? (format-spec #t (cons "str" (list (cons "hello" (lambda () "there"))))) "<zo:str>\n  hello : there")
    ; Nested struct fields
    (check-equal? (format-spec #f (cons "pika" (list (cons "f1" (lambda () "val1"))
                                                     (cons "f2" (lambda () (list "nested" (cons "n1" (lambda () "wepa")))))))) "<zo:pika>")
    (check-equal? (format-spec #t (cons "pika" (list (cons "f1" (lambda () "val1"))
                                                     (cons "f2" (lambda () (list "nested" (cons "n1" (lambda () "wepa")))))))) "<zo:pika>\n  f1 : val1\n  f2 : <zo:nested>")
    ; Padding
    (check-equal? (format-spec #t (cons "pika" (list (cons "long-name" (lambda () "v1"))
                                                     (cons "name" (lambda () "v2"))))) "<zo:pika>\n  long-name : v1\n  name      : v2"))

  (test-case "list->string"
    (check-equal? (list->string (lambda (x) "blah") '()) "[]")
    (check-equal? (list->string number->string (list 1 2 3 4)) "[1 2 3 4]")
    (check-equal? (list->string (lambda (x) (format-spec #f (expr->spec x))) (list (branch #t #t #t))) "[<zo:branch>]"))

  (test-case "listof-form-or-any->string"
    (check-equal? (listof-form-or-any->string (list (seq '()) 'cat 53)) "[<zo:seq> cat 53]"))

  (test-case "listof-zo->string"
    (check-equal? (listof-zo->string toplevel->spec (list (toplevel 1 1 #f #f))) "<zo:toplevel>[1]"))

  (test-case "module-path-index->string"
    (check-equal? (module-path-index->string (module-path-index-join #f #f)) "#<module-path-index>"))

  (test-case "module-path->spec"
    (check-equal? (module-path->spec 'lalala) "lalala"))

  (test-case "number-or-f->string"
    (check-equal? (number-or-f->string #f) "#f")
    (check-equal? (number-or-f->string 0) "0")
    (check-equal? (number-or-f->string -1) "-1")
    (check-equal? (number-or-f->string 98) "98"))

  (test-case "symbol-or-f->string"
    (check-equal? (symbol-or-f->string #f) "#f")
    (check-equal? (symbol-or-f->string '#f) "#f")
    (check-equal? (symbol-or-f->string 'foobar) "foobar")
    (check-equal? (symbol-or-f->string 'wunderbar) "wunderbar"))

  (test-case "toplevel-or-any->string"
    (check-equal? (toplevel-or-any->string (toplevel 19 462 #t #t)) "<zo:toplevel>")
    (check-equal? (toplevel-or-any->string (toplevel 0 0 #f #f)) "<zo:toplevel>")
    ; Only toplevel zo structs get pretty-printed
    (check-equal? (toplevel-or-any->string (branch #t #t (beg0 '()))) "#s((branch expr 0 form 0 zo 0) #t #t #s((beg0 expr 0 form 0 zo 0) ()))")
    (check-equal? (toplevel-or-any->string "help") "help"))

  (test-case "pad"
    (check-equal? (pad "str" 3) "str")
    (check-equal? (pad "str" 4) "str ")
    (check-equal? (pad "str" 5 #:char #\X) "strXX"))
)
