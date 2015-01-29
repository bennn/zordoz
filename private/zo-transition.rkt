#lang racket/base

;; Access the fields of a struct by name at runtime.

;; Uses predicates to guess what struct its argument is,
;; then compares strings with statically-known field names.
;; Functions that end with '->' are the specific transition function
;; for a type of zo struct.

(provide
 ;; (-> zo? string? (values (or/c zo? (listof zo?)) boolean?))
 ;; Access "structName-fieldName myStruct" at runtime.
 zo-transition)

(require compiler/zo-structs
         racket/match
         (only-in racket/list empty? empty)
         (only-in "dispatch-table.rkt" make-table))

;; -----------------------------------------------------------------------------

;; --- API functions

;; Look up the field name `field-name` in the struct `z`.
;; First use predicates to decide what type of struct `z` is,
;; then use string equality to check if `field-name` matches any
;; statically-known name.
;; Return two values.
;; - First is a zo struct or list of zo structs, depending on the
;;   value stored in the field denoted by `field-name`
;; - Second is a boolean indicating success or failure.
;;   On failure, the returned zo struct is `z`.
(define (zo-transition z field-name)
  ;; (-> zo? string? (values (or/c zo? (listof zo?)) boolean?))
  ;; Check if transition failed or returned a list without any zo, pack result values.
  (match (try-transition z field-name)
    [(? zo? nxt)
     (values nxt #t)]
    [(? list? nxt)
     (match (filter zo? nxt)
       ['() (values z #f)]
       [zs  (values zs #t)])]
    [_
     (values z #f)]))

;; --- dispatch

(define try-transition
  (make-table
   #:action ->
   compilation-top
   prefix
   global-bucket
   module-variable
   stx
   form
   wrapped
   wrap
   free-id-info
   all-from-module
   module-binding
   nominal-path
   provided))

(define form->
  (make-table
   #:action ->
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

(define expr->
  (make-table
   #:action ->
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

(define wrap->
  (make-table
   #:action ->
   top-level-rename
   mark-barrier
   lexical-rename
   phase-shift
   module-rename
   wrap-mark
   prune))

(define module-binding->
  (make-table
   #:action ->
   simple-module-binding
   phased-module-binding
   exported-nominal-module-binding
   nominal-module-binding
   exported-module-binding))

(define nominal-path->
  (make-table
   #:action ->
   simple-nominal-path
   imported-nominal-path
   phased-nominal-path))

;; --- getters

(define (compilation-top-> z field-name)
  ;; (-> compilation-top? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["prefix"
     (compilation-top-prefix z)]
    ["code"
     (compilation-top-code   z)]
    [_ #f]))

(define (prefix-> z field-name)
  ;; (-> prefix? string? (or/c (listof zo?) zo? #f))
  (define (gb-or-mv? tl)
    (or (global-bucket? tl) (module-variable? tl)))
  (match field-name
    ["toplevels"
     (filter gb-or-mv? (prefix-toplevels z))]
    ["stxs"
     (prefix-stxs z)]
    [_ #f]))

(define (global-bucket-> z field-name)
  ;; (-> global-bucket? string? (or/c (listof zo?) zo? #f))
  #f)

(define (module-variable-> z field-name)
  ;; (-> module-variable? string? (or/c (listof zo?) zo? #f))
  #f)

(define (stx-> z field-name)
  ;; (-> stx? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["encoded"
     (stx-encoded z)]
    [_  #f]))

(define (wrapped-> z field-name)
  ;; (-> wrapped? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["wraps"
     (wrapped-wraps z)]
    [_ #f]))

(define (free-id-info-> z field-name)
  ;; (-> free-id-info? string? (or/c (listof zo?) zo? #f))
  #f)

(define (all-from-module-> z field-name)
 ;; (-> all-from-module? string? (or/c (listof zo?) zo? #f))
  #f)

;; --- form

(define (def-values-> z field-name)
  ;; (-> def-values? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["ids"
     (def-values-ids z)]
    ["rhs"
     (match (def-values-rhs z)
       [(or (? expr? rhs) (? seq? rhs) (? inline-variant? rhs))
        rhs]
       [_ #f])]
  [_ #f]))

(define (def-syntaxes-> z field-name)
  ;; (-> def-syntaxes? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["ids"
     (filter toplevel? (def-syntaxes-ids z))]
    ["rhs"
     (match (def-syntaxes-rhs z)
       [(or (? expr? rhs) (? seq? rhs)) rhs]
       [_ #f])]
    ["prefix"
     (def-syntaxes-prefix z)]
    ["dummy"
     (match (def-syntaxes-dummy z)
       [(? toplevel? dm) dm]
       [_ #f])]
    [_ #f]))

(define (seq-for-syntax-> z field-name)
  ;; (-> seq-for-syntax? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["forms"
     (filter form? (seq-for-syntax-forms z))]
    ["prefix"
     (seq-for-syntax-prefix z)]
    ["dummy"
     (match (seq-for-syntax-dummy z)
       [(? toplevel? dm) dm]
       [_ #f])]
    [_ #f]))

(define (req-> z field-name)
  ;; (-> req? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["reqs"
     (req-reqs z)]
    ["dummy"
     (req-dummy z)]
    [_ #f]))

(define (seq-> z field-name)
  ;; (-> seq? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["forms"
     (filter form? (seq-forms z))]
    [_ #f]))

(define (splice-> z field-name)
  ;; (-> splice? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["forms"
     (filter form? (splice-forms z))]
    [_ #f]))

(define (inline-variant-> z field-name)
  ;; (-> inline-variant? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["direct"
     (inline-variant-direct z)]
    ["inline"
     (inline-variant-inline z)]
    [_ #f]))

(define (mod-> z field-name)
  ;; (-> mod? string? (or/c (listof zo?) zo? #f))
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
  (match field-name
    ["prefix"
     (mod-prefix z)]
    ["provides"
     (get-provided (mod-provides z))]
    ["body"
     (filter form? (mod-body z))]
    ["syntax-bodies"
     (get-syntaxes (mod-syntax-bodies z))]
    ["dummy"
     (mod-dummy z)]
    ["internal-context"
     (match (mod-internal-context z)
       [(? stx? ic) ic]
       [(? vector? ic) (vector->list ic)]
       [_ #f])]
    ["pre-submodules"
     (mod-pre-submodules z)]
    ["post-submodules"
     (mod-post-submodules z)]
    [_ #f]))

(define (provided-> z field-name)
  ;; (-> provided? string? (or/c (listof zo?) zo? #f))
  #f)

;; --- expr

(define (lam-> z field-name)
  ;; (-> lam? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["body"
     (match (lam-body z)
       [(? expr-or-seq? bd) bd]
       [_x #f])]
    [_ #f]))

(define (closure-> z field-name)
  ;; (-> closure? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["code"
     (closure-code z)]
    [_ #f]))

(define (case-lam-> z field-name)
  ;; (-> case-lam? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["clauses"
     (case-lam-clauses z)]
    [_ #f]))

(define (let-one-> z field-name)
  ;; (-> let-one? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["rhs"
     (match (let-one-rhs z)
       [(? expr-or-seq? rhs) rhs]
       [_ #f])]
    ["body"
     (match (let-one-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(define (let-void-> z field-name)
  ;; (-> let-void? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["body"
     (match (let-void-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(define (install-value-> z field-name)
  ;; (-> install-value? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["rhs"
     (match (install-value-rhs z)
       [(? expr-or-seq? rhs) rhs]
       [_ #f])]
    ["body"
     (match (install-value-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(define (let-rec-> z field-name)
  ;; (-> let-rec? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["procs"
     (let-rec-procs z)]
    ["body"
     (match (let-rec-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(define (boxenv-> z field-name)
  ;; (-> boxenv? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["body"
     (match (boxenv-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

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
  (match field-name
    ["rator"
     (match (application-rator z)
       [(? expr-or-seq? rator) rator]
       [_ #f])]
    ["rands"
     (filter expr-or-seq? (application-rands z))]
    [_ #f]))

(define (branch-> z field-name)
  ;; (-> branch? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["test"
     (match (branch-test z)
       [(? expr-or-seq? test) test]
       [_ #f])]
    ["then"
     (match (branch-then z)
       [(? expr-or-seq? then) then]
       [_ #f])]
    ["else"
     (match (branch-else z)
       [(? expr-or-seq? el) el]
       [_ #f])]
    [_ #f]))

(define (with-cont-mark-> z field-name)
  ;; (-> with-cont-mark? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["key"
     (match (with-cont-mark-key z)
       [(? expr-or-seq? key)  key]
       [_ #f])]
    ["val"
     (match (with-cont-mark-val z)
       [(? expr-or-seq? val) val]
       [_ #f])]
    ["body"
     (match (with-cont-mark-body z)
       [(? expr-or-seq? body) body]
       [_ #f])]
    [_ #f]))

(define (beg0-> z field-name)
  ;; (-> beg0? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["seq" (filter expr-or-seq? (beg0-seq z))]
    [_ #f]))

(define (varref-> z field-name)
  ;; (-> varref? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["toplevel"
     (match (varref-toplevel z)
       [(? toplevel? tl) tl]
       [_ #f])]
    ["dummy"
     (match (varref-dummy z)
       [(? toplevel? dm) dm]
       [_ #f])]
    [_ #f]))

(define (assign-> z field-name)
  ;; (-> assign? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["id" (assign-id z)]
    ["rhs" (match (assign-rhs z)
             [(? expr-or-seq? rhs) rhs]
             [_ #f])]
    [_ #f]))

(define (apply-values-> z field-name)
  ;; (-> apply-values? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["proc"
     (match (apply-values-proc z)
       [(? expr-or-seq? proc) proc]
       [_ #f])]
    ["args-expr"
     (match (apply-values-args-expr z)
       [(? expr-or-seq? args-expr) args-expr]
       [_ #f])]
    [_ #f]))

(define (primval-> z field-name)
  ;; (-> primval? string? (or/c (listof zo?) zo? #f))
  #f)

;; --- wrap

(define (top-level-rename-> z field-name)
  ;; (-> top-level-rename? string? (or/c (listof zo?) zo? #f))
  #f)

(define (mark-barrier-> z field-name)
  ;; (-> mark-barrier? string? (or/c (listof zo?) zo? #f))
  #f)

(define (lexical-rename-> z field-name)
  ;; (-> lexical-rename? string? (or/c (listof zo?) zo? #f))
  (define (get-free-id-info als)
    ;; (-> (listof (cons/c symbol? (or/c symbol? (cons/c symbol? (or/c (cons/c symbol? (or/c symbol? #f)) free-id-info?))))) (listof free-id-info?))
    (for/list ([blah als]
               #:when (and (pair? (cdr blah))
                           (free-id-info? (cddr blah))))
      (cddr blah)))
  (match field-name
    ["alist"
     (get-free-id-info (lexical-rename-alist z))]
    [_ #f]))
  
(define (phase-shift-> z field-name)
  ;; (-> phase-shift? string? (or/c (listof zo?) zo? #f))
  #f)

(define (module-rename-> z field-name)
  ;; (-> module-rename? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["unmarshals" (module-rename-unmarshals z)]
    ["renames"    (for/list ([mbpair (module-rename-renames z)]) (cdr mbpair))]
    [_ #f]))

(define (wrap-mark-> z field-name)
  ;; (-> wrap-mark? string? (or/c (listof zo?) zo? #f))
  #f)

(define (prune-> z field-name)
  ;; (-> prune? string? (or/c (listof zo?) zo? #f))
  #f)

;; --- module-binding

(define (simple-module-binding-> z field-name)
  ;; (-> simple-module-binding? string? (or/c (listof zo?) zo? #f))
  #f)

(define (phased-module-binding-> z field-name)
  ;; (-> phased-module-binding? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["nominal-path" (phased-module-binding-nominal-path z)]
    [_ #f]))

(define (exported-nominal-module-binding-> z field-name)
  ;; (-> exported-nominal-module-binding? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["nominal-path" (exported-nominal-module-binding-nominal-path z)]
    [_ #f]))

(define (nominal-module-binding-> z field-name)
  ;; (-> nominal-module-binding? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["nominal-path" (nominal-module-binding-nominal-path z)]
    [_ #f]))

(define (exported-module-binding-> z field-name)
  ;; (-> exported-module-binding? string? (or/c (listof zo?) zo? #f))
  #f)

;; --- nominal-path

(define (simple-nominal-path-> z field-name)
  ;; (-> simple-nominal-path? string? (or/c (listof zo?) zo? #f))
  #f)

(define (imported-nominal-path-> z field-name)
  ;; (-> imported-nominal-path? string? (or/c (listof zo?) zo? #f))
  #f)

(define (phased-nominal-path-> z field-name)
  ;; (-> phased-nominal-path? string? (or/c (listof zo?) zo? #f))
  #f)

;; --- helpers

;; True if the argument is an 'expr' or a 'seq' zo struct.
(define (expr-or-seq? x)
  ;; (-> any/c boolean?)
  (or (expr? x) (seq? x)))

;; -----------------------------------------------------------------------------
;; --- testing

(module+ test
  (require rackunit)
           ;(only-in syntax/modresolve module-path-index-join))

  ;; compilation-top->
  (let* ([px (prefix 0 '() '())]
         [cd (form)]
         [z  (compilation-top 0 px cd)])
    (begin (check-equal? (compilation-top-> z "prefix") px)
           (check-equal? (compilation-top-> z "code")   cd)
           (check-equal? (compilation-top-> z "")       #f)))

  ;; prefix->
  (let* ([mpi (module-path-index-join #f #f)]
         [gb (global-bucket 'NAME)]
         [mv (module-variable mpi 'SYM 0 0 #f)]
         [sx (stx (wrapped (void) '() 'tainted))]
         [z  (prefix 0
                    (list gb mv)
                    (list sx))])
    (begin (check-equal? (prefix-> z "toplevels") (list gb mv))
           (check-equal? (prefix-> z "stxs")      (list sx))
           (check-equal? (prefix-> z "num-lifts") #f)
           (check-equal? (prefix-> z "")          #f)))

  ;; global-bucket->
  (let* ([z (global-bucket 'arbitrary-symbol)])
    (check-equal? (global-bucket-> z "name") #f))

  ;; module-variable->
  (let* ([mpi (module-path-index-join #f #f)]
         [fs  (function-shape 1 #f)]
         [ss  (struct-shape)] 
         [z   (module-variable mpi 'arbitrary 999 9001 fs)]
         ;; Testing when 'constantness' is a struct shape (still #f, it's not a zo)
         [z*  (module-variable mpi 'arbitrary 999 9001 ss)])
    (begin (check-equal? (module-variable-> z "modidx") #f)
           (check-equal? (module-variable-> z "sym") #f)
           (check-equal? (module-variable-> z "pos") #f)
           (check-equal? (module-variable-> z "phase") #f)
           (check-equal? (module-variable-> z "constantness") #f)
           (check-equal? (module-variable-> z* "constantness") #f)))

  ;; stx->
  (let* ([wp (wrapped (void) '() 'tainted)]
         [z (stx wp)])
    (begin (check-equal? (stx-> z "encoded") wp)
           (check-equal? (stx-> z "")        #f)))

  ;; form-> (this is better tested by the specific tests for 'def-values->', 'req->', ...)
  (let* ([z (form)])
    (check-equal? (form-> z "") #f))

  ;; expr-> (see tests for specific expressions below
  (let* ([z (expr)])
    (check-equal? (expr-> z "") #f))

  ;; wrapped->
  (let* ([wps (list (wrap) (wrap) (wrap))]
         [z   (wrapped (void) wps 'tainted)])
    (begin (check-equal? (wrapped-> z "wraps")        wps)
           (check-equal? (wrapped-> z "datum")         #f)
           (check-equal? (wrapped-> z "tamper-status") #f)
           (check-equal? (wrapped-> z "")              #f)))

  ;; wrap-> (see below)
  (let* ([z (wrap)])
    (check-equal? (wrap-> z "") #f))
  
  ;; free-id-info->
  (let* ([mpi (module-path-index-join #f #f)]
         [z   (free-id-info mpi 'A mpi 'B #f 101 #f #f)])
    (begin (check-equal? (free-id-info-> z "path0") #f)
           (check-equal? (free-id-info-> z "symbol0") #f)
           (check-equal? (free-id-info-> z "path1") #f)
           (check-equal? (free-id-info-> z "symbol1") #f)
           (check-equal? (free-id-info-> z "phase0") #f)
           (check-equal? (free-id-info-> z "phase1") #f)
           (check-equal? (free-id-info-> z "phase2") #f)
           (check-equal? (free-id-info-> z "use-current-inspector?") #f)
           (check-equal? (free-id-info-> z "") #f)))

  ;; all-from-module->
  (let* ([mpi (module-path-index-join #f #f)]
         [z (all-from-module mpi #f #f '() #f '())])
    (begin (check-equal? (all-from-module-> z "path") #f)
           (check-equal? (all-from-module-> z "phase") #f)
           (check-equal? (all-from-module-> z "src-phase") #f)
           (check-equal? (all-from-module-> z "exceptions") #f)
           (check-equal? (all-from-module-> z "prefix") #f)
           (check-equal? (all-from-module-> z "context") #f)
           (check-equal? (all-from-module-> z "") #f)))
  
  ;; module-binding-> (see below)
  (let* ([z (module-binding)])
    (check-equal? (module-binding-> z "") #f))
  
  ;; nominal-path-> (see below)
  (let* ([z (nominal-path)])
    (check-equal? (nominal-path-> z "") #f))

  ;; def-values->
  (let* ([ids (list (toplevel 1 2 #t #f))]
         [rhs (expr)]
         [z (def-values ids rhs)])
    (begin (check-equal? (def-values-> z "ids") ids)
           (check-equal? (def-values-> z "rhs") rhs)
           (check-equal? (def-values-> z "") #f)))

  ;; def-syntaxes->
  (let* ([ids (list (toplevel 1 2 #t #f))]
         [rhs (expr)]
         [px  (prefix 0 '() '())]
         [dm  (toplevel 1 1 #t #t)]
         [z   (def-syntaxes ids rhs px 42 dm)]
         ;; If dummy is false, transition fails
         [z*  (def-syntaxes ids rhs px 42 #f)])
    (begin (check-equal? (def-syntaxes-> z "ids") ids)
           (check-equal? (def-syntaxes-> z "rhs") rhs)
           (check-equal? (def-syntaxes-> z "prefix") px)
           (check-equal? (def-syntaxes-> z "dummy") dm)
           (check-equal? (def-syntaxes-> z "max-let-depth") #f)
           (check-equal? (def-syntaxes-> z "") #f)
           (check-equal? (def-syntaxes-> z* "dummy") #f)))

  ;; seq-for-syntax->
  (let* ([fms (list (form))]
         [px  (prefix 0 '() '())]
         [dm  (toplevel 9 9 #t #t)]
         [z   (seq-for-syntax fms px 8 dm)]
         ;; should filter non-zo from the forms list
         [z*  (seq-for-syntax '(A B C) px 9 dm)])
    (begin (check-equal? (seq-for-syntax-> z "forms") fms)
           (check-equal? (seq-for-syntax-> z "prefix") px)
           (check-equal? (seq-for-syntax-> z "max-let-depth") #f)
           (check-equal? (seq-for-syntax-> z "dummy") dm)
           (check-equal? (seq-for-syntax-> z "") #f)
           (check-equal? (seq-for-syntax-> z* "forms") '())
           ;; empty list filtered at toplevel
           (let-values ([(ctx* pass?) (zo-transition z* "forms")])
             (begin (check-equal? ctx* z*)
                    (check-false pass?)))))

  ;; req->
  (let* ([sx (stx (wrapped 'XXX '() 'clean))]
         [dm (toplevel 1 1 #t #t)]
         [z  (req sx dm)])
    (begin (check-equal? (req-> z "reqs") sx)
           (check-equal? (req-> z "dummy") dm)
           (check-equal? (req-> z "") #f)))

  ;; seq->
  (let* ([fms (list (form) (form) (form))]
         [z   (seq fms)]
         [z*  (seq '(N O T F O R M S))])
    (begin (check-equal? (seq-> z "forms") fms)
           (check-equal? (seq-> z "") #f)
           (check-equal? (seq-> z* "forms") '())
           (let-values ([(ctx* pass?) (zo-transition z* "forms")])
             (begin (check-equal? ctx* z*)
                    (check-false pass?)))))

  
  ;; splice->
  (let* ([fms (list (form) (form))]
         [z   (splice fms)]
         [z*  (splice '(X X X))])
    (begin (check-equal? (splice-> z "forms") fms)
           (check-equal? (splice-> z "") #f)
           (check-equal? (splice-> z* "forms") '())
           (let-values ([(ctx* pass?) (zo-transition z* "forms")])
             (begin (check-equal? ctx* z*)
                    (check-false pass?)))))
    
  ;; inline-variant->
  (let* ([dr (expr)]
         [il (expr)]
         [z  (inline-variant dr il)])
    (begin (check-equal? (inline-variant-> z "direct") dr)
           (check-equal? (inline-variant-> z "inline") il)
           (check-equal? (inline-variant-> z "") #f)))
         
  ;; mod->
  (let* ([mpi (module-path-index-join #f #f)]
         [px  (prefix 0 '() '())]
         [pd1 (provided 'p1 #f 'B 'C 13 #t)]
         [pd2 (provided 'p2 #f 'asdf 'C 6 #f)]
         [pd3 (provided 'p3 #f 'B 'lol 832 #t)]
         [pd4 (provided 'p4 #f 'R 'xx 1 #t)]
         [pvs (list (list #f (list pd1 pd2) (list pd3))
                    (list #f (list pd4) '()))]
         [bd  (list (form) 'any)]
         [ds  (def-syntaxes '() (expr) (prefix 0 '() '()) 1 #f)]
         [sfs (seq-for-syntax '() (prefix 0 '() '()) 999 (toplevel 9 9 #t #t))]
         [sb  (list (cons 7 (list ds))
                    (cons 8 (list sfs)))]
         [dm  (toplevel 1 1 #f #f)]
         [ic  (stx (wrapped 'dirty '() 'clean))]
         [m1  (mod 'm1 'm1src mpi px pvs '() bd sb '() 0 dm #f ic '() '() '())]
         [m2  (mod 'm2 'm2src mpi px pvs '() bd sb '() 0 dm #f ic '() '() '())]
         [m3  (mod 'm3 'm3src mpi px pvs '() bd sb '() 0 dm #f ic '() '() '())]
         [prs (list m1 m2)]
         [pts (list m3)]
         [z   (mod 'name 'srcname mpi px pvs '() bd sb '() 0 dm #f ic '() prs pts)])
    (begin (check-equal? (mod-> z "prefix") px)
           (check-equal? (mod-> z "provides") (list pd1 pd2 pd3 pd4))
           (check-equal? (mod-> z "body") (list (form)))
           (check-equal? (mod-> z "syntax-bodies") (list ds sfs))
           (check-equal? (mod-> z "dummy") dm)
           (check-equal? (mod-> z "internal-context") ic)
           (check-equal? (mod-> z "pre-submodules") prs)
           (check-equal? (mod-> z "post-submodules") pts)
           (check-equal? (mod-> z "name") #f)
           (check-equal? (mod-> z "srcname") #f)
           (check-equal? (mod-> z "self-modidx") #f)
           (check-equal? (mod-> z "requires") #f)
           (check-equal? (mod-> z "unexported") #f)
           (check-equal? (mod-> z "max-let-depth") #f)
           (check-equal? (mod-> z "lang-info") #f)
           (check-equal? (mod-> z "flags") #f)
           (check-equal? (mod-> z "") #f)))

  ;; provided->
  (let* ([z (provided 'name #f 'srcname 'nomnom 12 #t)])
    (begin (check-equal? (provided-> z "name") #f)
           (check-equal? (provided-> z "src") #f)
           (check-equal? (provided-> z "src-name") #f)
           (check-equal? (provided-> z "nom-src") #f)
           (check-equal? (provided-> z "src-phase") #f)
           (check-equal? (provided-> z "protected?") #f)
           (check-equal? (provided-> z "") #f)))

  ;; lam->
  (let* ([bd (expr)]
         [z  (lam 'name '() 3 '() #f '#() '() #f 1 bd)])
    (begin (check-equal? (lam-> z "body") bd)
           (check-equal? (lam-> z "name") #f)
           (check-equal? (lam-> z "flags") #f)
           (check-equal? (lam-> z "num-params") #f)
           (check-equal? (lam-> z "param-types") #f)
           (check-equal? (lam-> z "rest?") #f)
           (check-equal? (lam-> z "closure-map") #f)
           (check-equal? (lam-> z "closure-types") #f)
           (check-equal? (lam-> z "toplevel-map") #f)
           (check-equal? (lam-> z "max-let-depth") #f)
           (check-equal? (lam-> z "") #f)))

  ;; closure->
  (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
         [z  (closure lm 'genid)])
    (begin (check-equal? (closure-> z "code") lm)
           (check-equal? (closure-> z "gen-id") #f)
           (check-equal? (closure-> z "") #f)))

  ;; case-lam->
  (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
         [cl (closure lm 'id)]
         [cls (list lm cl lm)]
         [z   (case-lam 'name cls)])
    (begin (check-equal? (case-lam-> z "clauses") cls)
           (check-equal? (case-lam-> z "name") #f)
           (check-equal? (case-lam-> z "") #f)))

  ;; let-one->
  (let* ([rhs (expr)]
         [bdy (expr)]
         [z   (let-one rhs bdy #f #f)]
         ;; Testing any/c rhs and body
         [z*  (let-one #f #f #f #f)])
    (begin (check-equal? (let-one-> z "rhs") rhs)
           (check-equal? (let-one-> z "body") bdy)
           (check-equal? (let-one-> z "type") #f)
           (check-equal? (let-one-> z "unused?") #f)
           (check-equal? (let-one-> z "") #f)
           (check-equal? (let-one-> z* "rhs") #f)
           (check-equal? (let-one-> z* "body") #f)))

  ;; let-void->
  (let* ([bdy (expr)]
         [z   (let-void 1 #f bdy)]
         [z*  (let-void 1 #f #f)])
    (begin (check-equal? (let-void-> z "body") bdy)
           (check-equal? (let-void-> z "count") #f)
           (check-equal? (let-void-> z "boxes") #f)
           (check-equal? (let-void-> z "") #f)
           (check-equal? (let-void-> z* "body") #f)))

  ;; install-value->
  (let* ([rhs (expr)]
         [bdy (expr)]
         [z   (install-value 2 3 #f rhs bdy)]
         [z*  (install-value 0 0 #f #f #f)])
    (begin (check-equal? (install-value-> z "rhs") rhs)
           (check-equal? (install-value-> z "body") bdy)
           (check-equal? (install-value-> z "count") #f)
           (check-equal? (install-value-> z "pos") #f)
           (check-equal? (install-value-> z "boxes?") #f)
           (check-equal? (install-value-> z "") #f)
           (check-equal? (install-value-> z* "rhs") #f)
           (check-equal? (install-value-> z* "body") #f)))

  ;; let-rec->
  (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
         [pcs (list lm lm)]
         [bdy (expr)]
         [z   (let-rec pcs bdy)]
         [z*  (let-rec '() '())])
    (begin (check-equal? (let-rec-> z "procs") pcs)
           (check-equal? (let-rec-> z "body") bdy)
           (check-equal? (let-rec-> z "") #f)
           (check-equal? (let-rec-> z* "procs") '())
           (check-equal? (let-rec-> z* "body") #f)))

  ;; boxenv->
  (let* ([bdy (expr)]
         [z   (boxenv 2 bdy)]
         [z*  (boxenv 3 4)])
    (begin (check-equal? (boxenv-> z "body") bdy)
           (check-equal? (boxenv-> z "pos") #f)
           (check-equal? (boxenv-> z "") #f)
           (check-equal? (boxenv-> z* "body") #f)))

  ;; localref->
  (let ([z (localref #t 1 #t #t #f)])
    (begin (check-equal? (localref-> z "unbox?") #f)
           (check-equal? (localref-> z "pos") #f)
           (check-equal? (localref-> z "clear?") #f)
           (check-equal? (localref-> z "other-clears?") #f)
           (check-equal? (localref-> z "type") #f)
           (check-equal? (localref-> z "") #f)))

  ;; toplevel->
  (let ([z (toplevel 1 2 #f #f)])
    (begin (check-equal? (toplevel-> z "depth") #f)
           (check-equal? (toplevel-> z "pos") #f)
           (check-equal? (toplevel-> z "const?") #f)
           (check-equal? (toplevel-> z "ready?") #f)
           (check-equal? (toplevel-> z "") #f)))

  ;; topsyntax->
  (let ([z (topsyntax 1 2 3)])
    (begin (check-equal? (topsyntax-> z "depth") #f)
           (check-equal? (topsyntax-> z "pos") #f)
           (check-equal? (topsyntax-> z "midpt") #f)
           (check-equal? (topsyntax-> z "") #f)))

  ;; application->
  (let* ([e (expr)]
         [s (seq '())]
         [z (application s (list e s s '() 'any 54 e))])
    (begin (check-equal? (application-> z "rator") s)
           (check-equal? (application-> z "rands") (list e s s e))
           (check-equal? (application-> z "") #f)))

  ;; branch->
  (let* ([z (branch (expr) (expr) (expr))]
         [z* (branch #f #f #f)])
    (begin (check-equal? (branch-> z "test") (expr))
           (check-equal? (branch-> z "then") (expr))
           (check-equal? (branch-> z "else") (expr))
           (check-equal? (branch-> z "") #f)
           (check-equal? (branch-> z* "test") #f)
           (check-equal? (branch-> z* "then") #f)
           (check-equal? (branch-> z* "else") #f)))

  ;; with-cont-mark->
  (let* ([z (with-cont-mark (expr) (expr) (expr))]
         [z* (with-cont-mark #f #f #f)])
    (begin (check-equal? (with-cont-mark-> z "key") (expr))
           (check-equal? (with-cont-mark-> z "val") (expr))
           (check-equal? (with-cont-mark-> z "body") (expr))
           (check-equal? (with-cont-mark-> z "") #f)
           (check-equal? (with-cont-mark-> z* "key") #f)
           (check-equal? (with-cont-mark-> z* "val") #f)
           (check-equal? (with-cont-mark-> z* "body") #f)))

  ;; beg0->
  (let ([z (beg0 (list (expr) 'asdf (expr)))])
    (begin (check-equal? (beg0-> z "seq") (list (expr) (expr)))
           (check-equal? (beg0-> z "")    #f)))

  ;; varref->
  (let* ([tl (toplevel 1 1 #f #f)]
         [z  (varref tl tl)]
         [z* (varref #t #f)])
    (begin (check-equal? (varref-> z "toplevel") tl)
           (check-equal? (varref-> z "dummy") tl)
           (check-equal? (varref-> z "") #f)
           (check-equal? (varref-> z* "dummy") #f)
           (check-equal? (varref-> z* "toplevel") #f)))

  ;; assign->
  (let* ([id  (toplevel 1 1 #f #f)]
         [rhs (expr)]
         [z   (assign id rhs #t)]
         [z*  (assign id #f #t)])
    (begin (check-equal? (assign-> z "id") id)
           (check-equal? (assign-> z "rhs") rhs)
           (check-equal? (assign-> z "undef-ok?") #f)
           (check-equal? (assign-> z "") #f)
           (check-equal? (assign-> z* "rhs") #f)))

  ;; apply-values->
  (let* ([z (apply-values (expr) (expr))]
         [z* (apply-values #f #f)])
    (begin (check-equal? (apply-values-> z "proc") (expr))
           (check-equal? (apply-values-> z "args-expr") (expr))
           (check-equal? (apply-values-> z "") #f)
           (check-equal? (apply-values-> z* "proc") #f)
           (check-equal? (apply-values-> z* "args-expr") #f)))

  ;; primval->
  (let ([z (primval 420)])
    (begin (check-equal? (primval-> z "id") #f)
           (check-equal? (primval-> z "") #f)))

  ;; top-level-rename->
  (let* ([z (top-level-rename #t)])
    (begin (check-equal? (top-level-rename-> z "flag") #f)
           (check-equal? (top-level-rename-> z "") #f)))

  ;; mark-barrier->
  (let* ([z (mark-barrier 'val)])
    (begin (check-equal? (mark-barrier-> z "value") #f)
           (check-equal? (mark-barrier-> z "") #f)))

  ;; lexical-rename->
  (let* ([mpi (module-path-index-join #f #f)]
         [fii (free-id-info mpi 'A mpi 'B #f 101 #f #f)]
         [alist (list (cons 'A 'B)
                      (cons 'C (cons 'D fii))
                      (cons 'F (cons 'G (cons 'H 'I))))]
         [z (lexical-rename #f #f alist)])
    (begin (check-equal? (lexical-rename-> z "alist") (list fii))
           (check-equal? (lexical-rename-> z "bool2") #f)
           (check-equal? (lexical-rename-> z "has-free-id-renames?") #f)
           (check-equal? (lexical-rename-> z "") #f)))

  ;; phase-shift->
  (let ([z (phase-shift #f #f #f #f)])
    (begin (check-equal? (phase-shift-> z "amt") #f)
           (check-equal? (phase-shift-> z "src") #f)
           (check-equal? (phase-shift-> z "dest") #f)
           (check-equal? (phase-shift-> z "cancel-id") #f)
           (check-equal? (phase-shift-> z "") #f)))

  ;; module-rename->
  (let* ([mpi (module-path-index-join #f #f)]
         [ums (list (all-from-module mpi #f #f '() #f '()))]
         [bds (list (cons 'A (module-binding)))]
         [z (module-rename #f 'marked 'setid ums bds 'any #f)])
    (begin (check-equal? (module-rename-> z "unmarshals") ums)
           (check-equal? (module-rename-> z "renames") (list (module-binding)))
           (check-equal? (module-rename-> z "phase") #f)
           (check-equal? (module-rename-> z "kind") #f)
           (check-equal? (module-rename-> z "set-id") #f)
           (check-equal? (module-rename-> z "mark-renames") #f)
           (check-equal? (module-rename-> z "plus-kern") #f)
           (check-equal? (module-rename-> z "") #f)))

  ;; wrap-mark->
  (let ([z (wrap-mark 121)])
    (begin (check-equal? (wrap-mark-> z "val") #f)
           (check-equal? (wrap-mark-> z "") #f)))

  ;; prune->
  (let ([z (prune 'anything-at-all)])
    (begin (check-equal? (prune-> z "sym") #f)
           (check-equal? (prune-> z "") #f)))

  ;; simple-module-binding->
  (let* ([mpi (module-path-index-join #f #f)]
         [z (simple-module-binding mpi)])
    (begin (check-equal? (simple-module-binding-> z "path") #f)
           (check-equal? (simple-module-binding-> z "") #f)))

  ;; phased-module-binding->
  (let* ([mpi (module-path-index-join #f #f)]
         [np (nominal-path)]
         [z (phased-module-binding mpi 1 'any np 'any2)])
    (begin (check-equal? (phased-module-binding-> z "path") #f)
           (check-equal? (phased-module-binding-> z "phase") #f)
           (check-equal? (phased-module-binding-> z "export-name") #f)
           (check-equal? (phased-module-binding-> z "nominal-path") np)
           (check-equal? (phased-module-binding-> z "nominal-export-name") #f)
           (check-equal? (phased-module-binding-> z "") #f)))

  ;; exported-nominal-module-binding->
  (let* ([mpi (module-path-index-join #f #f)]
         [np (nominal-path)]
         [z (exported-nominal-module-binding mpi 'any np 'any)])
    (begin (check-equal? (exported-nominal-module-binding-> z "path") #f)
           (check-equal? (exported-nominal-module-binding-> z "export-name") #f)
           (check-equal? (exported-nominal-module-binding-> z "nominal-path") np)
           (check-equal? (exported-nominal-module-binding-> z "nominal-export-name") #f)
           (check-equal? (exported-nominal-module-binding-> z "") #f)))

  ;; nominal-module-binding->
  (let* ([mpi (module-path-index-join #f #f)]
         [np (nominal-path)]
         [z (nominal-module-binding mpi np)])
    (begin (check-equal? (nominal-module-binding-> z "path") #f)
           (check-equal? (nominal-module-binding-> z "nominal-path") np)
           (check-equal? (nominal-module-binding-> z "") #f)))

  ;; exported-module-binding->
  (let* ([mpi (module-path-index-join #f #f)]
         [z (exported-module-binding mpi 'any)])
    (begin (check-equal? (exported-module-binding-> z "path") #f)
           (check-equal? (exported-module-binding-> z "export-name") #f)
           (check-equal? (exported-module-binding-> z "") #f)))

  ;; simple-nominal-path->
  (let* ([mpi (module-path-index-join #f #f)]
         [z (simple-nominal-path mpi)])
    (begin (check-equal? (simple-nominal-path-> z "value") #f)
           (check-equal? (simple-nominal-path-> z "") #f)))

  ;; imported-nominal-path->
  (let* ([mpi (module-path-index-join #f #f)]
         [z (imported-nominal-path mpi 12423)])
    (begin (check-equal? (imported-nominal-path-> z "value") #f)
           (check-equal? (imported-nominal-path-> z "import-phase") #f)
           (check-equal? (imported-nominal-path-> z "") #f)))

  ;; phased-nominal-path->
  (let* ([mpi (module-path-index-join #f #f)]
         [z (phased-nominal-path mpi #f 8)])
    (begin (check-equal? (phased-nominal-path-> z "value") #f)
           (check-equal? (phased-nominal-path-> z "import-phase") #f)
           (check-equal? (phased-nominal-path-> z "phase") #f)
           (check-equal? (phased-nominal-path-> z "") #f)))

  ;; expr-or-seq?
  (check-true (expr-or-seq? (expr)))
  (check-true (expr-or-seq? (branch #t #t #t)))
  (check-true (expr-or-seq? (application (expr) (list expr))))
  (check-true (expr-or-seq? (seq '())))

  (check-false (expr-or-seq? 'asdf))
  (check-false (expr-or-seq? "yolo"))
  (check-false (expr-or-seq? (nominal-path)))
  (check-false (expr-or-seq? (form)))
)
