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

;; -----------------------------------------------------------------------------

(require compiler/zo-structs
         racket/match
         (only-in racket/list empty? empty)
         (only-in zordoz/private/dispatch-table make-table))

;; =============================================================================

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
   expr
   stx-obj
   wrap
   module-shift
   scope
   multi-scope
   binding
   provided
   all-from-module))

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

(define binding->
  (make-table
   #:action ->
   module-binding
   decoded-module-binding
   local-binding
   free-id=?-binding))

;; --- getters

(define (compilation-top-> z field-name)
  ;; (-> compilation-top? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["prefix"
     (compilation-top-prefix z)]
    ["code"
     (define res (compilation-top-code z))
     (if (form? res) res #f)]
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
    ["content"
     (stx-content z)]
    [_  #f]))

(define (all-from-module-> z field-name)
 ;; (-> all-from-module? string? (or/c (listof zo?) zo? #f))
  #f)

;; --- form

(define (def-values-> z field-name)
  ;; (-> def-values? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["ids"
     (filter toplevel? (def-values-ids z))]
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
    ;; "binding-names" does have stx? inside, but they're within a hashtable
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
       [_ #f])]
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

;; --- stx-obj

(define
  (stx-obj-> z field-name)
  (match field-name
    ["wrap"
     (stx-obj-wrap z)]
    [_ #f]))

;; --- wrap

(define
  (wrap-> z field-name)
  (match field-name
    ["shifts"
     (wrap-shifts z)]
    ["simple-scopes"
     (wrap-simple-scopes z)]
    ["multi-scopes"
     (map car (wrap-multi-scopes z))]
    [_ #f]))

;; --- misc. syntax

(define
  (module-shift-> z field-name)
  (match field-name
    [_ #f]))

(define
  (scope-> z field-name)
  (define (get-bindings bs)
    (cond [(empty? bs) '()]
          [else (append (cadar bs) (cddar bs) (get-bindings (cdr bs)))]))
  (define (get-bulk-bindings bbs)
    (cond [(empty? bbs) '()]
          [else (append (caar bbs) (cdar bbs) (get-bulk-bindings (cdr bbs)))]))
  (match field-name
    ["bindings"
     (get-bindings (scope-bindings z))]
    ["bulk-bindings"
     (get-bulk-bindings (scope-bulk-bindings z))]
    ["multi-owner"
     (scope-multi-owner z)]
    [_ #f]))

(define
  (multi-scope-> z field-name)
  (match field-name
    ["scopes"
     (map cadr (multi-scope-scopes z))]
    [_ #f]))

(define
  (module-binding-> z field-name)
  (match field-name
    [_ #f]))

(define
  (decoded-module-binding-> z field-name)
  (match field-name
    [_ #f]))

(define
  (local-binding-> z field-name)
  #f)

(define
  (free-id=?-binding-> z field-name)
  (match field-name
    ["base"
     (free-id=?-binding-base z)]
    ["id"
     (free-id=?-binding-id z)]
    [_ #f]))

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
  (let* ([px (prefix 0 '() '() 'x)]
         [cd (form)]
         [z  (compilation-top 0 (make-hash '((asdf . 2))) px cd)])
    (begin (check-equal? (compilation-top-> z "prefix") px)
           (check-equal? (compilation-top-> z "code")   cd)
           (check-equal? (compilation-top-> z "max-let-depth")   #f)
           (check-equal? (compilation-top-> z "binding-namess")   #f)
           (check-equal? (compilation-top-> z "")       #f)))

  ;; prefix->
  (let* ([mpi (module-path-index-join #f #f)]
         [gb (global-bucket 'NAME)]
         [mv (module-variable mpi 'SYM 0 0 #f)]
         [sx (stx (stx-obj 'x (wrap '() '() '()) #f (make-hash) 'tainted))]
         [z  (prefix 0
                    (list gb mv)
                    (list sx)
                    'inspector)])
    (begin (check-equal? (prefix-> z "toplevels") (list gb mv))
           (check-equal? (prefix-> z "stxs")      (list sx))
           (check-equal? (prefix-> z "num-lifts") #f)
           (check-equal? (prefix-> z "src-inspector-desc") #f)
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
  (let* ([s (stx-obj 'a (wrap '() '() '()) #f (make-hash) 'clean)]
         [z (stx s)])
    (begin (check-equal? (stx-> z "content") s)
           (check-equal? (stx-> z "")        #f)))

  ;; form-> (this is better tested by the specific tests for 'def-values->', 'req->', ...)
  (let* ([z (form)])
    (check-equal? (form-> z "") #f))

  ;; expr-> (see tests for specific expressions below
  (let* ([z (expr)])
    (check-equal? (expr-> z "") #f))

  ;; stx-obj->
  (let* ([w (wrap '() '() '())]
         [z (stx-obj 'a w (srcloc 'asdf 6 6 6 #f) (make-hash '((t . 1))) 'clean)])
    (begin (check-equal? (stx-obj-> z "datum") #f)
           (check-equal? (stx-obj-> z "wrap") w)
           (check-equal? (stx-obj-> z "srcloc") #f)
           (check-equal? (stx-obj-> z "props") #f)
           (check-equal? (stx-obj-> z "tamper-status") #f)))

  ;; wrap->
  (let* ([ms (module-shift #f #f 'a 'b)]
         [sc (scope 3 'b '() '() #f)]
         [msc (multi-scope 1 '2 '())]
         [z (wrap (list ms) (list sc) (list (list msc 5)))])
    (begin (check-equal? (wrap-> z "shifts") (list ms))
           (check-equal? (wrap-> z "simple-scopes") (list sc))
           (check-equal? (wrap-> z "multi-scopes") (list msc))))


  ;; module-shift->
  (let* ([ms (module-shift #f #f 'a 'b)])
    (begin (check-equal? (module-shift-> ms "from") #f)
           (check-equal? (module-shift-> ms "to") #f)
           (check-equal? (module-shift-> ms "from-inspector-desc") #f)
           (check-equal? (module-shift-> ms "to-inspector-desc") #f)
           (check-equal? (module-shift-> ms "") #f)))

  ;; scope->
  (let* ([mpi (module-path-index-join #f #f)]
         [afm (all-from-module mpi #f #f 'x '() #f)]
         [b (binding)]
         [s (scope 5 'blah '() '() #f)]
         [ms (multi-scope 6 'name '())]
         [mb (module-binding #t)]
         [z (scope 'root 'kind `((sym (,s) ,mb)) `(((,s) ,afm)) ms)])
    (begin (check-equal? (scope-> z "bindings") (list s mb))
           (check-equal? (scope-> z "bulk-bindings") (list s afm))
           (check-equal? (scope-> z "name") #f)
           (check-equal? (scope-> z "kind") #f)
           (check-equal? (scope-> z "multi-owner") ms)))

  ;; multi-scope->
  (let* ([s (scope 1 'a '() '() #f)]
         [z (multi-scope 4 'yolo `((#f ,s) (1 ,s)))])
    (begin (check-equal? (multi-scope-> z "name") #f)
           (check-equal? (multi-scope-> z "src-name") #f)
           (check-equal? (multi-scope-> z "scopes") (list s s))))

  ;; binding->
  (let* ([z (binding)])
    (check-equal? (binding-> z "") #f))

  ;; all-from-module->
  (let* ([mpi (module-path-index-join #f #f)]
         [z (all-from-module mpi 2 3 'gadget '(ex1 ex2) 'pre)])
    (begin (check-equal? (all-from-module-> z "path") #f)
           (check-equal? (all-from-module-> z "phase") #f)
           (check-equal? (all-from-module-> z "src-phase") #f)
           (check-equal? (all-from-module-> z "inspector-desc") #f)
           (check-equal? (all-from-module-> z "exceptions") #f)
           (check-equal? (all-from-module-> z "prefix") #f)))

  ;; module-binding->
  (let* ([z (module-binding 'any)])
    (begin (check-equal? (module-binding-> z "encoded") #f)
           (check-equal? (module-binding-> z "encoded") #f)))

  ;; decoded-module-binding->
  (let* ([mpi (module-path-index-join #f #f)]
        [z (decoded-module-binding mpi 'foo 1 mpi 'ex 0 0 'hi)])
    (begin (check-equal? (decoded-module-binding-> z "path") #f)
           (check-equal? (decoded-module-binding-> z "name") #f)
           (check-equal? (decoded-module-binding-> z "phase") #f)
           (check-equal? (decoded-module-binding-> z "nominal-path") #f)
           (check-equal? (decoded-module-binding-> z "nominal-export-name") #f)
           (check-equal? (decoded-module-binding-> z "nominal-phase") #f)
           (check-equal? (decoded-module-binding-> z "import-phase") #f)
           (check-equal? (decoded-module-binding-> z "import-desc") #f)))

  ;; local-binding->
  (let ([z (local-binding 'hello)])
    (begin (check-equal? (local-binding-> z "name") #f)
           (check-equal? (local-binding-> z "") #f)))

  ;; free-id=?-binding->
  (let* ([lb (local-binding 'hi)]
        [stx (stx-obj 0 (wrap '() '() '()) #f (make-hash) 'armed)]
        [z (free-id=?-binding lb stx 3)])
    (begin (check-equal? (free-id=?-binding-> z "base") lb)
           (check-equal? (free-id=?-binding-> z "id") stx)
           (check-equal? (free-id=?-binding-> z "phase") #f)))

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
         [px  (prefix 0 '() '() 'yes)]
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
         [px  (prefix 0 '() '() 'pacman)]
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
  (let* ([sx (stx (stx-obj 'XXX (wrap '() '() '()) #f (make-hash) 'clean))]
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
         [px  (prefix 0 '() '() 'a)]
         [pd1 (provided 'p1 #f 'B 'C 13 #t)]
         [pd2 (provided 'p2 #f 'asdf 'C 6 #f)]
         [pd3 (provided 'p3 #f 'B 'lol 832 #t)]
         [pd4 (provided 'p4 #f 'R 'xx 1 #t)]
         [pvs (list (list #f (list pd1 pd2) (list pd3))
                    (list #f (list pd4) '()))]
         [bd  (list (form) 'any)]
         [ds  (def-syntaxes '() (expr) (prefix 0 '() '() 'r) 1 #f)]
         [sfs (seq-for-syntax '() (prefix 0 '() '() 'qq) 999 (toplevel 9 9 #t #t))]
         [sb  (list (cons 7 (list ds))
                    (cons 8 (list sfs)))]
         [dm  (toplevel 1 1 #f #f)]
         [ic  (stx (stx-obj 'dirty (wrap '() '() '()) #f (make-hash) 'clean))]
         [m1  (mod 'm1 'm1src mpi px pvs '() bd sb '() 0 dm #f ic (hash) '() '() '())]
         [m2  (mod 'm2 'm2src mpi px pvs '() bd sb '() 0 dm #f ic (hash) '() '() '())]
         [m3  (mod 'm3 'm3src mpi px pvs '() bd sb '() 0 dm #f ic (hash) '() '() '())]
         [prs (list m1 m2)]
         [pts (list m3)]
         [z   (mod 'name 'srcname mpi px pvs '() bd sb '() 0 dm #f ic (hash) '() prs pts)])
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
           (check-equal? (mod-> z "binding-names") #f)
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

  ;; expr-or-seq?
  (check-true (expr-or-seq? (expr)))
  (check-true (expr-or-seq? (branch #t #t #t)))
  (check-true (expr-or-seq? (application (expr) (list expr))))
  (check-true (expr-or-seq? (seq '())))

  (check-false (expr-or-seq? 'asdf))
  (check-false (expr-or-seq? "yolo"))
  (check-false (expr-or-seq? (form)))
)
