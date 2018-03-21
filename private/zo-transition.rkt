#lang racket/base

;; Access the fields of a struct by name at runtime.

;; Uses predicates to guess what struct its argument is,
;; then compares strings with statically-known field names.
;; Functions that end with '->' are the specific transition function
;; for a type of zo struct.

(provide
  zo-transition
  ;; (-> zo? string? (values (or/c zo? (listof zo?)) boolean?))
  ;; Access "structName-fieldName myStruct" at runtime.
)

;; -----------------------------------------------------------------------------

(require
  compiler/zo-structs
  ;zordoz/typed/zo-structs ;; For testing
  racket/match
  (only-in zordoz/private/dispatch-table make-table)
)

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
   linkl-directory
   linkl-bundle
   linkl
   form
   expr))

(define form->
  (make-table
   #:action ->
   def-values
   seq
   inline-variant
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

;; --- getters

(define (linkl-directory-> ld field-name)
  (match field-name
    ["table"
     (linkl-directory-table ld)]
    [_ #f]))

(define (linkl-bundle-> lb field-name)
  (match field-name
    ["table"
     (linkl-bundle-table lb)]
    [_ #f]))

(define (linkl-> ll field-name)
  (match field-name
    ["body"
     (linkl-body ll)]
    [_ #f]))

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

(define (seq-> z field-name)
  ;; (-> seq? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["forms"
     (filter form? (seq-forms z))]
    [_ #f]))

(define (inline-variant-> z field-name)
  ;; (-> inline-variant? string? (or/c (listof zo?) zo? #f))
  (match field-name
    ["direct"
     (inline-variant-direct z)]
    ["inline"
     (inline-variant-inline z)]
    [_ #f]))

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

(define (with-immed-mark-> z field-name)
  (match field-name
    ["key"
     (match (with-immed-mark-key z)
       [(? expr-or-seq? proc) proc]
       [_ #f])]
    ["def-val"
     (match (with-immed-mark-def-val z)
       [(? expr-or-seq? args-expr) args-expr]
       [_ #f])]
    ["body"
     (match (with-immed-mark-body z)
       [(? expr-or-seq? proc) proc]
       [_ #f])]
    [_ #f]))

(define (primval-> z field-name)
  ;; (-> primval? string? (or/c (listof zo?) zo? #f))
  #f)

;; --- helpers

;; True if the argument is an 'expr' or a 'seq' zo struct.
(define (expr-or-seq? x)
  ;; (-> any/c boolean?)
  (or (expr? x) (seq? x)))

;; -----------------------------------------------------------------------------
;; --- testing

(module+ test
  (require rackunit compiler/zo-structs)

  (test-case "linkl-directory->"
    (let* ([lb (linkl-bundle (make-hash (list (cons 'B #true))))]
           [t (make-hash (list (cons '(A) lb)))]
           [z (linkl-directory t)])
      (check-equal? (linkl-directory-> z "table") t)
      (check-equal? (linkl-directory-> z "") #f)))

  (test-case "linkl-bundle->"
    (let* ([ll (linkl 'dummy '() '() '() '() '() (make-hash) '() 0 #false)]
           [t (make-hash (list (cons 'A #true) (cons 44 ll)))]
           [z (linkl-bundle t)])
      (check-equal? (linkl-bundle-> z "table") t)
      (check-equal? (linkl-bundle-> z "") #false)))

  (test-case "linkl->"
    (let* ([z (linkl 'name '((import)) '((constant) (#false)) '(exp)
                     '(internals #f) '(lifts) (make-hash '((src . names)))
                     '(body) 8 #t)])
      (check-equal? (linkl-> z "name") #false)
      (check-equal? (linkl-> z "importss") #false)
      (check-equal? (linkl-> z "import-shapess") #false)
      (check-equal? (linkl-> z "import-shapess") #false)
      (check-equal? (linkl-> z "exports") #false)
      (check-equal? (linkl-> z "internals") #false)
      (check-equal? (linkl-> z "lifts") #false)
      (check-equal? (linkl-> z "source-names") #false)
      (check-equal? (linkl-> z "body") '(body))
      (check-equal? (linkl-> z "max-let-depth") #false)
      (check-equal? (linkl-> z "need-instance-access?") #false)))

  (test-case "form->"
    ;; (this is better tested by the specific tests for 'def-values->', 'req->', ...)
    (let* ([z (form)])
      (check-equal? (form-> z "") #f)))

  (test-case "expr-> (see tests for specific expressions below)"
    (let* ([z (expr)])
      (check-equal? (expr-> z "") #f)))

  (test-case "def-values->"
    (let* ([ids (list (toplevel 1 2 #t #f))]
           [rhs (expr)]
           [z (def-values ids rhs)])
      (begin (check-equal? (def-values-> z "ids") ids)
             (check-equal? (def-values-> z "rhs") rhs)
             (check-equal? (def-values-> z "") #f))))

  (test-case "seq->"
    (let* ([fms (list (form) (form) (form))]
           [z   (seq fms)]
           [z*  (seq '(N O T F O R M S))])
      (begin (check-equal? (seq-> z "forms") fms)
             (check-equal? (seq-> z "") #f)
             (check-equal? (seq-> z* "forms") '())
             (let-values ([(ctx* pass?) (zo-transition z* "forms")])
               (begin (check-equal? ctx* z*)
                      (check-false pass?))))))

  (test-case "inline-variant->"
    (let* ([dr (expr)]
           [il (expr)]
           [z  (inline-variant dr il)])
      (begin (check-equal? (inline-variant-> z "direct") dr)
             (check-equal? (inline-variant-> z "inline") il)
             (check-equal? (inline-variant-> z "") #f))))

  (test-case "lam->"
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
             (check-equal? (lam-> z "") #f))))

  (test-case "closure->"
    (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
           [z  (closure lm 'genid)])
      (begin (check-equal? (closure-> z "code") lm)
             (check-equal? (closure-> z "gen-id") #f)
             (check-equal? (closure-> z "") #f))))

  (test-case "case-lam->"
    (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
           [cl (closure lm 'id)]
           [cls (list lm cl lm)]
           [z   (case-lam 'name cls)])
      (begin (check-equal? (case-lam-> z "clauses") cls)
             (check-equal? (case-lam-> z "name") #f)
             (check-equal? (case-lam-> z "") #f))))

  (test-case "let-one->"
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
             (check-equal? (let-one-> z* "body") #f))))

  (test-case "let-void->"
    (let* ([bdy (expr)]
           [z   (let-void 1 #f bdy)]
           [z*  (let-void 1 #f #f)])
      (begin (check-equal? (let-void-> z "body") bdy)
             (check-equal? (let-void-> z "count") #f)
             (check-equal? (let-void-> z "boxes") #f)
             (check-equal? (let-void-> z "") #f)
             (check-equal? (let-void-> z* "body") #f))))

  (test-case "install-value->"
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
             (check-equal? (install-value-> z* "body") #f))))

  (test-case "let-rec->"
    (let* ([lm (lam 'nmme '() 3 '() #f '#() '() #f 1 (seq '()))]
           [pcs (list lm lm)]
           [bdy (expr)]
           [z   (let-rec pcs bdy)]
           [z*  (let-rec '() '())])
      (begin (check-equal? (let-rec-> z "procs") pcs)
             (check-equal? (let-rec-> z "body") bdy)
             (check-equal? (let-rec-> z "") #f)
             (check-equal? (let-rec-> z* "procs") '())
             (check-equal? (let-rec-> z* "body") #f))))

  (test-case "boxenv->"
    (let* ([bdy (expr)]
           [z   (boxenv 2 bdy)]
           [z*  (boxenv 3 4)])
      (begin (check-equal? (boxenv-> z "body") bdy)
             (check-equal? (boxenv-> z "pos") #f)
             (check-equal? (boxenv-> z "") #f)
             (check-equal? (boxenv-> z* "body") #f))))

  (test-case "localref->"
    (let ([z (localref #t 1 #t #t #f)])
      (begin (check-equal? (localref-> z "unbox?") #f)
             (check-equal? (localref-> z "pos") #f)
             (check-equal? (localref-> z "clear?") #f)
             (check-equal? (localref-> z "other-clears?") #f)
             (check-equal? (localref-> z "type") #f)
             (check-equal? (localref-> z "") #f))))

  (test-case "toplevel->"
    (let ([z (toplevel 1 2 #f #f)])
      (begin (check-equal? (toplevel-> z "depth") #f)
             (check-equal? (toplevel-> z "pos") #f)
             (check-equal? (toplevel-> z "const?") #f)
             (check-equal? (toplevel-> z "ready?") #f)
             (check-equal? (toplevel-> z "") #f))))

  (test-case "application->"
    (let* ([e (expr)]
           [s (seq '())]
           [z (application s (list e s s '() 'any 54 e))])
      (begin (check-equal? (application-> z "rator") s)
             (check-equal? (application-> z "rands") (list e s s e))
             (check-equal? (application-> z "") #f))))

  (test-case "branch->"
    (let* ([z (branch (expr) (expr) (expr))]
           [z* (branch #f #f #f)])
      (begin (check-equal? (branch-> z "test") (expr))
             (check-equal? (branch-> z "then") (expr))
             (check-equal? (branch-> z "else") (expr))
             (check-equal? (branch-> z "") #f)
             (check-equal? (branch-> z* "test") #f)
             (check-equal? (branch-> z* "then") #f)
             (check-equal? (branch-> z* "else") #f))))

  (test-case "with-cont-mark->"
    (let* ([z (with-cont-mark (expr) (expr) (expr))]
           [z* (with-cont-mark #f #f #f)])
      (begin (check-equal? (with-cont-mark-> z "key") (expr))
             (check-equal? (with-cont-mark-> z "val") (expr))
             (check-equal? (with-cont-mark-> z "body") (expr))
             (check-equal? (with-cont-mark-> z "") #f)
             (check-equal? (with-cont-mark-> z* "key") #f)
             (check-equal? (with-cont-mark-> z* "val") #f)
             (check-equal? (with-cont-mark-> z* "body") #f))))

  (test-case "beg0->"
    (let ([z (beg0 (list (expr) 'asdf (expr)))])
      (begin (check-equal? (beg0-> z "seq") (list (expr) (expr)))
             (check-equal? (beg0-> z "")    #f))))

  (test-case "varref->"
    (let* ([tl (toplevel 1 1 #f #f)]
           [z  (varref tl tl #f #f)]
           [z* (varref #t #f #t #t)])
      (begin (check-equal? (varref-> z "toplevel") tl)
             (check-equal? (varref-> z "dummy") tl)
             (check-equal? (varref-> z "constant?") #f)
             (check-equal? (varref-> z "from-unsafe?") #f)
             (check-equal? (varref-> z "") #f)
             (check-equal? (varref-> z* "dummy") #f)
             (check-equal? (varref-> z* "toplevel") #f)
             (check-equal? (varref-> z* "constant?") #f)
             (check-equal? (varref-> z* "from-unsafe?") #f))))

  (test-case "assign->"
    (let* ([id  (toplevel 1 1 #f #f)]
           [rhs (expr)]
           [z   (assign id rhs #t)]
           [z*  (assign id #f #t)])
      (begin (check-equal? (assign-> z "id") id)
             (check-equal? (assign-> z "rhs") rhs)
             (check-equal? (assign-> z "undef-ok?") #f)
             (check-equal? (assign-> z "") #f)
             (check-equal? (assign-> z* "rhs") #f))))

  (test-case "apply-values->"
    (let* ([z (apply-values (expr) (expr))]
           [z* (apply-values #f #f)])
      (begin (check-equal? (apply-values-> z "proc") (expr))
             (check-equal? (apply-values-> z "args-expr") (expr))
             (check-equal? (apply-values-> z "") #f)
             (check-equal? (apply-values-> z* "proc") #f)
             (check-equal? (apply-values-> z* "args-expr") #f))))

  (test-case "with-immed-mark->"
    (let* ([z (with-immed-mark (expr) (expr) (expr))]
           [z* (with-immed-mark 'x 'y 'z)])
      (begin (check-equal? (with-immed-mark-> z "key") (expr))
             (check-equal? (with-immed-mark-> z "def-val") (expr))
             (check-equal? (with-immed-mark-> z "body") (expr))
             (check-equal? (with-immed-mark-> z "") #f)
             (check-equal? (with-immed-mark-> z* "key") #f)
             (check-equal? (with-immed-mark-> z* "def-val") #f)
             (check-equal? (with-immed-mark-> z* "body") #f))))

  (test-case "primval->"
    (let ([z (primval 420)])
      (begin (check-equal? (primval-> z "id") #f)
             (check-equal? (primval-> z "") #f))))

  (test-case "expr-or-seq?"
    (check-true (expr-or-seq? (expr)))
    (check-true (expr-or-seq? (branch #t #t #t)))
    (check-true (expr-or-seq? (application (expr) (list expr))))
    (check-true (expr-or-seq? (seq '()))))

  (check-false (expr-or-seq? 'asdf))
  (check-false (expr-or-seq? "yolo"))
  (check-false (expr-or-seq? (form)))
)
