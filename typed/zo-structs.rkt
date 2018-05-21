#lang typed/racket


;; A Spec is the name of a zo struct and a list of pairs representing its fields:
;; - The car of each field is the name of that field
;; - The cdr of each field is a thunk for building a representation of the field's value.
;;   If the value is a zo-struct, the thunk should build a Spec
;;   Otherwise, the thunk should build a string
(define-type Spec
  (Rec Spec
   (Pair String (Listof (Pair String (-> (U Spec String)))))))
(define-type Zo zo)
(provide Spec Zo)

(require/typed/provide
  compiler/zo-structs
  [#:struct zo ()]
  [#:struct (linkl-directory zo) (
    [table : (HashTable (Listof Symbol) linkl-bundle)])]
  [#:struct (linkl-bundle zo) (
    [table : (HashTable (U Symbol Fixnum) (U linkl Any))])]
  [#:struct (linkl zo) (
    [name : Symbol]
    [importss : (Listof (Listof Symbol))]
    [import-shapess : (Listof (Listof (U #f 'constant 'fixed function-shape struct-shape)))]
    [exports : (Listof Symbol)]
    [internals : (Listof (U Symbol #f))]
    [lifts : (Listof Symbol)]
    [source-names : (HashTable Symbol Symbol)]
    [body : (Listof (U form Any))]
    [max-let-depth : Exact-Nonnegative-Integer]
    [need-instance-access? : Boolean])]
  [#:struct function-shape (
    [arity : (U Natural arity-at-least (Listof (U Natural arity-at-least)))]
    [preserves-marks? : Boolean])] ;; bennn: got type from (:print-type procedure-arity)
  [#:struct struct-shape ()]
  [#:struct (struct-type-shape struct-shape) (
    [field-count : Exact-Nonnegative-Integer]
    [authentic? : Boolean])]
  [#:struct (constructor-shape struct-shape) (
    [arity : Exact-Nonnegative-Integer])]
  [#:struct (predicate-shape struct-shape) (
    [authentic? : Boolean])]
  [#:struct (accessor-shape struct-shape) (
    [field-count : Exact-Nonnegative-Integer]
    [authentic? : Boolean])]
  [#:struct (mutator-shape struct-shape) (
    [field-count : Exact-Nonnegative-Integer]
    [authentic? : Boolean])]
  [#:struct (struct-type-property-shape struct-shape) (
    [has-guard? : Boolean])]
  [#:struct (property-predicate-shape struct-shape) ()]
  [#:struct (property-accessor-shape struct-shape) ()]
  [#:struct (struct-other-shape struct-shape) ()]
  [#:struct (form zo) ()]
  [#:struct (expr form) ()]
  [#:struct (def-values form) (
    [ids : (Listof toplevel)]
    [rhs : (U expr seq inline-variant Any)])]
  [#:struct (inline-variant zo) (
    [direct : expr]
    [inline : expr])]
  [#:struct (seq form) (
    [forms : (Listof (U expr Any))])]
  [#:struct (lam expr) (
    [name : (U Symbol (Vectorof Any))] ;empty list
    [flags : (Listof (U 'preserves-marks 'is-method 'single-result
                         'only-rest-arg-not-used 'sfs-clear-rest-args))]
    [num-params : Exact-Nonnegative-Integer]
    [param-types : (Listof (U 'val 'ref 'flonum 'fixnum 'extflonum))]
    [rest? : Boolean]
    [closure-map : (Vectorof Exact-Nonnegative-Integer)]
    [closure-types : (Listof (U 'val/ref 'flonum 'fixnum 'extflonum))]
    [toplevel-map : (U #f (Setof Exact-Nonnegative-Integer))]
    [max-let-depth : Exact-Nonnegative-Integer]
    [body : (U expr seq Any)])]
  [#:struct (closure expr) (
    [code : lam]
    [gen-id : Symbol])]
  [#:struct (case-lam expr) (
    [name : (U Symbol (Vectorof Any) (List ))]
    [clauses : (Listof (U lam closure))])]
  [#:struct (let-one expr) (
    [rhs : (U expr seq Any)]  ; pushes one value onto stack
    [body : (U expr seq Any)] 
    [type : (U #f 'flonum 'fixnum 'extflonum)]
    [unused? : Boolean])]
  [#:struct (let-void expr) (
    [count : Exact-Nonnegative-Integer]
    [boxes? : Boolean]
    [body : (U expr seq Any)])]
  [#:struct (install-value expr) (
    [count : Exact-Nonnegative-Integer]
    [pos : Exact-Nonnegative-Integer] 
    [boxes? : Boolean] 
    [rhs : (U expr seq Any)] 
    [body : (U expr seq Any)])]
  [#:struct (let-rec expr) (
    [procs : (Listof lam)]
    [body : (U expr seq Any)])]
  [#:struct (boxenv expr) (
    [pos : Exact-Nonnegative-Integer]
    [body : (U expr seq Any)])]
  [#:struct (localref expr) (
    [unbox? : Boolean] 
    [pos : Exact-Nonnegative-Integer] 
    [clear? : Boolean] 
    [other-clears? : Boolean] 
    [type : (U #f 'flonum 'fixnum 'extflonum)])]
  [#:struct (toplevel expr) (
    [depth : Exact-Nonnegative-Integer] 
    [pos : Exact-Nonnegative-Integer] 
    [const? : Boolean] 
    [ready? : Boolean])]
  [#:struct (application expr) (
    [rator : (U expr seq Any)]
    [rands : (Listof (U expr seq Any))])]
  [#:struct (branch expr) (
    [test : (U expr seq Any)]
    [then : (U expr seq Any)]
    [else : (U expr seq Any)])]
  [#:struct (with-cont-mark expr) (
    [key : (U expr seq Any)] 
    [val : (U expr seq Any)] 
    [body : (U expr seq Any)])]
  [#:struct (beg0 expr) (
    [seq : (Listof (U expr seq Any))])]
  [#:struct (varref expr) (
    [toplevel : (U toplevel #t #f Symbol)]
    [dummy : (U toplevel #f)]
    [constant? : Boolean]
    [from-unsafe? : Boolean])]
  [#:struct (assign expr) (
    [id : toplevel]
    [rhs : (U expr seq Any)]
    [undef-ok? : Boolean])]
  [#:struct (apply-values expr) (
    [proc : (U expr seq Any)]
    [args-expr : (U expr seq Any)])]
  [#:struct (with-immed-mark expr) (
    [key : (U expr seq Any)]
    [def-val : (U expr seq Any)]
    [body : (U expr seq Any)])]
  [#:struct (primval expr) ([id : Exact-Nonnegative-Integer])]
)
