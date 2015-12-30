#lang typed/racket


;; A Spec is the name of a zo struct and a list of pairs representing its fields:
;; - The car of each field is the name of that field
;; - The cdr of each field is a thunk for building a representation of the field's value.
;;   If the value is a zo-struct, the thunk should build a Spec
;;   Otherwise, the thunk should build a string
(define-type Spec
  (Rec Spec
   (Pair String (Listof (Pair String (-> (U Spec String)))))))
(provide Spec)

(require/typed/provide compiler/zo-structs
               [#:struct zo ()]
               [#:struct (compilation-top zo) (
                 [max-let-depth : Exact-Nonnegative-Integer]
                 [binding-namess : (HashTable Exact-Nonnegative-Integer
                                              (HashTable Symbol Identifier))]
                 [prefix : prefix]
                 [code : (U form Any)])]
               [#:struct (prefix zo) (
                 [num-lifts : Exact-Nonnegative-Integer] 
                 [toplevels : (Listof (U #f Symbol global-bucket module-variable))] 
                 [src-inspector-desc : Symbol]
                 [stxs : (Listof (U #f stx))])]
               [#:struct (global-bucket zo) ([name : Symbol])]
               [#:struct (module-variable zo) (
                 [modidx : Module-Path-Index] 
                 [sym : Symbol] 
                 [pos : Integer] 
                 [phase : Exact-Nonnegative-Integer]
                 [constantness : (U #f 'constant 'fixed 
                                     function-shape
                                     struct-shape)])]
               [#:struct function-shape (
                 [arity : (U Natural arity-at-least (Listof (U Natural arity-at-least)))]
                 [preserves-marks? : Boolean])] ;; bennn: got type from (:print-type procedure-arity)
               [#:struct struct-shape ()]
               [#:struct (struct-type-shape struct-shape) ([field-count : Exact-Nonnegative-Integer])]
               [#:struct (constructor-shape struct-shape) ([arity : Exact-Nonnegative-Integer])]
               [#:struct (predicate-shape struct-shape) ()]
               [#:struct (accessor-shape struct-shape) ([field-count : Exact-Nonnegative-Integer])]
               [#:struct (mutator-shape struct-shape) ([field-count : Exact-Nonnegative-Integer])]
               [#:struct (struct-other-shape struct-shape) ()]
               [#:struct (stx zo) ([content : stx-obj])]
               [#:struct (stx-obj zo) (
                 [datum : Any]
                 [wrap : wrap]
                 [srcloc : (U #f srcloc)]
                 [props : (HashTable Symbol Any)]
                 [tamper-status : (U 'clean 'armed 'tainted)])]
               [#:struct (form zo) ()]
               [#:struct (expr form) ()]
               [#:struct (binding zo) ()]
               [#:struct (wrap zo) (
                 [shifts : (Listof module-shift)]
                 [simple-scopes : (Listof scope)]
                 [multi-scopes : (Listof (List multi-scope (U #f Integer)))])]
               [#:struct (all-from-module zo) (
                 [path : Module-Path-Index]
                 [phase : (U Integer #f)]
                 [src-phase : (U Integer #f)]
                 [inspector-desc : Symbol]
                 [exceptions : (Listof Symbol)]
                 [prefix : (U Symbol #f)])]
               [#:struct (def-values form) (
                 [ids : (Listof (U toplevel Symbol))]
                 [rhs : (U expr seq inline-variant Any)])]
               [#:struct (def-syntaxes form) (
                 [ids : (Listof (U toplevel Symbol))]
                 [rhs : (U expr seq Any)] 
                 [prefix : prefix] 
                 [max-let-depth : Exact-Nonnegative-Integer]
                 [dummy : (U toplevel #f)])]
               [#:struct (seq-for-syntax form) (
                 [forms : (Listof (U form Any))] ; `begin-for-syntax'
                 [prefix : prefix] 
                 [max-let-depth : Exact-Nonnegative-Integer]
                 [dummy : (U toplevel #f)])]
               [#:struct (req form) (
                 [reqs : stx]
                 [dummy : toplevel])]
               [#:struct (seq form) (
                 [forms : (Listof (U form Any))])]
               [#:struct (splice form) ([forms : (Listof (U form Any))])]
               [#:struct (inline-variant form) (
                 [direct : expr]
                 [inline : expr])]
               [#:struct (mod form) (
                 [name : (U Symbol (Listof Symbol))]
                 [srcname : Symbol]
                 [self-modidx : Module-Path-Index] 
                 [prefix : prefix] 
                 [provides : (Listof (List (U Integer #f)
                                           (Listof provided)
                                           (Listof provided)))] 
                 [requires : (Listof (Pair (U Integer #f)
                                           (Listof Module-Path-Index)))]
                 [body : (Listof (U form Any))]
                 [syntax-bodies : (Listof (Pair Exact-Positive-Integer
                                                (Listof (U def-syntaxes seq-for-syntax))))]
                 [unexported : (Listof (List Exact-Nonnegative-Integer
                                             (Listof Symbol)
                                             (Listof Symbol)))]
                 [max-let-depth : Exact-Nonnegative-Integer]
                 [dummy : toplevel]
                 [lang-info : (U #f (Vector Module-Path Symbol Any))]
                 [internal-context : (U #f #t stx (Vectorof stx))]
                 [binding-names : (HashTable Integer (HashTable Symbol (U #t stx)))]
                 [flags : (Listof (U 'cross-phase))]
                 [pre-submodules : (Listof mod)]
                 [post-submodules : (Listof mod)])]
               [#:struct (provided zo) (
                 [name : Symbol] 
                 [src : (U Module-Path-Index #f)] 
                 [src-name : Symbol] 
                 [nom-src : (U Module-Path-Index #f)]
                 [src-phase : Exact-Nonnegative-Integer] 
                 [protected? : Boolean])]
               [#:struct (lam expr) (
                 [name : (U Symbol (Vectorof Any) (List ))] ;empty list
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
               [#:struct (topsyntax expr) (
                 [depth : Exact-Nonnegative-Integer]
                 [pos : Exact-Nonnegative-Integer]
                 [midpt : Exact-Nonnegative-Integer])]
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
               [#:struct (beg0 expr) ([seq : (Listof (U expr seq Any))])]
               [#:struct (varref expr) (
                 [toplevel : (U toplevel #t)]
                 [dummy : (U toplevel #f)])]
               [#:struct (assign expr) (
                 [id : toplevel]
                 [rhs : (U expr seq Any)]
                 [undef-ok? : Boolean])]
               [#:struct (apply-values expr) (
                 [proc : (U expr seq Any)]
                 [args-expr : (U expr seq Any)])]
               [#:struct (primval expr) ([id : Exact-Nonnegative-Integer])]
               [#:struct (module-shift zo) (
                 [from : (U #f Module-Path-Index)]
                 [to : (U #f Module-Path-Index)]
                 [from-inspector-desc : (U #f Symbol)]
                 [to-inspector-desc : (U #f Symbol)])]
               [#:struct (scope zo) (
                 [name : (U 'root Natural)]
                 [kind : Symbol]
                 [bindings : (Listof (List Symbol (Listof scope) binding))]
                 [bulk-bindings : (Listof (List (Listof scope) all-from-module))]
                 [multi-owner : (U #f multi-scope)])]
               [#:struct (multi-scope zo) (
                 [name : Natural]
                 [src-name : Any]
                 [scopes : (Listof (List (U #f Integer) scope))])]
               [#:struct (module-binding binding) ([encoded : Any])]
               [#:struct (decoded-module-binding binding) (
                 [path : (U #f Module-Path-Index)]
                 [name : Symbol]
                 [phase : Integer]
                 [nominal-path : (U #f Module-Path-Index)]
                 [nominal-export-name : Symbol]
                 [nominal-phase : (U #f Integer)]
                 [import-phase : (U #f Integer)]
                 [inspector-desc : (U #f Symbol)])]
               [#:struct (local-binding binding) ([name : Symbol])]
               [#:struct (free-id=?-binding binding) ([base : binding]
                 [id : stx-obj]
                 [phase : (U #f Integer)])]
               )
