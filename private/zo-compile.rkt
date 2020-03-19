#lang racket/base

(provide compile-c-module
         from-c)

(require racket/file
         dynext/file
         dynext/compile
         dynext/link
         ffi/unsafe
         ffi/unsafe/atomic
         (only-in ffi/unsafe/global register-process-global)
         (for-syntax racket/base
                     syntax/parse
                     racket/require-transform
                     racket/file
                     dynext/file
                     dynext/compile
                     dynext/link
                     ffi/unsafe
                     ffi/unsafe/atomic
                     (only-in ffi/unsafe/global register-process-global)))

(define-syntax-rule (define-for-syntax-and-runtime f ...)
  (begin (define f ...)
         (define-for-syntax f ...)))

; Helpers for creating a tag for the global process table
(define-for-syntax-and-runtime zordoz-prefix-string "ZORDOZ-INTERNAL-")
(define-for-syntax-and-runtime (mk-process-global-key key)
  (format "~a~a" zordoz-prefix-string key))

(define-for-syntax-and-runtime done (cast 1 _scheme _pointer))

(define-for-syntax-and-runtime object-target-path
  (build-path "compiled" "native" (system-library-subpath)))

; Try to compile the library. If already linked, don't relink,
; if linked and c file has been modified, error.
; path? -> void?
(define-for-syntax-and-runtime (try-load-library in)
  (define extensionless-source (path-replace-suffix in ""))
  (define out (build-path object-target-path (append-extension-suffix extensionless-source)))
  (with-handlers ([exn:fail?
                   (raise-user-error
                    'zordoz
                    "DrRacket cannot do background check-syntax on modules imported with from-c")])
    (make-temporary-file)) ; Cludgy hack to see if in sandbox
  (call-as-atomic
   (lambda ()
     (if (register-process-global (mk-process-global-key (path->string out)) done)
         (unless (and (file-exists? in)
                      (file-exists? out)
                      ((file-or-directory-modify-seconds in)
                       . < .
                       (file-or-directory-modify-seconds out)))
           (raise-user-error 'zordoz
                             "Cannot reload C based module, please restart Racket (or DrRacket)"))
         (call-as-nonatomic
          (lambda ()
            (with-handlers ([exn:fail?
                             (error 'zordoz
                                    "Could not compile C file, please restart Racket (or DrRacket)")])
                (compile-c-module in))))))))

; Compile C implementation of module. Does not load into program
; path -> void?
(define-for-syntax-and-runtime (compile-c-module c-source)
  (define extensionless-source (path-replace-suffix c-source ""))
  (define object-target
    (build-path object-target-path (append-object-suffix extensionless-source)))
  (define shared-object-target
    (build-path object-target-path (append-extension-suffix extensionless-source)))
  (make-directory* object-target-path)
  (compile-extension #t c-source object-target '())
  (link-extension #t (list object-target) shared-object-target))

; Compile C module, load into module. Fail if C file changed and module has
; already been loaded into the VM.
(define-syntax from-c
  (make-require-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ c-source:str)
        (define f (syntax-e #'c-source))
        (try-load-library f)
        (expand-import (datum->syntax stx (path->string (path-replace-suffix f ""))))]))))
