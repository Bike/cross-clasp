(in-package #:cross-clasp)

(defparameter *files*
  '("lsp/foundation.lisp"
    "clos/hierarchy.lisp"
    "clos/method-function.lisp"
    "clos/method-combination.lisp"
    "clos/standard-method-combinations.lisp"
    "clos/applicable-methods.lisp"
    "clos/effective-method.lisp"
    "clos/outcome.lisp"
    "clos/satiate.lisp"
    ;;"lsp/export.lisp"
    "lsp/assert.lisp"
    "lsp/arraylib.lisp"
    "lsp/predlib.lisp"
    "lsp/listlib.lisp"
    "lsp/mislib.lisp"
    "lsp/defstruct.lisp"
    "lsp/seqmacros.lisp"
    "lsp/seq.lisp"
    "lsp/seqlib.lisp"
    "lsp/iolib.lisp"
    "lsp/trace.lisp"
    "lsp/debug.lisp"
    "lsp/numlib.lisp"
    "lsp/assorted.lisp"
    "lsp/packlib.lisp"
    #+(or)"lsp/top.lisp"
    ;; macros
    "lsp/do.lisp"
    #+(or)"lsp/defpackage.lisp"
    #+(or)"lsp/loop2.lisp"))

(defun default-output-directory ()
  (asdf:system-relative-pathname :cross-clasp "build/"))

(defun build (&key (output (default-output-directory))
                   ((:verbose *compile-verbose*) *compile-verbose*)
                   ((:print *compile-print*) *compile-print*))
  (ensure-directories-exist output)
  (initialize)
  (maclina.vm-cross:initialize-vm 20000)
  (apply #'maclina.compile-file:link-fasls
         (make-pathname :name "clasp" :type "fasl" :defaults output)
         (maclina.compile:with-compilation-unit ()
           (loop with kernel = (asdf:system-relative-pathname
                                :cross-clasp "kernel/")
                 for file in *files*
                 for absolute = (merge-pathnames file kernel)
                 ;; FIXME: maclina compile-file-pathname should handle this
                 for out = (make-pathname :type "fasl" :defaults output
                                          :name (pathname-name absolute))
                 collect (cross-compile-file absolute :output-file out)))))
