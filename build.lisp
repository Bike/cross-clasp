(in-package #:cross-clasp)

(defparameter *files*
  '(;; Enough CLOS to call generic functions
    "clos/method-combination-environment.lisp"
    "clos/standard-method-combinations.lisp"
    "clos/hierarchy.lisp"
    "clos/method-function.lisp"
    "clos/eql-specializer.lisp"
    "clos/applicable-methods.lisp"
    "clos/effective-method.lisp"
    "clos/outcome.lisp"
    "clos/slot-value.lisp"
    "clos/effective-accessor.lisp"
    "clos/interpreted-discriminator.lisp"
    "clos/miss.lisp"
    "clos/check-initargs.lisp"
    "clos/make.lisp"
    "clos/print.lisp"
    "clos/misc.lisp"
    "clos/satiate.lisp"
    ;; Library
    "lsp/debug.lisp"
    "clos/conditions.lisp"
    "lsp/assert.lisp"
    "clos/package.lisp"
    "lsp/arraylib.lisp"
    "lsp/numlib.lisp"
    "lsp/predlib.lisp"
    "lsp/cdr-5.lisp"
    "lsp/module.lisp"
    "clos/streams.lisp"
    "lsp/pprint.lisp"
    "lsp/listlib.lisp"
    "lsp/mislib.lisp"
    "lsp/seqmacros.lisp"
    "lsp/seq.lisp"
    "lsp/seqlib.lisp"
    "lsp/iolib.lisp"
    "lsp/trace.lisp"
    "lsp/assorted.lisp"
    "lsp/packlib.lisp"
    "clos/sequences.lisp"
    "lsp/helpfile.lisp"
    "lsp/describe.lisp"
    "lsp/source-location.lisp"
    "clos/inspect.lisp"
    ;; CLOS part two: defining new generics, classes, etc
    "clos/change.lisp"
    "clos/dependent.lisp"
    "clos/class.lisp"
    "clos/cpl.lisp"
    "clos/generic.lisp"
    ;; macros
    "lsp/source-transformations.lisp"
    "lsp/cmuutil.lisp"
    "lsp/shiftf-rotatef.lisp"
    "lsp/setf.lisp"
    "lsp/do.lisp"
    "lsp/defpackage.lisp"
    "lsp/format.lisp"
    "lsp/format-pprint.lisp"
    "lsp/defmacro.lisp"
    "lsp/evalmacros.lisp"
    "lsp/defstruct.lisp"
    "lsp/loop2.lisp"
    "lsp/sharpmacros.lisp"
    "lsp/mp.lisp"
    "lsp/atomics.lisp"
    "clos/define-method-combination.lisp"
    ;; toplevel
    "lsp/top.lisp"
    "lsp/top-hook.lisp"))

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
