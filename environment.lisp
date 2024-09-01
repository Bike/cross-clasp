(in-package #:cross-clasp)

(defvar *build-rte*)
(defvar *build-ce*)

(defclass reader-client (maclina.compile-file::reader-client) ())

(defvar *reader-client* (make-instance 'reader-client))

(defmethod maclina.compile-file::find-package ((client reader-client) package-name)
  (clostrum:find-package m:*client* *build-rte* package-name))

(defun cross-compile-file (input-file &rest keys)
  (apply #'maclina.compile-file:compile-file input-file
         :environment *build-rte*
         :reader-client *reader-client*
         keys))

(defmethod clostrum-sys:variable-cell :around ((client maclina.vm-cross:client)
                                               environment symbol)
  (if (keywordp symbol)
      (let ((cell (clostrum-sys:ensure-variable-cell client environment symbol)))
        (setf (clostrum-sys:variable-cell-value client cell) symbol)
        cell)
      (call-next-method)))

(defmethod clostrum-sys:variable-status :around ((client maclina.vm-cross:client)
                                                 environment symbol)
  (if (keywordp symbol)
      :constant
      (call-next-method)))

(defmethod common-macro-definitions:defun-compile-time-action
    ((client maclina.vm-cross:client) name lambda-list env)
  (declare (ignore lambda-list))
  (clostrum:note-function client (trucler:global-environment client env) name)
  nil)

(defmethod common-macro-definitions:proclaim
    ((client maclina.vm-cross:client) declspec env)
  (declare (ignore declspec env))) ; FIXME

(defmethod common-macro-definitions:get-setf-expansion
    ((client maclina.vm-cross:client) place &optional environment)
  (let ((env (or environment *build-rte*)))
    (extrinsicl:get-setf-expansion
     client env
     #'funcall ;(maclina.machine:symbol-value client env '*macroexpand-hook*)
     place)))

(defun install-packages (&optional (client m:*client*)
                           (environment *build-rte*))
  (let ((cl (find-package '#:common-lisp))
        (core (find-package '#:cross-clasp.clasp.core))
        (gctools (find-package '#:cross-clasp.clasp.gctools))
        (mp (find-package '#:cross-clasp.clasp.mp))
        (clos (find-package '#:cross-clasp.clasp.clos))
        (seq (find-package '#:cross-clasp.clasp.sequence))
        (ext (find-package '#:cross-clasp.clasp.ext))
        (kw (find-package "KEYWORD")))
    (setf (clostrum:find-package client environment "COMMON-LISP") cl
          (clostrum:find-package client environment "CL") cl)
    (setf ;(clostrum:package-name client environment core) "CORE"
          (clostrum:find-package client environment "CORE") core
          (clostrum:find-package client environment "SYS") core
          (clostrum:find-package client environment "SYSTEM") core
          (clostrum:find-package client environment "SI") core)
    (setf ;(clostrum:package-name client environment gctools) "GCTOOLS"
          (clostrum:find-package client environment "GCTOOLS") gctools)
    (setf ;(clostrum:package-name client environment mp) "MP"
          (clostrum:find-package client environment "MP") mp)
    (setf ;(clostrum:package-name client environment clos) "CLOS"
          (clostrum:find-package client environment "CLOS") clos)
    (setf ;(clostrum:package-name client environment seq) "SEQUENCE"
          (clostrum:find-package client environment "SEQUENCE") seq)
    (setf ;(clostrum:package-name client environment ext) "EXT"
          (clostrum:find-package client environment "EXT") ext)
    (setf ;(clostrum:package-name client environment kw) "KEYWORD"
          (clostrum:find-package client environment "KEYWORD") kw)))

;;; FIXME: defconstant should really be in common macros.
(defun core::make-constant (name value)
  (clostrum:make-constant m:*client* *build-rte* name value))

(defmacro %defconstant (name value &optional doc)
  (declare (ignore doc))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (core::make-constant ',name ,value)))

(defun install-environment (&optional (client m:*client*)
                              (rte *build-rte*)
                              (ce *build-ce*))
  (declare (ignore ce))
  (extrinsicl:install-cl client rte)
  (extrinsicl.maclina:install-eval client rte)
  (loop for fname in '(core::make-constant
                       core::find-declarations core:process-declarations
                       ext:parse-macro
                       ;; FIXME: Used in common-macros defmacro expansions
                       ecclesia:list-structure
                       ext:parse-compiler-macro ext:parse-deftype
                       ext:parse-define-setf-expander)
        for f = (fdefinition fname)
        do (setf (clostrum:fdefinition client rte fname) f))
  (loop for fname in '(core:invoke-internal-debugger
                       core:name-of-class core:fmt core::gdb
                       core:hash-table-pairs)
        do (clostrum:note-function client rte fname))
  (loop for mname in '(eclector.reader:quasiquote
                       core::with-unique-names core::once-only
                       core::defconstant-eqx core::defconstant-equal)
        for m = (macro-function mname)
        do (setf (clostrum:macro-function client rte mname) m))
  (loop for (mname . src) in '((defconstant . %defconstant))
        for m = (macro-function src)
        do (setf (clostrum:macro-function client rte mname) m))
  (values))

(defun initialize ()
  (setf m:*client* (make-instance 'maclina.vm-cross:client)
        *build-rte* (make-instance 'clostrum-basic:run-time-environment)
        *build-ce* (make-instance 'clostrum-basic:compilation-environment
                     :parent *build-rte*))
  (install-environment)
  (install-packages)
  (values))
