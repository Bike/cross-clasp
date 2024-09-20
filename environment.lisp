(in-package #:cross-clasp)

(defvar *build-rte*)
(defvar *build-ce*)

(defclass client (maclina.vm-cross:client) ())

(defclass reader-client (maclina.compile-file::reader-client) ())

(defvar *reader-client* (make-instance 'reader-client))

(defmethod maclina.compile-file::find-package ((client reader-client) package-name)
  (or (clostrum:find-package m:*client* *build-rte* package-name)
    (warn "Unknown package ~s; substituting CORE" package-name)
    (cl:find-package "CROSS-CLASP.CLASP.CORE")))

(defmethod maclina.compile-file::package-name ((client client) env package)
  (or (clostrum:package-name client env package)
    (warn "Unknown package ~s; substituting CORE"
          (cl:package-name package))
    "CORE"))

(defun cross-compile-file (input-file &rest keys)
  (let ((maclina.compile-file::*primitive* t))
    (apply #'maclina.compile-file:compile-file input-file
           :environment *build-rte*
           :reader-client *reader-client*
           keys)))

(defmethod clostrum-sys:variable-cell :around ((client client)
                                               environment symbol)
  (if (keywordp symbol)
      (let ((cell (clostrum-sys:ensure-variable-cell client environment symbol)))
        (setf (clostrum-sys:variable-cell-value client cell) symbol)
        cell)
      (call-next-method)))

(defmethod clostrum-sys:variable-status :around ((client client)
                                                 environment symbol)
  (if (keywordp symbol)
      :constant
      (call-next-method)))

(defmethod common-macro-definitions:defun-compile-time-action
    ((client client) name lambda-list env)
  (declare (ignore lambda-list))
  (clostrum:note-function client (trucler:global-environment client env) name)
  (signal 'maclina.compile:resolve-function :name name)
  nil)

(defun proclaim (proclamation)
  ;; FIXME: record types, etc
  (when (consp proclamation)
    (case (car proclamation)
      ((special)
       (loop for s in (rest proclamation)
             do (clostrum:make-variable m:*client* *build-rte* s)))))
  (values))

(defun core::*make-special (var)
  (clostrum:make-variable m:*client* *build-rte* var))

(defmethod common-macro-definitions:proclaim
    ((client client) declspec env)
  (declare (ignore env))
  (if (and (consp declspec) (eq (car declspec) 'special))
      ;; special case this so clasp can load it early.
      `(progn
         ,@(loop for s in (rest declspec)
                 collect `(core::*make-special ',s)))
      nil #+(or)`(cl:proclaim ',declspec)))

(defun fdesignator (designator)
  (etypecase designator
    (function designator)
    (symbol (clostrum:fdefinition m:*client* *build-rte* designator))))

(defun macroexpand-hook ()
  (fdesignator
   (maclina.machine:symbol-value m:*client* *build-rte*
                                 '*macroexpand-hook*)))

(defun build-macroexpand-1 (form &optional env)
  (extrinsicl:macroexpand-1 m:*client* (or env *build-rte*)
                            (macroexpand-hook) form))
(defun build-macroexpand (form &optional env)
  (extrinsicl:macroexpand m:*client* (or env *build-rte*)
                          (macroexpand-hook) form))

(defun find-compiler-class (name)
  (clostrum:find-class m:*client* *build-rte* name t))

(defun gf-info (name)
  ;; stuffed into inline data for now
  (let ((dat (clostrum:operator-inline-data m:*client* *build-rte*
                                            name)))
    (etypecase dat
      ((or null clos::compiler-generic) dat)
      (t (error "Not a generic: ~s" name)))))

(defun clos::note-generic (name compiler-generic)
  (clostrum:note-function m:*client* *build-rte* name)
  (signal 'maclina.compile:resolve-function :name name)
  (setf (clostrum:operator-inline-data m:*client* *build-rte* name)
        compiler-generic)
  (values))

;;; make a package in the build environment.
;;; this basically entails resolving all names with respect to that
;;; environment, and then making a host package with CROSS-CLASP.CLASP.
;;; prepended to the name.
(defun %make-package (package-name &key nicknames use)
  (let* ((name (string package-name))
         (hname (concatenate 'string "CROSS-CLASP.CLASP." name))
         (use
           (loop for u in use
                 for s = (string u)
                 collect (or (clostrum:find-package
                              m:*client* *build-rte* s)
                           (error "Tried to use undefined package ~s" s))))
         (_ (when (find-package hname)
              (delete-package hname))) ; fuck it
         (package (cl:make-package hname :use use)))
    (declare (ignore _))
    (setf (clostrum:package-name m:*client* *build-rte* package) name
          (clostrum:find-package m:*client* *build-rte* name) package)
    (loop for nick in nicknames
          for snick = (string nick)
          do (setf (clostrum:find-package m:*client* *build-rte* snick)
                   package))
    package))

;;; We ignore package locks for now
(defun ext:add-implementation-package (implementors &optional package)
  (declare (ignore implementors package)))

(defmethod common-macro-definitions:get-setf-expansion
    ((client client) place &optional environment)
  (let ((env (or environment *build-rte*)))
    (extrinsicl:get-setf-expansion
     client env (macroexpand-hook) place)))

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
    (setf (clostrum:package-name client environment cl) "COMMON-LISP"
          (clostrum:find-package client environment "COMMON-LISP") cl
          (clostrum:find-package client environment "CL") cl)
    (setf (clostrum:package-name client environment core) "CORE"
          (clostrum:find-package client environment "CORE") core
          (clostrum:find-package client environment "SYS") core
          (clostrum:find-package client environment "SYSTEM") core
          (clostrum:find-package client environment "SI") core)
    (setf (clostrum:package-name client environment gctools) "GCTOOLS"
          (clostrum:find-package client environment "GCTOOLS") gctools)
    (setf (clostrum:package-name client environment mp) "MP"
          (clostrum:find-package client environment "MP") mp)
    (setf (clostrum:package-name client environment clos) "CLOS"
          (clostrum:find-package client environment "CLOS") clos)
    (setf (clostrum:package-name client environment seq) "SEQUENCE"
          (clostrum:find-package client environment "SEQUENCE") seq)
    (setf (clostrum:package-name client environment ext) "EXT"
          (clostrum:find-package client environment "EXT") ext)
    (setf (clostrum:package-name client environment kw) "KEYWORD"
          (clostrum:find-package client environment "KEYWORD") kw)))

;;; FIXME: defconstant should really be in common macros.
(defun core::symbol-constantp (name)
  (clostrum:constantp m:*client* *build-rte* name))
(defun (setf core::symbol-constantp) (value name)
  (when value
    (clostrum:make-constant m:*client* *build-rte* name
                            (m:symbol-value m:*client* *build-rte* name)))
  value)

(defmacro %defconstant (name value &optional doc)
  (declare (ignore doc))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set ',name ,value)
     (funcall #'(setf core::symbol-constantp) t ',name)
     #+(or)
     (core::make-constant ',name ,value)
     ',name))

(defun core::make-simple-vector-t (dimension initial-element iep)
  (if iep
      (make-array dimension :initial-element initial-element)
      (make-array dimension)))

(defun features ()
  '(:clasp :threads :unicode :clos :ansi-cl :common-lisp))

(defparameter *noted-functions*
  '(core::generalp core:header-stamp
    ext:specialp
    core:function-name
    core::coerce-to-function core::coerce-fdesignator
    core::fixnump
    core:num-op-acosh core:num-op-asinh
    core:num-op-atanh
    core:num-op-acos core:num-op-asin core:num-op-atan
    core::member1
    core::sequence-start-end
    core:vref (setf core:vref)
    core::copy-subarray core:replace-array
    core:data-vector-p
    core:sbv-bit-and core:sbv-bit-ior
    core:sbv-bit-nor core:sbv-bit-nand
    core:sbv-bit-xor core:sbv-bit-eqv
    core:sbv-bit-andc1 core:sbv-bit-andc2
    core:sbv-bit-orc1 core:sbv-bit-orc2
    core:sbv-bit-not
    core::%displacement core::%displaced-index-offset
    core::make-vector core::make-mdarray
    core::fill-array-with-elt
    core::base-string-p core::base-string-concatenate
    core::search-string
    core:hash-table-pairs core:hash-eql
    core::coerce-to-package core::package-hash-tables
    core:allocate-standard-instance core:class-new-stamp
    clos::classp core::subclassp core:name-of-class
    core:allocate-raw-instance core:class-stamp-for-instances
    core:make-rack core:rack-ref (setf core:rack-ref)
    clos::standard-instance-access
    (setf clos::standard-instance-access)
    clos::funcallable-standard-instance-access
    (setf clos::funcallable-standard-instance-access)
    core:instance-rack core:instance-sig-set
    core:setf-find-class
    core::get-sysprop (setf core::get-sysprop)
    core::write-object core:write-addr
    mp:make-shared-mutex
    core:get-thread-local-write-to-string-output-stream-string
    core:thread-local-write-to-string-output-stream
    core:fmt core::gdb core::mkdir core::file-kind
    core:invoke-internal-debugger
    core:debugger-frame-up core:debugger-frame-down
    core:debugger-frame-fname core:debugger-frame-source-position
    core:debugger-frame-function-description core:debugger-frame-lang
    core:debugger-frame-closure core:debugger-frame-xep-p
    core:debugger-frame-args-available-p core:debugger-frame-args
    core:debugger-frame-locals
    core:unix-get-local-time-zone core:unix-daylight-saving-time
    gc:thread-local-unwind-counter gc:bytes-allocated
    core:unbound))

(defun install-environment (&optional (client m:*client*)
                              (rte *build-rte*)
                              (ce *build-ce*))
  (declare (ignore ce))
  (extrinsicl:install-cl client rte)
  (extrinsicl.maclina:install-eval client rte)
  (setf (m:symbol-value client rte '*features*) (features))
  (loop for vname in '(*features*
                       core::*condition-restarts* core::*restart-clusters*)
        do (clostrum:make-variable client rte vname))
  (loop for fname in '(core::symbol-constantp (setf core::symbol-constantp)
                       core::*make-special
                       core::find-declarations core:process-declarations
                       core::dm-too-many-arguments core::dm-too-few-arguments
                       ext:parse-macro
                       ;; used in CLOS, not expected to actually exist
                       ;; in the target
                       clos::note-generic clos::note-method
                       ;; FIXME: Used in common-macros defmacro expansions
                       ecclesia:list-structure
                       ext:parse-compiler-macro ext:parse-deftype
                       ext:parse-define-setf-expander
                       ext:add-implementation-package
                       core::make-simple-vector-t)
        for f = (fdefinition fname)
        do (setf (clostrum:fdefinition client rte fname) f))
  (loop for (fname . src) in '((cl:proclaim . proclaim)
                               (cl:make-package . %make-package))
        for f = (fdefinition src)
        do (setf (clostrum:fdefinition client rte fname) f))
  (loop for fname in *noted-functions*
        do (clostrum:note-function client rte fname))
  (loop for mname in '(eclector.reader:quasiquote
                       ext:with-current-source-form
                       core::with-clean-symbols core::with-unique-names
                       core::once-only
                       core::defconstant-eqx core::defconstant-equal
                       core::while core::until
                       clos::with-early-accessors
                       clos::early-allocate-instance
                       clos::early-initialize-instance
                       clos::early-make-instance
                       clos::with-mutual-defclass
                       clos::with-effective-method-parameters
                       clos::satiate)
        for m = (macro-function mname)
        do (setf (clostrum:macro-function client rte mname) m))
  (loop for (mname . src) in '((defconstant . %defconstant)
                               (defclass . clos::early-defclass)
                               (defgeneric . clos::early-defgeneric)
                               (defmethod . clos::early-defmethod)
                               (call-method . clos::%call-method)
                               (handler-bind . %handler-bind)
                               (restart-case . %restart-case)
                               (restart-bind . %restart-bind)
                               (with-condition-restarts . %with-condition-restarts))
        for m = (macro-function src)
        do (setf (clostrum:macro-function client rte mname) m))
  ;; Extrinsicl copies over a bunch of classes, but we actually need
  ;; to use our own instead.
  (loop for s being the external-symbols of "CL"
        do (setf (clostrum:find-class client rte s) nil))
  (values))

(defun initialize ()
  (setf m:*client* (make-instance 'client)
        *build-rte* (make-instance 'clostrum-basic:run-time-environment)
        *build-ce* (make-instance 'clostrum-basic:compilation-environment
                     :parent *build-rte*))
  (install-environment)
  (install-packages)
  (values))
