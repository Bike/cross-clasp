(in-package #:cross-clasp)

;;; Install stuff in an environment to match what we need for build.
;;; To paper over a lot of details: many macros, and then what functions
;;; are defined in C++ and thus available before any Lisp is loaded.

(defmethod maclina.compile:debug-lambda-name ((client client) decls)
  (loop for (declare . decs) in decls
        for p = (assoc 'core:lambda-name decs)
        when p return (second p)))

(defmethod maclina.compile:debug-lambda-list ((client client) decls)
  (loop for (declare . decs) in decls
        for p = (assoc 'core:lambda-list decs)
        when p
          return (values (rest p) t)
        finally (return (values nil nil))))

(defun cmp::register-global-function-def (type name)
  (declare (ignore type))
  (clostrum:note-function m:*client* *build-rte* name)
  (signal 'maclina.compile:resolve-function :name name)
  (values))

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

(defmethod common-macro-definitions::wrap-in-setf-setf-expander
    ((client client) name function environment)
  (declare (ignore environment))
  `(setf (ext:setf-expander ',name) ,function))

(defmethod common-macro-definitions:get-setf-expansion
    ((client client) place &optional environment)
  (let ((env (or environment *build-rte*)))
    (extrinsicl:get-setf-expansion
     client env (macroexpand-hook) place)))

(defun install-packages (&optional (client m:*client*)
                           (environment *build-rte*))
  (macrolet ((defpack (name hostname &rest nicknames)
               `(let ((package (find-package ',hostname)))
                  (setf (clostrum:package-name client environment package) ,name
                        (clostrum:find-package client environment ,name) package
                        ,@(loop for nick in nicknames
                                collect `(clostrum:find-package client environment
                                                                ,nick)
                                collect 'package)))))
    (defpack "COMMON-LISP" #:common-lisp "CL")
    (defpack "CORE" #:cross-clasp.clasp.core "SYS" "SYSTEM" "SI")
    (defpack "GCTOOLS" #:cross-clasp.clasp.gctools)
    (defpack "MP" #:cross-clasp.clasp.mp)
    (defpack "LLVM-SYS" #:cross-clasp.clasp.llvm-sys)
    (defpack "CLOS" #:cross-clasp.clasp.clos)
    (defpack "SEQUENCE" #:cross-clasp.clasp.sequence)
    (defpack "GRAY" #:cross-clasp.clasp.gray)
    (defpack "CLASP-DEBUG" #:cross-clasp.clasp.debug)
    (defpack "EXT" #:cross-clasp.clasp.ext)
    (defpack "KEYWORD" #:keyword)))

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
  '(:clasp :threads :unicode :clos :ansi-cl :common-lisp :bytecode))

(defparameter *noted-functions*
  '(core::generalp core:header-stamp
    core::derivable-stamp core::rack-stamp core::wrapped-stamp
    core:stamps-adjacent-p llvm-sys:tag-tests
    core:gfbytecode-simple-fun/make
    core:single-dispatch-generic-function-p
    ext:specialp core:operator-shadowed-p
    core:function-name core:setf-function-name
    core::valid-function-name-p core::function-block-name
    core:function-docstring (setf core:function-docstring)
    core:function-source-pos ext:function-lambda-list
    core::coerce-to-function core::coerce-fdesignator
    core::fixnump core:ratiop
    core:short-float-p core:single-float-p
    core:double-float-p core:long-float-p
    core:num-op-acosh core:num-op-asinh
    core:num-op-atanh
    core:num-op-acos core:num-op-asin core:num-op-atan
    core::float-to-digits
    core::member1
    core::sequence-start-end
    core:vref (setf core:vref)
    core::read-dense-specialized-array
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
    core::printing-char-p
    core:hash-table-pairs core:hash-equal
    core::coerce-to-package core::package-hash-tables
    ext:lock-package ext:unlock-package ext:package-locked-p
    core:call-with-package-read-lock core:call-with-package-read-write-lock
    core:package-local-nicknames-internal (setf core:package-local-nicknames-internal)
    ext:package-implemented-by-list
    core:package-documentation (setf core:package-documentation)
    core::load-cxx-object core::patch-object core::make-record-patcher
    core::record-field core::rem-record-field
    core:cxx-object-p
    core::make-sharp-equal-wrapper core::sharp-equal-wrapper-p
    core::sharp-equal-wrapper-value (setf core::sharp-equal-wrapper-value)
    core:instancep core:allocate-standard-instance core:class-new-stamp
    clos::classp core::subclassp core:name-of-class
    core:allocate-raw-instance core:allocate-raw-funcallable-instance
    core:class-stamp-for-instances
    core:make-rack core:rack-ref (setf core:rack-ref)
    core::cas-rack core::atomic-rack-read
    core:instance-ref core:instance-stamp core:instance-class
    clos::standard-instance-access
    (setf clos::standard-instance-access)
    clos::funcallable-standard-instance-access
    (setf clos::funcallable-standard-instance-access)
    clos:set-funcallable-instance-function
    core:instance-rack core:instance-sig-set
    core:setf-find-class
    core::get-sysprop (setf core::get-sysprop)
    core::write-object core:write-addr core::write-ugly-object
    mp:make-lock mp:get-lock mp:giveup-lock
    mp:make-shared-mutex mp:suspend-loop
    mp:abort-process mp:enqueue-interrupt
    mp:process-name mp:process-resume mp:all-processes
    mp:process-active-p
    mp:make-condition-variable
    mp:condition-variable-signal mp:condition-variable-wait
    mp:push-default-special-binding
    core::check-pending-interrupts
    core::signal-code-alist
    core::process-lambda-list
    gray::%open-stream-p gray::%input-stream-p gray::%output-stream-p
    gray::%stream-interactive-p gray::%close
    gray::%stream-input-line gray::%stream-input-column gray::%stream-start-line-p
    gray::%stream-line-number gray::%stream-line-column
    gray::%truename gray::%pathname
    gray::%stream-element-type gray::%stream-set-element-type
    gray::%stream-external-format gray::%stream-set-external-format
    gray::%stream-file-descriptor gray::%stream-advance-to-column
    core::stream-output-column
    core::make-string-output-stream-from-string
    core:get-thread-local-write-to-string-output-stream-string
    core:thread-local-write-to-string-output-stream
    core:fmt core::gdb core::mkdir core::file-kind
    core:invoke-internal-debugger
    core:call-with-frame
    core:debugger-frame-up core:debugger-frame-down
    core:debugger-frame-fname core:debugger-frame-source-position
    core:debugger-frame-function-description core:debugger-frame-lang
    core:debugger-frame-closure core:debugger-frame-xep-p
    core:debugger-frame-args-available-p core:debugger-frame-args
    core:debugger-frame-locals
    core:function-description-docstring core:function-description-column
    core:function-description-lineno core:function-description-source-pathname
    core:function-description-lambda-list
    core:make-source-pos-info
    core:source-pos-info-column core:source-pos-info-lineno
    core:source-pos-info-file-handle core:source-pos-info-filepos
    ext:annotate
    core:file-scope core:file-scope-pathname
    core:unix-get-local-time-zone core:unix-daylight-saving-time ext:getenv
    gc:thread-local-unwind-counter gc:bytes-allocated
    core:sl-boundp core:unbound
    core:vaslistp core:list-from-vaslist
    core:interpret core:load-source
    core:startup-type core:is-interactive-lisp core:noinform-p core:noprint-p
    core:rc-file-name core:no-rc-p core:debugger-disabled-p
    core:command-line-load-eval-sequence
    core:quit core::exit))

(defparameter *copied-variables*
  '(;;Eclector expects these to be globally bound
    eclector.reader::*quasiquotation-state*
    eclector.reader::*quasiquotation-depth*
    eclector.reader::*consing-dot-allowed-p*))

(defun install-environment (&optional (client m:*client*)
                              (rte *build-rte*)
                              (ce *build-ce*))
  (declare (ignore ce))
  (extrinsicl:install-cl client rte)
  (extrinsicl.maclina:install-eval client rte)
  (clostrum:make-parameter client rte '*features* (features))
  (clostrum:make-parameter client rte 'core::*current-source-pos-info* nil)
  (loop for vname in '(core::*condition-restarts* core::*restart-clusters*
                       core::*interrupts-enabled* core::*allow-with-interrupts*
                       core:*quasiquote* core::*sharp-equal-final-table*
                       core:*variable-source-infos*
                       ext:*invoke-debugger-hook* ext:*toplevel-hook*
                       ext:*inspector-hook* core::*documentation-pool*
                       core:*initialize-hooks* core:*terminate-hooks*
                       core:*extension-systems*
                       core::*circle-counter* core::*circle-stack*
                       mp:*current-process*
                       core:+type-header-value-map+
                       ext:+process-standard-input+ ext:+process-standard-output+
                       ext:+process-error-output+ ext:+process-terminal-io+)
        do (clostrum:make-variable client rte vname))
  (loop for vname in *copied-variables*
        do (clostrum:make-variable client rte vname (symbol-value vname)))
  (loop for fname in '(core::symbol-constantp (setf core::symbol-constantp)
                       core::*make-special
                       core::find-declarations core:process-declarations
                       core::dm-too-many-arguments core::dm-too-few-arguments
                       cmp::register-global-function-def
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
                       clos::define-method-combination
                       clos::define-simple-method-combination
                       clos::define-complex-method-combination
                       mp:with-lock
                       mp:without-interrupts mp:with-interrupts
                       mp::atomic mp::define-atomic-expander
                       mp::define-simple-atomic-expander mp::cas
                       mp::atomic-update-explicit mp::atomic-update
                       mp::atomic-incf-explicit mp::atomic-incf
                       mp::atomic-decf-explicit mp::atomic-decf
                       mp::atomic-push-explicit mp::atomic-push
                       mp::atomic-pop-explicit mp::atomic-pop
                       mp::atomic-pushnew-explicit mp::atomic-pushnew
                       clos::early-allocate-instance
                       clos::earlier-allocate-instance
                       clos::early-initialize-instance
                       clos::early-make-instance
                       clos::with-mutual-defclass
                       clos::with-effective-method-parameters
                       clos::satiate)
        for m = (macro-function mname)
        do (setf (clostrum:macro-function client rte mname) m))
  (loop for (mname . src) in '((defun . core::%defun)
                               (defmacro . core::%defmacro)
                               (define-compiler-macro . core::%define-compiler-macro)
                               (deftype . core::%deftype)
                               (define-setf-expander . core::%define-setf-expander)
                               (defvar . core::%defvar)
                               (defparameter . core::%defparameter)
                               (defconstant . %defconstant)
                               (defclass . clos::early-defclass)
                               (defgeneric . clos::early-defgeneric)
                               (defmethod . clos::early-defmethod)
                               (defstruct . clos::early-defstruct)
                               (call-method . clos::%call-method)
                               (handler-bind . %handler-bind)
                               (assert . %assert)
                               (check-type . %check-type)
                               (restart-case . %restart-case)
                               (restart-bind . %restart-bind)
                               (with-condition-restarts . %with-condition-restarts))
        for m = (macro-function src)
        do (setf (clostrum:macro-function client rte mname) m))
  (loop for (fname . set) in '((mp::atomic . mp::expand-atomic))
        for f = (fdefinition set)
        do (setf (clostrum:setf-expander client rte fname) f))
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
