;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;
;;; conditions.lisp
;;;
;;; Originally written by Kent M. Pitman of Symbolics, Inc. and
;;; distributed without any copyright.
;;; This is Version 18.
;;;
;;; KMP's disclaimer:
;;;
;;; This is a sample implementation. It is not in any way intended as the
;;; definition of any aspect of the condition system. It is simply an existence
;;; proof that the condition system can be implemented.
;;;

#+(or)
(eval-when (:execute)
  (setq core:*echo-repl-read* t))

(in-package "SYSTEM")

;;; ----------------------------------------------------------------------


;;; Restarts

;;; Current restarts available. A list of lists of restarts.
;;; Each RESTART-BIND adds another list of restarts to this list.
(defparameter *restart-clusters* ())
;;; Current condition-restarts associations, made by WITH-CONDITION-RESTARTS.
;;; A list of (condition . restarts) lists.
(defparameter *condition-restarts* ())

(defun compute-restarts (&optional condition)
  (let* ((assoc-restart ())
	 (other ())
	 (output ()))
    (when condition
      (dolist (i *condition-restarts*)
	(if (eq (first i) condition)
	    (setq assoc-restart (append (rest i) assoc-restart))
	    (setq other (append (rest i) other)))))
    (dolist (restart-cluster *restart-clusters*)
      (dolist (restart restart-cluster)
	(when (and (or (not condition)
		       (member restart assoc-restart)
		       (not (member restart other)))
		   (funcall (ext:restart-test-function restart) condition))
	  (push restart output))))
    (nreverse output)))

;;; Not used here, but can be useful to debuggers
(defun ext:restart-associated-conditions (restart)
  (let ((conditions ()))
    (dolist (i *condition-restarts* conditions)
      (when (member restart (rest i))
        (pushnew (first i) conditions :test #'eq)))))

(defclass restart ()
  ((%name :initarg :name :reader restart-name)
   (%function :initarg :function :reader ext:restart-function
              :type function)
   (%report-function :initarg :report-function
                     :reader ext:restart-report-function
                     :type (function (stream)))
   (%interactive-function :initarg :interactive-function
                          :reader ext:restart-interactive-function
                          :initform (constantly ())
                          :type (function () list))
   (%test-function :initarg :test-function
                   :reader ext:restart-test-function
                   :initform (constantly t)
                   :type (function (condition) t))))

;;; This is necessary for bootstrapping reasons: assert.lisp, at least,
;;; uses restart-bind before CLOS and static-gfs are up.
;;; FIXME: Probably not true in the cross build
(defun make-restart (&key name function
                       (report-function
                        (lambda (stream) (prin1 name stream)))
                       (interactive-function (constantly ()))
                       (test-function (constantly t)))
  (clos::early-make-instance restart
    :name name :function function
    :report-function report-function
    :interactive-function interactive-function
    :test-function test-function))

(defun restart-p (object) (typep object 'restart))

(defmethod print-object ((restart restart) stream)
  (if *print-escape*
      (print-unreadable-object (restart stream :type t :identity t)
        (write (restart-name restart) :stream stream))
      (funcall (ext:restart-report-function restart) stream))
  restart)

(defmacro restart-bind (bindings &body forms)
  `(let ((*restart-clusters*
	  (cons (list ,@(mapcar #'(lambda (binding)
				    `(make-restart
                                       :NAME     ',(car binding)
                                       :FUNCTION ,(cadr binding)
                                       ,@(cddr binding)))
				bindings))
		*restart-clusters*)))
     ,@forms))

;;; Return true iff either the restart is not associated with any condition, or
;;; is associated with the given condition; see CLHS find-restart.
(defun restart-associated-p (restart condition)
  (let ((associated-at-all-p nil))
    (dolist (i *condition-restarts* (not associated-at-all-p))
      (let ((rcondition (first i)))
        (when (member restart (rest i) :test #'eq)
          (if (eq condition rcondition)
              (return t)
              (setf associated-at-all-p t)))))))

(defun find-restart-by-name (name condition)
  (dolist (restart-cluster *restart-clusters* nil)
    (dolist (restart restart-cluster)
      (when (and (eq (restart-name restart) name)
                 (or (not condition)
                     (restart-associated-p restart condition))
                 (funcall (ext:restart-test-function restart) condition))
        (return-from find-restart-by-name restart)))))

;;; Checks if a restart is "valid". CLHS says INVOKE-RESTART must signal an
;;; error if it isn't. "valid" means its extent hasn't exited,
;;; I think, though I can't find this spelled out anywhere.
(defun valid-restart-p (restart)
  (dolist (cluster *restart-clusters* nil)
    (when (find restart cluster :test #'eq)
      (return t))))

;;; Given a restart, FIND-RESTART isn't an identity - it's only returned if that
;;; restart is visible in the dynamic environment with respect to the condition.
(defun find-restart-by-identity (restart condition)
  (and (valid-restart-p restart)
       (or (null condition) (restart-associated-p restart condition))
       (funcall (ext:restart-test-function restart) condition)
       restart))

(defun find-restart (identifier &optional condition)
  (etypecase identifier
    (symbol (find-restart-by-name identifier condition))
    (restart (find-restart-by-identity identifier condition))))

;;; We don't just call FIND-RESTART because it has slightly
;;; different behavior when called with a restart argument.
;;; FIND-RESTART given a restart only returns it if that restart
;;; is active _relative to the given condition_. That means you
;;; can pass it a restart and get NIL back (resulting in an
;;; error here).
;;; For restart designators like invoke-restart takes, however,
;;; a restart just designates itself.
;;; This comes up with e.g. (invoke-restart (find-restart x c) ...)
;;; If x is only active relative to C, and invoke-restart used
;;; FIND-RESTART, it would come up empty since x is not active
;;; relative to no-condition.
;;; Strictly speaking we could still test whether the restart is
;;; active, but I don't think this is required, and it seems
;;; rare enough that I don't mind not checking.
(defun coerce-restart-designator (designator &optional condition)
  (etypecase designator
    (restart (if (valid-restart-p designator)
                 designator
                 (error 'invalid-restart :restart designator)))
    (symbol (or (find-restart-by-name designator condition)
                (signal-simple-error 'simple-control-error nil
                                     "Restart ~S is not active."
                                     (list designator))))))

(defun invoke-restart (restart &rest values)
  (let ((real-restart (coerce-restart-designator restart)))
    (apply (ext:restart-function real-restart) values)))

(defun invoke-restart-interactively (restart)
  (let ((real-restart (coerce-restart-designator restart)))
    (apply (ext:restart-function real-restart)
           (funcall
            (ext:restart-interactive-function real-restart)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun munge-with-condition-restarts-form (original-form env)
  (ext:with-current-source-form (original-form)
    (let ((form (macroexpand original-form env)))
      (if (consp form)
          (let* ((name (first form))
                 (condition-form
                   (case name
                     ((signal)
                      `(coerce-to-condition ,(second form)
                                            (list ,@(cddr form))
                                            'simple-condition 'signal))
                     ((warn)
                      `(coerce-to-condition ,(second form)
                                            (list ,@(cddr form))
                                            'simple-warning 'warn))
                     ((error)
                      `(coerce-to-condition ,(second form)
                                            (list ,@(cddr form))
                                            'simple-error 'error))
                     ((cerror)
                      `(coerce-to-condition ,(third form)
                                            (list ,@(cdddr form))
                                            'simple-error 'cerror)))))
            (if condition-form
                (let ((condition-var (gensym "CONDITION")))
                  `(let ((,condition-var ,condition-form))
                     (with-condition-restarts ,condition-var
                         (first *restart-clusters*)
                       ,(if (eq name 'cerror)
                            `(cerror ,(second form) ,condition-var)
                            `(,name ,condition-var)))))
                original-form))
          original-form))))
) ; eval-when

(defmacro restart-case (expression &body clauses &environment env)
  (flet ((transform-keywords (&key report interactive test)
	   (let ((keywords '()))
	     (when test
	       (setq keywords (list :TEST-FUNCTION `#',test)))				    
	     (when interactive
	       (setq keywords (list* :INTERACTIVE-FUNCTION
                                     `#',interactive
                                     keywords)))
	     (when report
	       (setq keywords (list* :REPORT-FUNCTION
				     (if (stringp report)
					 `#'(lambda (stream)
					      (write-string ,report stream))
					 `#',report)
				     keywords)))
	     keywords)))
    (let* ((block-tag (gensym))
           (temp-var  (gensym))
           (data (mapcar #'(lambda (clause)
                             (ext:with-current-source-form (clause)
                               (let (keywords (forms (cddr clause)))
                                 (do ()
                                     ((null forms))
                                   (if (keywordp (car forms))
                                       (setq keywords (list* (car forms)
                                                             (cadr forms)
                                                             keywords)
                                             forms (cddr forms))
                                       (return)))
                                 (list (car clause) 		;Name=0
                                       (gensym) 			;Tag=1
                                       (apply #'transform-keywords ;Keywords=2
                                              keywords)
                                       (cadr clause)		;BVL=3
                                       forms)))) 			;Body=4
                         clauses))
           (expression (munge-with-condition-restarts-form expression env)))
      `(block ,block-tag
	 (let ((,temp-var nil))
	   (tagbody
              (return-from ,block-tag
                (restart-bind
                    ,(mapcar #'(lambda (datum)
                                 (let*((name (nth 0 datum))
                                       (tag  (nth 1 datum))
                                       (keys (nth 2 datum)))
                                   `(,name #'(lambda (&rest temp)
                                               (setq ,temp-var temp)
                                               (go ,tag))
                                           ,@keys)))
                      data)
                  ,expression))
	     ,@(mapcan #'(lambda (datum)
			   (let*((tag  (nth 1 datum))
				 (bvl  (nth 3 datum))
				 (body (nth 4 datum)))
			     (list tag
				   `(return-from ,block-tag
				      (apply #'(lambda ,bvl ,@body)
					     ,temp-var)))))
		       data)))))))

(defmacro with-simple-restart ((restart-name format-control
					     &rest format-arguments)
			       &body forms)
  `(restart-case (progn ,@forms)
     (,restart-name ()
        :REPORT (lambda (stream)
		  (format stream ,format-control ,@format-arguments))
      (values nil t))))

(defmacro with-condition-restarts (condition restarts &body forms)
  `(let ((*condition-restarts* (cons (cons ,condition ,restarts)
				     *condition-restarts*)))
    ,@forms))


;;; ----------------------------------------------------------------------
;;; Condition Data Type

(defun default-condition-reporter (condition stream)
  (format stream "Condition of type ~a was signaled." (type-of condition)))

(defclass condition ()
  ((reporter :allocation :class
             :reader condition-reporter
             :type function
             :initform #'default-condition-reporter)))

(defmethod print-object ((c condition) stream)
  (if *print-escape*
      (call-next-method)
      (funcall (condition-reporter c) c stream)))

(defmacro define-condition (name parent-list slot-specs &rest options)
  ;; CAUTION: ANSI states the equivalence between :REPORT and a method.
  ;; This does not mean CALL-NEXT-METHOD should be available, as it also
  ;; says the function is evaluated in the CURRENT lexical environment.
  (let* ((class-options nil))
    (dolist (option options)
      (ext:with-current-source-form (option)
        (case (car option)
          ((:DEFAULT-INITARGS :DOCUMENTATION)
           (push option class-options))
          (:REPORT
           (let ((reporter (cadr option)))
             (push `(reporter :allocation :class
                              :initform #',(if (stringp reporter)
                                               `(lambda (condition stream)
                                                  (declare (ignore condition))
                                                  (write-string ,reporter stream))
                                               reporter))
                   slot-specs)))
          (otherwise (cerror "Ignore this DEFINE-CONDITION option."
                             "Invalid DEFINE-CONDITION option: ~S" option)))))
    `(PROGN
      (DEFCLASS ,name ,(or parent-list '(CONDITION)) ,slot-specs ,@class-options)
      ',NAME)))

(defun find-subclasses-of-type (type class)
  ;; Find all subclasses of CLASS that are subtypes of the given TYPE.
  (if (subtypep class type)
      (list class)
      (loop for c in (clos::class-direct-subclasses class)
	    nconc (find-subclasses-of-type type c))))

(defun make-condition (type &rest slot-initializations)
  (let ((class (or (and (symbolp type) (find-class type nil))
		   (first (last (sort (find-subclasses-of-type type (find-class 'condition))
				      #'si::subclassp))))))
    (unless class
      (error 'SIMPLE-TYPE-ERROR
	     :DATUM type
	     :EXPECTED-TYPE 'CONDITION
	     :FORMAT-CONTROL "Not a condition type: ~S"
	     :FORMAT-ARGUMENTS (list type)))
    (apply #'make-instance class slot-initializations)))



(defparameter *handler-clusters* nil)

(defmacro handler-bind (bindings &body forms)
  `(let ((*handler-clusters*
           (cons (list ,@(mapcar #'(lambda (binding)
                                     (ext:with-current-source-form (binding)
                                       (unless (and (listp binding)
                                                    (= (length binding) 2))
                                         (simple-program-error
                                          "Ill-formed handler binding ~s."
                                          binding))
                                       `(cons (lambda (condition)
                                                (typep condition ',(car binding)))
                                              ,(cadr binding))))
                                 bindings))
                 *handler-clusters*)))
     ,@forms))

(defun %signal (condition)
  ;; We pop as we go, rather than just iterating, so that if a condition
  ;; is signaled by the type test or by the handler function, it doesn't
  ;; find itself or lower handlers as active.
  (let ((*handler-clusters* *handler-clusters*))
    (when (typep condition *break-on-signals*)
      (break "~a~%Break entered because of *BREAK-ON-SIGNALS*." condition))
    (loop (unless *handler-clusters* (return))
          (let ((cluster (pop *handler-clusters*)))
            (dolist (handler cluster)
              (when (funcall (car handler) condition)
                (funcall (cdr handler) condition))))))
  nil)

(defun signal (datum &rest arguments)
  (%signal (coerce-to-condition datum arguments 'simple-condition 'signal)))



(defmacro handler-case (form &rest cases)
  (let ((no-error-clause (assoc ':NO-ERROR cases)))
    (if no-error-clause
	(let* ((normal-return (make-symbol "NORMAL-RETURN"))
	       (error-return  (make-symbol "ERROR-RETURN")))
	  `(block ,error-return
             (multiple-value-call #'(lambda ,@(cdr no-error-clause))
               (block ,normal-return
                 (return-from ,error-return
                   (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let* ((tag (gensym))
	       (var (gensym))
	       (annotated-cases (mapcar #'(lambda (case) (cons (gensym) case))
					cases)))
	  `(block ,tag
	     (let ((,var nil))
	       (declare (ignorable ,var))
	       (tagbody
                  (return-from ,tag
                    (handler-bind ,(mapcar #'(lambda (annotated-case)
                                               (list (cadr annotated-case)
                                                     `#'(lambda (temp)
                                                          (declare (ignorable temp))
                                                          ,@(if (caddr annotated-case)
                                                                `((setq ,var temp)))
                                                          (go ,(car annotated-case)))))
                                    annotated-cases)
                      ,form))
                  ,@(mapcan #'(lambda (annotated-case)
                                (list (car annotated-case)
                                      (let ((body (cdddr annotated-case)))
                                        `(return-from ,tag
                                           ,(if (caddr annotated-case)
                                                `(let ((,(caaddr annotated-case)
                                                         ,var))
                                                   ,@body)
                                                ;; We must allow declarations!
                                                `(locally ,@body))))))
                            annotated-cases))))))))

			   
;;; COERCE-TO-CONDITION
;;;  Internal routine used in ERROR, CERROR, WARN, and MP:INTERRUPT for parsing
;;;  the hairy argument conventions into a single argument that's directly usable
;;;  by all the other routines.

(defun coerce-to-condition (datum arguments default-type function-name)
  (typecase datum
    (condition
     (when arguments
       (cerror "Ignore the additional arguments."
               'SIMPLE-TYPE-ERROR
               :DATUM arguments
               :EXPECTED-TYPE 'NULL
               :FORMAT-CONTROL "You may not supply additional arguments ~
				     when giving ~S to ~S."
               :FORMAT-ARGUMENTS (list datum function-name)))
     datum)
    (symbol                  ;roughly, (subtypep datum 'CONDITION)
     (apply #'make-condition datum arguments))
    ((or string function)
     (make-condition default-type
                     :FORMAT-CONTROL datum
                     :FORMAT-ARGUMENTS arguments))
    (t
     (error 'SIMPLE-TYPE-ERROR
            :DATUM datum
            :EXPECTED-TYPE '(or condition symbol string function)
            :FORMAT-CONTROL "Bad argument to ~S: ~S"
            :FORMAT-ARGUMENTS (list function-name datum)))))

(defun break (&optional (format-control "Break") &rest format-arguments)
  "Enters a break loop.  The execution of the program can be resumed by typing
:CONTINUE at the break loop.  Type :HELP to see the break-loop commands list.
If FORMAT-STRING is non-NIL, it is used as the format string to be output to
*ERROR-OUTPUT* before entering the break loop.  ARGs are arguments to the
format string."
  (clasp-debug:with-truncated-stack ()
    (with-simple-restart (continue "Return from BREAK.")
      (let ((*debugger-hook* nil))
        (invoke-debugger
         (make-condition 'SIMPLE-CONDITION
                         :FORMAT-CONTROL format-control
                         :FORMAT-ARGUMENTS format-arguments)))))
  nil)

(defun breakstep (source)
  "Pause due to stepping or a breakpoint."
  (clasp-debug:with-truncated-stack ()
    (restart-case
        (let ((*debugger-hook* nil))
          (invoke-debugger
           (make-condition 'clasp-debug:step-form :source source)))
      ;; cc_breakstep interprets our return value as follows:
      ;; 0: continue without stepping
      ;; 1: step-into
      ;; 2: step-over
      ;; anything else: bug error
      (continue ()
        :report "Resume normal, unstepped execution."
        0)
      (clasp-debug:step-into ()
        :report "Step into call."
        1)
      (clasp-debug:step-over ()
        :report "Step over call."
        2))))

(defun warn (datum &rest arguments)
  "Args: (format-string &rest args)
Formats FORMAT-STRING and ARGs to *ERROR-OUTPUT* as a warning message.  Enters
a break level if the value of *BREAK-ON-WARNINGS* is non-NIL.  Otherwise,
returns with NIL."
  (let ((condition
	  (coerce-to-condition datum arguments 'SIMPLE-WARNING 'WARN)))
    (check-type condition warning "a warning condition")
    ;; FIXME? We could use %signal, but then with-condition-restarts wouldn't
    ;; happen correctly.
    (restart-case (signal condition)
      (muffle-warning ()
	  :REPORT "Skip warning."
	(return-from warn nil)))
    (format *error-output* "~&;;; Warning: ~A~%" condition)
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALL CONDITIONS
;;;

(define-condition warning () ())

(define-condition serious-condition () ())

(define-condition error (serious-condition) ())

(define-condition simple-condition ()
  ((format-control :INITARG :FORMAT-CONTROL :INITFORM ""
		   :reader simple-condition-format-control)
   (format-arguments :INITARG :FORMAT-ARGUMENTS :INITFORM NIL
		     :reader simple-condition-format-arguments))
  (:REPORT
   (lambda (condition stream)
     (handler-case
         (apply #'format stream
                (simple-condition-format-control condition)
                (simple-condition-format-arguments condition))
       (format-error (p)
         (declare (ignore p))
         (format stream "~%#<Error while printing condition>~%"))))))

(defmethod simple-condition-format-control (instance)
  (error 'type-error :datum instance :expected-type 'simple-condition))

(defmethod simple-condition-format-arguments (instance)
  (error 'type-error :datum instance :expected-type 'simple-condition))

(define-condition simple-warning (simple-condition warning) ())

(define-condition style-warning (warning) ())

(define-condition simple-style-warning (style-warning simple-condition) ())

(define-condition simple-error (simple-condition error) ())

(define-condition storage-condition (serious-condition) ())

(define-condition ext:segmentation-violation (storage-condition)
  ((address :initarg :address :reader memory-condition-address))
  (:REPORT
   (lambda (condition stream)
     (format stream "Segmentation fault. Attempted to access restricted memory address #x~x.

This is due either to a problem in foreign code (e.g., C++), or a bug in Clasp itself."
             (memory-condition-address condition)))))

;; Called by signal handlers in gctools/interrupt.cc.
(defun ext:segmentation-violation (address)
  (error 'ext:segmentation-violation :address address))

(define-condition ext:stack-overflow (storage-condition)
  ((size :initarg :size :initform 0 :reader ext:stack-overflow-size)
   (type :initarg :type :initform nil :reader ext:stack-overflow-type))
  (:REPORT
   (lambda (condition stream)
     (let* ((type (ext:stack-overflow-type condition))
	    (size (ext:stack-overflow-size condition)))
       (if size
	   (format stream "~A overflow at size ~D. Stack can probably be resized."
		   type size)
	   (format stream "~A stack overflow. Stack cannot grow any further. Either exit
or return to an outer frame, undoing all the function calls so far."
		   type))))))

(define-condition ext:storage-exhausted (storage-condition) ()
  (:REPORT "Memory limit reached. Please jump to an outer pointer, quit program and enlarge the
memory limits before executing the program again."))

(define-condition ext:bus-error (error)
  ((address :initarg :address :reader memory-condition-address))
  (:report
   (lambda (condition stream)
     (format stream "Bus error. Attempted to access invalid memory address #x~x.

This is due to either a problem in foreign code (e.g., C++), or a bug in Clasp itself."
             (memory-condition-address condition)))))
(defun ext:bus-error (address) (error 'ext:bus-error :address address))

(define-condition type-error (error)
  ((datum :INITARG :DATUM :READER type-error-datum)
   (expected-type :INITARG :EXPECTED-TYPE :READER type-error-expected-type))
  (:REPORT
   (lambda (condition stream)
     (format stream "~S is not of type ~S."
	     (type-error-datum condition)
	     (type-error-expected-type condition)))))

(defmethod type-error-datum (instance)
  (error 'type-error :datum instance :expected-type 'type-error))

(defmethod type-error-expected-type (instance)
  (error 'type-error :datum instance :expected-type 'type-error))

(define-condition out-of-bounds (type-error)
  ;; the type-error DATUM is the index, and its EXPECTED-TYPE is the range.
  ;; This is the sequence and/or array.
  ;; We don't generally display it because it could be huge,
  ;; but it might be nice to have.
  ((object :INITARG :object :READER out-of-bounds-object)))

;; for row-major-aref
(define-condition row-major-out-of-bounds (out-of-bounds)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Row-major array index ~d is out of bounds ~s."
             (type-error-datum condition)
             (type-error-expected-type condition)))))

;; for aref
(define-condition array-out-of-bounds (out-of-bounds)
  ((axis :initarg :axis :reader out-of-bounds-axis))
  (:REPORT
   (lambda (condition stream)
     (format stream "Array index ~d is out of bounds ~s on axis ~d."
             (type-error-datum condition)
             (type-error-expected-type condition)
             (out-of-bounds-axis condition)))))

;; for elt
(define-condition sequence-out-of-bounds (out-of-bounds)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Sequence index ~d is out of bounds ~s."
             (type-error-datum condition)
             (type-error-expected-type condition)))))

(define-condition simple-type-error (simple-condition type-error) ())

(define-condition case-failure (type-error)
  ((name :INITARG :NAME :READER case-failure-name)
   (possibilities :INITARG :POSSIBILITIES :READER case-failure-possibilities))
  (:REPORT
   (lambda (condition stream)
     (format stream "~S fell through ~S expression.~%Wanted one of ~{~s~^ ~}."
	     (type-error-datum condition)
	     (case-failure-name condition)
	     (case-failure-possibilities condition)))))

(define-condition clos:no-applicable-method-error (error)
  ((generic-function :initarg :generic-function
                     :reader no-applicable-method-generic-function)
   (arguments :initarg :arguments :reader no-applicable-method-arguments))
  (:report
   (lambda (condition stream)
     (format stream "No applicable method for ~S with ~
                  ~:[no arguments.~;arguments~%~t~:*(~{~S~^ ~})~]"
             (clos:generic-function-name
              (no-applicable-method-generic-function condition))
             (no-applicable-method-arguments condition)))))

(define-condition program-error (error) ())

(define-condition core:simple-program-error (simple-condition program-error) ())

(defun core:simple-program-error (format-control &rest format-arguments)
  (error 'core:simple-program-error
         :format-control format-control :format-arguments format-arguments))

(define-condition control-error (error) ())

(define-condition core:simple-control-error (simple-condition control-error) ())

;; FIXME: We could probably try to at least include the name of the block or
;; tag that was supposed to be returned to.
(define-condition core:out-of-extent-unwind (control-error)
  ()
  (:report "Attempted to return or go to an expired block or tagbody tag."))

(define-condition core:no-catch-tag (control-error)
  ((%tag :initarg :tag :reader no-catch-tag-tag))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to throw to unestablished catch tag ~a"
             (no-catch-tag-tag condition)))))

(define-condition invalid-restart (control-error)
  ((%restart :initarg :restart :reader invalid-restart-restart))
  (:report
   (lambda (condition stream)
     (format stream "~s's extent has been exited and it can no longer be invoked."
             (invalid-restart-restart condition)))))

(define-condition abort-returned (control-error)
  ((%abort-restart :initarg :restart :reader abort-restart))
  (:report
   (lambda (condition stream)
     (format stream "~s restart ~s did not transfer control.
(~2:*~s restarts must transfer control; the function ~:*~s is defined to never return.)"
             'abort (abort-restart condition)))))

(define-condition muffle-warning-returned (control-error)
  ((%mw-restart :initarg :restart :reader muffle-warning-restart))
  (:report
   (lambda (condition stream)
     (format stream "~s restart ~s did not transfer control.
(~2:*~s restarts must transfer control; the function ~:*~s is defined to never return."
             'muffle-warning (muffle-warning-restart condition)))))

#+threads
(define-condition mp:not-atomic (error)
  ((place :initarg :place :reader mp:not-atomic-place))
  (:report
   (lambda (condition stream)
     (format stream "Don't know how to atomically access the place ~a"
             (mp:not-atomic-place condition)))))

#+threads
(define-condition mp:process-error (error)
  ((process :initarg :process :reader mp:process-error-process))
  (:documentation "Superclass of errors relating to processes."))

#+threads
(define-condition mp:process-join-error (mp:process-error)
  ((original-condition :initarg :original-condition :initform nil
                       :reader mp:process-join-error-original-condition)
   (aborted :initarg :aborted :initform nil
            :reader mp:process-join-error-aborted))
  (:report
   (lambda (condition stream)
     (format stream "Failed to join process: Process ~s ~:[not started~;aborted~]~:[.~; ~
due to error:~%  ~:*~a~]"
             (mp:process-error-process condition)
             (mp:process-join-error-aborted condition)
             (mp:process-join-error-original-condition condition))))
  (:documentation "PROCESS-JOIN signals a condition of this type when the thread
being joined ended abnormally or was not started."))

#+threads
(progn
  ;; Somewhat KLUDGE-y way to add an ABORT restart to every new thread.
  ;; FIXME: Actually pass the damn condition to abort-thread.
  ;; The normal ABORT restart doesn't work for this since it takes no
  ;; arguments. Annoying.
  (mp:push-default-special-binding
   '*restart-clusters*
   '(list (list (make-restart
                  :name 'abort
                  :function #'mp:abort-process
                  :report-function (lambda (stream)
                                     (format stream "Abort the process (~s)"
                                             mp:*current-process*)))))))

(define-condition stream-error (error)
  ((stream :initarg :stream :reader stream-error-stream)))

(defmethod stream-error-stream (instance)
  (error 'type-error :datum instance :expected-type 'stream-error))

(define-condition core:simple-stream-error (simple-condition stream-error) ())

(define-condition core:closed-stream (core:simple-stream-error)
  ())

(define-condition end-of-file (stream-error)
  ()
  (:REPORT (lambda (condition stream)
	     (format stream "Unexpected end of file on ~S."
		     (stream-error-stream condition)))))

(define-condition file-error (error)
  ((pathname :INITARG :PATHNAME :READER file-error-pathname))
  (:REPORT (lambda (condition stream)
	     (format stream "Filesystem error with pathname ~S.~%Either
 1) the file does not exist, or
 2) we are not allowed to access the file, or
 3) the pathname points to a broken symbolic link."
		     (file-error-pathname condition)))))

(defmethod file-error-pathname (instance)
  (error 'type-error :datum instance :expected-type 'file-error))

(define-condition core:simple-file-error (simple-condition file-error) ())

(define-condition core:file-does-not-exist (file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "The file ~s does not exist."
                     (file-error-pathname condition)))))

(define-condition core:file-exists (file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "The file ~s already exists."
                     (file-error-pathname condition)))))

(define-condition package-error (error)
  ((package :INITARG :PACKAGE :READER package-error-package))
  (:report (lambda (condition stream)
             (format stream "Package error on package ~S" (package-error-package condition)))))

(defmethod package-error-package (instance)
  (error 'type-error :datum instance :expected-type 'package-error))

(define-condition core:simple-package-error (simple-condition package-error) ())

(define-condition ext:name-conflict (package-error)
  ((%operation :initarg :operation :reader name-conflict-operation)
   (%troublemaker :initarg :troublemaker :reader name-conflict-troublemaker
                  :type symbol)
   (%candidates :initarg :candidates :reader ext:name-conflict-candidates
                :type list))
  (:report (lambda (condition stream)
             (format stream "~s ~s causes name-conflicts in ~s between the following symbols:~%~s"
                     (name-conflict-operation condition)
                     (name-conflict-troublemaker condition)
                     (package-error-package condition)
                     (ext:name-conflict-candidates condition)))))

(defun resolve-conflict-interactive (package candidates)
  ;; Cribbed from SBCL's NAME-CONFLICT function.
  (let* ((len (length candidates))
         (nlen (length (write-to-string len :base 10)))
         (*print-pretty* t))
    (format *query-io*
            "~&~@<Select a symbol to be made accessible in package ~a:~2i~@:_~{~{~v,' d. ~s~}~@:_~}~@:>"
            (package-name package)
            (loop for s in candidates for i upfrom 1 collect (list nlen i s)))
    (loop
      (format *query-io* "~&Enter an integer (between 1 and ~d): " len)
      (finish-output *query-io*)
      (let ((i (parse-integer (read-line *query-io*) :junk-allowed t)))
        (when (and i (<= 1 i len))
          (return (list (nth (1- i) candidates))))))))

(defun check-chosen-symbol (chosen-symbol candidates)
  (assert (member chosen-symbol candidates) (chosen-symbol)
          "~s is not one of the symbols that can resolve the conflict.
The conflict resolver must be one of ~s" chosen-symbol candidates))

(defun core:import-name-conflict (package existing to-import)
  (let ((candidates (list existing to-import)))
    (restart-case
        (error 'ext:name-conflict
               :package package :operation 'import
               :troublemaker to-import :candidates candidates)
      (take-new ()
        :report (lambda (s)
                  (format s "Shadowing-import ~s, uninterning ~s."
                          to-import existing))
        (shadowing-import to-import package))
      (keep-old ()
        :report (lambda (s)
                  (format s "Don't import ~s, keeping ~s."
                          to-import existing)))
      (ext:resolve-conflict (chosen-symbol)
        :interactive (lambda () (resolve-conflict-interactive
                                 package candidates))
        :report "Resolve conflict."
        (check-chosen-symbol chosen-symbol candidates)
        (cond ((eq chosen-symbol existing)) ; don't import
              ((eq chosen-symbol to-import)
               (shadowing-import to-import package)))))))

;;; Shared logic for USE-PACKAGE and EXPORT conflicts
(defun accessibility-conflict (operation troublemaker new package)
  (let ((name (symbol-name new)))
    (multiple-value-bind (old status) (find-symbol name package)
      (assert (and old (not (eq old new))))
      (let ((candidates (list new old)))
        (ecase status
          ((:inherited)
           (restart-case
               (error 'ext:name-conflict
                      :package package :operation operation
                      :troublemaker troublemaker :candidates candidates)
             (keep-old ()
               :report (lambda (s)
                         (format s "Keep ~s accessible in ~a by importing and shadowing it."
                                 old package))
               (shadowing-import (list old) package))
             (take-new ()
               :report (lambda (s)
                         (format s "Make ~s accessible in ~a by importing and shadowing it."
                                 new package))
               (shadowing-import (list new) package))
             (ext:resolve-conflict (chosen-symbol)
               :interactive (lambda () (resolve-conflict-interactive
                                        package candidates))
               :report "Resolve conflict."
               (check-chosen-symbol chosen-symbol candidates)
               (shadowing-import (list chosen-symbol) package))))
          ((:internal :external)
           (restart-case
               (error 'ext:name-conflict
                      :package package :operation operation
                      :troublemaker troublemaker :candidates candidates)
             (keep-old ()
               :report (lambda (s)
                         (format s "Keep ~s accessible in ~a by shadowing it."
                                 old package))
               (shadow (list name) package))
             (take-new ()
               :report (lambda (s)
                         (format s "Make ~s accessible in ~a by uninterning the old symbol."
                                 new package))
               (unintern old package))
             (ext:resolve-conflict (chosen-symbol)
               :interactive (lambda () (resolve-conflict-interactive
                                        package candidates))
               :report "Resolve conflict."
               (check-chosen-symbol chosen-symbol candidates)
                 (cond ((eq chosen-symbol old) (shadow (list name) package))
                       ((eq chosen-symbol new) (unintern old package)))))))))))

(defun core:export-name-conflict (to-export problematic)
  ;; CLHS export says
  ;; "aborting from a name-conflict error caused by export of one of symbols
  ;;  does not leave that symbol accessible to some packages and inaccessible
  ;;  to others; with respect to each of symbols processed, export behaves as
  ;;  if it were as an atomic operation."
  ;; However, a programmer can do arbitrary things while handling a condition,
  ;; so we can't necessarily undo all conflict resolution.
  ;; So what we interpret this to mean is that the symbol is certainly
  ;; exported or certainly not exported, but it would be possible for it to
  ;; have been made accessible in some packages by conflict resolution
  ;; before a later abort.
  (dolist (package problematic)
    ;; The actual name conflict is in the using package,
    ;; not the package doing the export.
    (accessibility-conflict 'export to-export to-export package)))

(defun core:use-package-name-conflict (package used conflicts)
  (dolist (sym conflicts)
    (let* ((name (symbol-name sym))
           (new (find-symbol name used)))
      (accessibility-conflict 'use-package used new package))))

(defun core:unintern-name-conflict (package symbol candidates)
  (restart-case
      (error 'ext:name-conflict :package package :operation 'unintern
                                :troublemaker symbol :candidates candidates)
    (ext:resolve-conflict (chosen-symbol)
      :report "Resolve conflict."
      :interactive (lambda () (resolve-conflict-interactive package candidates))
      ;; Actual restart body
      (check-chosen-symbol chosen-symbol candidates)
      (shadowing-import (list chosen-symbol) package))))

(define-condition core:package-lock-violation (package-error simple-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Lock on package ~a violated when ~?~:@>"
             (package-error-package condition)
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)))))

(defun core:package-lock-violation (package
                                    format-control &rest format-arguments)
  (error 'core:package-lock-violation :package package
         :format-control format-control :format-arguments format-arguments))

(define-condition cell-error (error)
  ((name :INITARG :NAME :READER cell-error-name)))

(defmethod cell-error-name (instance)
  (error 'type-error :datum instance :expected-type 'cell-error))

(define-condition unbound-variable (cell-error)
  ()
  (:REPORT (lambda (condition stream)
	     (format stream "The variable ~S is unbound."
		     (cell-error-name condition)))))
  
(define-condition unbound-slot (cell-error)
  ((instance :INITARG :INSTANCE :READER unbound-slot-instance))
  (:REPORT (lambda (condition stream)
             (handler-case
	         (format stream "~@<The slot ~S in the object ~S is unbound.~@:>"
		         (cell-error-name condition)
		         (unbound-slot-instance condition))
               (serious-condition ()
                 (format stream "~@<The slot ~s is unbound in an instance of ~s.~@:>"
                         (cell-error-name condition)
                         (type-of (unbound-slot-instance condition))))))))

(defmethod unbound-slot-instance (instance)
  (error 'type-error :datum instance :expected-type 'unbound-slot))

(define-condition undefined-function (cell-error)
  ()
  (:REPORT (lambda (condition stream)
	     (format stream "The function ~S is undefined."
		     (cell-error-name condition)))))

(define-condition ext:undefined-class (cell-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Could not find the class ~s."
                     (cell-error-name condition)))))

(define-condition arithmetic-error (error)
  (;; NOTE/FIXME: Sometimes we have the OPERATION be NIL - if we can't determine what
   ;; it was, as happens with floating point traps sometimes (currently all the time).
   ;; This is probably nonconforming.
   (operation :initform nil :INITARG :OPERATION :READER arithmetic-error-operation)
   (operands :initform nil :INITARG :OPERANDS :READER arithmetic-error-operands)))

(defmethod arithmetic-error-operation (instance)
  (error 'type-error :datum instance :expected-type 'arithmetic-error))

(defmethod arithmetic-error-operands (instance)
  (error 'type-error :datum instance :expected-type 'arithmetic-error))

(define-condition division-by-zero (arithmetic-error) ())

(define-condition floating-point-overflow (arithmetic-error) ())

(define-condition floating-point-underflow (arithmetic-error) ())

(define-condition floating-point-inexact (arithmetic-error) ())

(define-condition floating-point-invalid-operation (arithmetic-error) ())

(define-condition core:do-not-funcall-special-operator (undefined-function)
  ((operator :initarg :operator :reader cell-error-name))
  (:report (lambda (condition stream)
             (format stream "Cannot call special operator as function: ~s"
                     (cell-error-name condition)))))

(define-condition core:wrong-number-of-arguments (program-error)
  (;; may be NIL if this is called from the interpreter and we don't know anything
   ;; (KLUDGE, FIXME?)
   (called-function :initform nil :initarg :called-function :reader called-function)
   (given-nargs :initarg :given-nargs :reader given-nargs)
   ;; also may be NIL, same reason (KLUDGE, FIXME?)
   (min-nargs :initarg :min-nargs :reader min-nargs :initform nil)
   ;; may be NIL to indicate no maximum.
   (max-nargs :initarg :max-nargs :reader max-nargs :initform nil))
  (:report (lambda (condition stream)
             (let* ((min (min-nargs condition))
                    (max (max-nargs condition))
                    (function (called-function condition))
                    (name (and function (core:function-name function)))
                    (dname (if (eq name 'cl:lambda) "anonymous function" name)))
               (format stream "~@[Calling ~a - ~]Got ~d arguments, but expected ~@?"
                       dname (given-nargs condition)
                       (cond ((null max)  "at least ~d")
                             ((null min)  "at most ~*~d")
                             ;; I think "exactly 0" is better than "at most 0", thus duplication
                             ((= min max) "exactly ~d")
                             ((zerop min) "at most ~*~d")
                             (t           "between ~d and ~d"))
                       min max)))))

(define-condition destructure-wrong-number-of-arguments (program-error)
  ((%macro-name :initarg :macro-name :reader macro-name)
   (%lambda-list :initarg :lambda-list :reader lambda-list)
   (%arguments :initarg :arguments :reader arguments)
   (%problem :initarg :problem :reader problem))
  (:report (lambda (condition stream)
             (let ((name (macro-name condition)))
               (if name
                   (format stream "Error while parsing arguments to ~a:
~2t~a in~%~4t~a~%~2tto satisfy lambda list~%~4t~a"
                           name
                           (ecase (problem condition)
                             ((:too-many) "Too many arguments")
                             ((:too-few) "Too few arguments"))
                           (arguments condition) (lambda-list condition))
                   (format stream "~a in~%~2t~a~%to satisfy lambda list~%~2t~a"
                           (ecase (problem condition)
                             ((:too-many) "Too many arguments")
                             ((:too-few) "Too few arguments"))
                           (arguments condition) (lambda-list condition)))))))

(define-condition core:odd-keywords (program-error)
  ((%called-function :initarg :called-function :reader called-function))
  (:report (lambda (condition stream)
             (format stream "Odd number of keyword arguments~:[~; for ~s~]."
                     (called-function condition)
                     (core:function-name (called-function condition))))))

(define-condition core:unrecognized-keyword-argument-error (program-error)
  ((called-function :initarg :called-function :reader called-function :initform nil)
   (unrecognized-keywords :initarg :unrecognized-keywords :reader unrecognized-keywords))
  (:report (lambda (condition stream)
             (format stream "Unrecognized keyword arguments ~S~:[~; for ~S~]."
                     (unrecognized-keywords condition)
                     (called-function condition)
                     (core:function-name (called-function condition))))))

(define-condition print-not-readable (error)
  ((object :INITARG :OBJECT :READER print-not-readable-object))
  (:REPORT (lambda (condition stream)
	     (format stream "Cannot print object ~A of class ~A readably."
		     (print-not-readable-object condition) (class-name (class-of (print-not-readable-object condition)))))))

(defmethod print-not-readable-object (instance)
  (error 'type-error :datum instance :expected-type 'print-not-readable))

(define-condition parse-error (error) ())

(define-condition core:simple-parse-error (simple-condition parse-error) ())

(define-condition reader-error (parse-error stream-error) ())

(define-condition core:simple-reader-error (simple-condition reader-error) ())

(defun core:simple-reader-error (stream format-control &rest format-arguments)
  (error 'core:simple-reader-error
         :stream stream :format-control format-control
         :format-arguments format-arguments))




(defun signal-simple-error (condition-type continue-message format-control format-args
			    &rest args)
  (if continue-message
      (apply #'cerror continue-message condition-type :format-control format-control
                                                      :format-arguments format-args args)
      (apply #'error condition-type :format-control format-control
                                    :format-arguments format-args args)))


(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

(defun abort (&optional c)
  (let ((restart (coerce-restart-designator 'abort c)))
    (invoke-restart restart)
    (error 'abort-returned :restart restart)))

(defun continue (&optional c)
  (let ((restart (find-restart 'CONTINUE c)))
    (and restart (invoke-restart restart))))

(defun muffle-warning (&optional c)
  (let ((restart (coerce-restart-designator 'muffle-warning c)))
    (invoke-restart restart)
    (error 'muffle-warning-returned :restart restart)))

(defun store-value (value &optional c)
  (let ((restart (find-restart 'STORE-VALUE c)))
    (and restart (invoke-restart restart value))))

(defun use-value (value &optional c)
  (let ((restart (find-restart 'USE-VALUE c)))
    (and restart (invoke-restart restart value))))

(defun assert-report (names stream)
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~P for ~{~S~^, ~}."
	      (length names) names)
      (format stream ".")))

(defun assert-prompt (name value)
  (if (y-or-n-p "The old value of ~S is ~S.~
		~%Do you want to supply a new value? "
                name value)
      (flet ((read-it () (eval (read *query-io*))))
        (format *query-io* "~&Type a form to be evaluated:~%")
        (if (symbolp name) ;Help user debug lexical variables
            (progv (list name) (list value) (read-it))
            (read-it)))
      value))

(define-condition ext:assert-error (simple-error)
  ((%test-form :initarg :test :reader test-form))
  (:report (lambda (condition stream)
             (format stream "The assertion ~s failed" (test-form condition)))))

(defun assert-failure (test-form place-names values datum &rest arguments)
  (let ((condition (if datum
                       (coerce-to-condition datum arguments
                                            'simple-error 'assert)
                       ;; issue #499
                       (make-condition 'ext:assert-error :test test-form))))
    (restart-case (error condition)
      (continue ()
        :REPORT (lambda (stream) (assert-report place-names stream))
	(values-list (loop for place-name in place-names
			   for value in values
			   collect (assert-prompt place-name value)))))))

(define-condition step-condition () ())

(define-condition clasp-debug:step-form (step-condition)
  ((%source :initarg :source :reader source))
  (:report (lambda (condition stream)
             (format stream "Evaluating form: ~s" (source condition)))))

;;; ----------------------------------------------------------------------
;;; Unicode, initially forgotten in clasp

#+unicode
(define-condition ext:character-coding-error (error)
  ((external-format :initarg :external-format :reader character-coding-error-external-format)))

#+unicode
(define-condition ext:character-encoding-error (ext:character-coding-error)
  ((code :initarg :code :reader character-encoding-error-code)))

#+unicode
(define-condition ext:character-decoding-error (ext:character-coding-error)
  ((octets :initarg :octets :reader character-decoding-error-octets)))

#+unicode
(define-condition ext:stream-encoding-error (stream-error ext:character-encoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (code (character-encoding-error-code c)))
       (format s "~@<encoding error on stream ~S (~S ~S): ~2I~_~
                  the character with code ~D cannot be encoded.~@:>"
               stream ':external-format
               (character-coding-error-external-format c)
               code)))))

#+unicode
(define-condition ext:stream-decoding-error (stream-error ext:character-decoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (octets (character-decoding-error-octets c)))
       (format s "~@<decoding error on stream ~S (~S ~S): ~2I~_~
                  the octet sequence ~S cannot be decoded.~@:>"
               stream ':external-format
               (character-coding-error-external-format c)
               octets)))))
#+unicode
(defun ext:encoding-error (stream external-format code)
  (restart-case (error 'ext:stream-encoding-error
                       :stream stream
                       :external-format external-format
                       :code code)
    (continue ()
      :report "Ignore character"
      nil)
    (use-value (c)
      :report "Store a different character code."
      (if (characterp c) c (code-char c)))))

#+unicode
(defun ext:decoding-error (stream external-format octets)
  (restart-case (error 'ext:stream-decoding-error
                       :stream stream
                       :external-format external-format
                       :octets octets)
    (continue ()
      :report "Read next character"
      nil)
    (use-value (c)
      :report "Replace the bogus sequence with a character"
      (if (characterp c) c (code-char c)))))

;;; ----------------------------------------------------------------------
;;;
;;; Interrupts
;;;

(define-condition mp:interrupt ()
  ()
  (:documentation "Abstract base class for interrupt conditions. All conditions signaled by SIGNAL-PENDING-INTERRUPTS are of this type."))

(defgeneric mp:service-interrupt (mp:interrupt)
  (:documentation "Execute the action represented by an interrupt. Not intended to be called by programmers."))

(define-condition ext:interactive-interrupt (mp:interrupt)
  ()
  (:report "Console interrupt."))

;; it's in EXT for backward compatibility. Eventually it should just be MP.
(import 'ext:interactive-interrupt "MP")
(export 'ext:interactive-interrupt "MP")

(define-condition mp:simple-interrupt (simple-condition mp:interrupt) ())
(define-condition mp:simple-interactive-interrupt (mp:simple-interrupt
                                                   ext:interactive-interrupt)
  ())

(define-condition mp:cancellation-interrupt (mp:interrupt) ()
  (:documentation "A request to a thread to abort computation."))

(define-condition mp:call-interrupt (mp:interrupt)
  ((%function :initarg :function :reader mp:call-interrupt-function
              :type function))
  (:documentation "A request to call a thunk."))

(define-condition mp:suspension-interrupt (mp:interrupt) ()
  (:documentation "A request to pause computation."))

;; terminate the program. I'm making it a subset of cancellation because
;; it cancels the particular thread as well obviously, and because if you
;; want to block one kind of termination you might want to block the other.
;; Not sure whether to export this specifically.
(define-condition termination-interrupt (mp:cancellation-interrupt) ()
  (:documentation "A request to terminate Clasp entirely."))

;; Do nothing. Good for ignorable signals.
(define-condition nop-interrupt (mp:interrupt) ()
  (:documentation "An interrput with no further behavior if unhandled."))

;; Interrupts that are also errors - they go into the debugger as if they had
;; been called by ERROR instead of INTERRUPT.
(define-condition error-interrupt (error mp:interrupt) ())

;;; POSIX signals
(macrolet ((defposix (name super string)
             (let ((cname (find-symbol (symbol-name name) "CORE")))
               (when cname
                 `(define-condition ,cname (,super)
                    ()
                    (:report ,string))))))
  ;; Many of these are never actually signaled (e.g. FPE) but are included
  ;; for completeness. Possibly they should be signaled in unusual situations,
  ;; e.g. being caused by a manual kill(2) or raise(3).
  ;; Some are obviously impossible, like sigkill and sigstop.
  ;; Also, the below superclasses are pretty ad-hoc - feel free to change them
  ;; if you have a good reason.
  (defposix #:sigabrt termination-interrupt "Abort signal.")
  (defposix #:sigalrm nop-interrupt "Alarm signal.")
  (defposix #:sigbus error-interrupt "Bus error.")
  (defposix #:sigchld nop-interrupt "Child process stopped or terminated.")
  (defposix #:sigcont nop-interrupt "Continue signal.")
  (defposix #:sigemt nop-interrupt "Emulator trap.")
  (defposix #:sigfpe error-interrupt "Floating point exception.")
  (defposix #:sighup termination-interrupt "Hangup.")
  (defposix #:sigill error-interrupt "Illegal instruction.")
  (defposix #:sigint ext:interactive-interrupt "Interruption signal.")
  (defposix #:sigio nop-interrupt "I/O now ready.")
  (defposix #:sigkill termination-interrupt "Kill signal.")
  (defposix #:sigpipe error-interrupt "Broken pipe.")
  (defposix #:sigpoll nop-interrupt "Polled event occurred.")
  (defposix #:sigprof nop-interrupt "Profiling signal.")
  (defposix #:sigpwr nop-interrupt "Power failure.")
  (defposix #:sigquit termination-interrupt "Quit signal.")
  (defposix #:sigsegv error-interrupt "Segmentation fault.")
  (defposix #:sigstop nop-interrupt "Stop signal.")
  (defposix #:sigtstp nop-interrupt "Stop typed.")
  (defposix #:sigsys error-interrupt "Bad syscall.")
  (defposix #:sigterm termination-interrupt "Termination signal.")
  (defposix #:sigtrap nop-interrupt "Trace/breakpoint trap.")
  (defposix #:sigttin nop-interrupt "Terminal input for background process.")
  (defposix #:sigttou nop-interrupt "Terminal output for background process.")
  (defposix #:sigurg error-interrupt "Urgent condition on socket.")
  (defposix #:sigusr1 nop-interrupt "User signal 1.")
  (defposix #:sigusr2 nop-interrupt "User signal 2.")
  (defposix #:sigvtalrm nop-interrupt "Virtual alarm.")
  (defposix #:sigxcpu nop-interrupt "CPU time limit exceeded.")
  (defposix #:sigxfsz nop-interrupt "File size limit exceeded.")
  (defposix #:sigwinch nop-interrupt "Window resize signal."))

(defun designated-interrupt (datum datump arguments)
  (if datump
      (coerce-to-condition datum arguments
                           'mp:simple-interactive-interrupt
                           'mp:interrupt)
      (make-condition 'mp:simple-interactive-interrupt
                      "Interactive interrupt.")))

(defun mp:interrupt (process &optional (datum nil datump) &rest arguments)
  "Send the given PROCESS an interrupt request.
If the process is alive/running, the interrupt is placed on the PROCESS's pending interrupt list. If the process is not yet started, an error is signaled. If the process has already exited, the interrupt is discarded.
DATUM and ARGUMENTS are designators for an interrupt of default type SIMPLE-INTERACTIVE-INTERRUPT. If DATUM is not specified, a SIMPLE-INTERACTIVE-INTERRUPT with default message is created.
The interrupt will be SIGNALed in the process at some time when it calls SIGNAL-PENDING-INTERRUPTS and interrupts are not blocked (by WITHOUT-INTERRUPTS). This can happen implicitly at implementation-defined times, or when the process is blocking on grabbing a lock, waiting on a condition variable, sleeping, or waiting for input.
Interrupts are implicitly blocked while signaling an interrupt, and while unwind-protect cleanups are executed. They can be reenabled temporarily with WITH-INTERRUPTS."
  (check-type process mp:process)
  (let ((condition (designated-interrupt datum datump arguments)))
    (check-type condition mp:interrupt "an interrupt")
    (mp:enqueue-interrupt process condition)))

(defun mp:signal-pending-interrupts ()
  "Signal all currently pending interrupts for this process. Note that even if this call returns, the pending interrupt list may not be empty, because the process could have been interrupted again."
  (core:check-pending-interrupts))

;;; Internal, used by interrupt machinery. Return value ignored.
(defun mp:signal-interrupt (interrupt)
  (mp:without-interrupts
    (restart-case (signal interrupt)
      (continue ()
        :report "Ignore the interruption and proceed from where things left off."
        (return-from mp:signal-interrupt))))
  (mp:service-interrupt interrupt))

(defun mp:raise (&optional (datum nil datump) &rest arguments)
  "Signal a designated interrupt right now, i.e. synchronously. As this does not go through the SIGNAL-PENDING-INTERRUPTS function, this will not actually add the interrupt to the process's pending interrupts, and is not affected by interrupts being blocked."
  ;; name analogous to raise(3)
  (mp:signal-interrupt (designated-interrupt datum datump arguments)))

;;; for backward compatibility (e.g. bordeaux)
(defun mp:interrupt-process (process function)
  (check-type process mp:process)
  (check-type function function)
  (mp:interrupt process 'mp:call-interrupt :function function))

(defun mp:process-kill (process)
  ;; FIXME: This function should maybe be the more chaotic SIGKILL version,
  ;; while cancel-thread (cancel-process?) is the nicer interrupt.
  (mp:interrupt process 'mp:cancellation-interrupt))

(defun mp:process-cancel (process) ; the nicer version.
  (mp:interrupt process 'mp:cancellation-interrupt))

(defun mp:process-suspend (process)
  (mp:interrupt process 'mp:suspension-interrupt))

(defmethod mp:service-interrupt ((i ext:interactive-interrupt))
  ;; kinda duplicates BREAK.
  (clasp-debug:with-truncated-stack ()
    (with-simple-restart (continue "Return from interactive interruption.")
      (let ((*debugger-hook* nil)) (invoke-debugger i)))))

;;; For an external signal, call this complicated function
;;; (defined in top.lisp) that lets the user pick a thread and stuff.
(defmethod mp:service-interrupt ((i core:sigint))
  (core:terminal-interrupt))

(defmethod mp:service-interrupt ((i error-interrupt))
  (clasp-debug:with-truncated-stack () (invoke-debugger i)))

(defmethod mp:service-interrupt ((i mp:cancellation-interrupt))
  (mp:abort-process i))
(defmethod mp:service-interrupt ((i termination-interrupt))
  (format *debug-io* "~&Terminating Clasp due to interrupt: ~a~%" i)
  (ext:quit 1))

(defmethod mp:service-interrupt ((i mp:call-interrupt))
  (funcall (mp:call-interrupt-function i)))

(defmethod mp:service-interrupt ((i mp:suspension-interrupt))
  (mp:suspend-loop))

(defmethod mp:service-interrupt ((i nop-interrupt)))

;; called from C++ (posix_signal_interrupt)
(defun mp:posix-interrupt (sig)
  (let* ((signals (load-time-value (core:signal-code-alist) t))
         (pair (rassoc sig signals)))
    (if pair
        (mp:raise (first pair))
        (mp:raise "Received POSIX signal: ~d" sig))))

;;; ----------------------------------------------------------------------
;;; ECL's interface to the toplevel and debugger

;;; This is a redefinition, clobbering core__universal_error_handler in lisp.cc.
(defun sys::universal-error-handler (continue-string datum args)
  "Args: (error-name continuable-p function-name
       continue-format-string error-format-string
       &rest args)
ECL specific.
Starts the error handler of ECL.
When an error is detected, ECL calls this function with the specified
arguments.  To change the error handler of ECL, redefine this function.
ERROR-NAME is the name of the error.  CONTINUABLE-P is T for a continuable
error and NIL for a fatal error.  FUNCTION-NAME is the name of the function
that caused the error.  CONTINUE-FORMAT-STRING and ERROR-FORMAT-STRING are the
format strings of the error message.  ARGS are the arguments to the format
bstrings."
  (clasp-debug:with-truncated-stack ()
    (let ((condition (coerce-to-condition datum args 'simple-error 'error)))
      (cond
        ((eq t continue-string)
                                        ; from CEerror; mostly allocation errors
         (with-simple-restart (ignore "Ignore the error, and try the operation again")
           (%signal condition)
           (invoke-debugger condition)))
        ((stringp continue-string)
         (with-simple-restart (continue "~?" continue-string args)
           (%signal condition)
           (invoke-debugger condition)))
        ((and continue-string (symbolp continue-string))
                                        ; from CEerror
         (with-simple-restart (accept "Accept the error, returning NIL")
           (multiple-value-bind (rv used-restart)
               (with-simple-restart (ignore "Ignore the error, and try the operation again")
                 (multiple-value-bind (rv used-restart)
                     (with-simple-restart (continue "Continue, using ~S" continue-string)
                       (%signal condition)
                       (invoke-debugger condition))
                   (if used-restart continue-string rv)))
             (if used-restart t rv))))
        (t
         (%signal condition)
         (invoke-debugger condition))))))

;;; Now that the condition system is up, define a few more things for
;;; the sake of the debugger

(in-package #:clasp-debug)

(defun safe-prin1 (object &optional output-stream-designator)
  "PRIN1 the OBJECT to the given stream (default *STANDARD-OUTPUT*).
Extra care is taken to ensure no errors are signaled. If the object cannot be printed, an unreadable representation is returned instead."
  (let ((string
          (handler-case
              ;; First just try it.
              (prin1-to-string object)
            (serious-condition ()
              (handler-case
                  ;; OK, print type.
                  ;; FIXME: Should print a pointer too but I don't actually
                  ;; know how to get that from Lisp.
                  (let ((type (type-of object)))
                    (concatenate 'string
                                 "#<error printing "
                                 (prin1-to-string type)
                                 ">"))
                (serious-condition ()
                  ;; Couldn't print the type. Give up entirely.
                  "#<error printing object>"))))))
    (write-string string output-stream-designator)))

(defun display-fname (fname &optional output-stream-designator)
  (if (stringp fname) ; C/C++ frame
      (write-string fname output-stream-designator)
      (safe-prin1 fname output-stream-designator)))

(defun prin1-frame-call (frame &optional output-stream-designator)
  "PRIN1 a representation of the given frame's call to the stream (default *STANDARD-OUTPUT*).
Extra care is taken to ensure no errors are signaled, using SAFE-PRIN1."
  (let ((fname (frame-function-name frame)))
    (multiple-value-bind (args availablep)
        (clasp-debug:frame-arguments frame)
      (cond (availablep
             (write-char #\( output-stream-designator)
             (display-fname fname output-stream-designator)
             (loop for arg in args
                   do (write-char #\Space output-stream-designator)
                      (safe-prin1 arg output-stream-designator))
             (write-char #\) output-stream-designator))
            (t (display-fname fname output-stream-designator)))))
  frame)

(defun princ-code-source-line (code-source-line &optional output-stream-designator)
  "Write a human-readable representation of the CODE-SOURCE-LINE to the stream."
  (let ((string
          (handler-case
              (format nil "~a:~d"
                      (code-source-line-pathname code-source-line)
                      (code-source-line-line-number code-source-line))
            (serious-condition () "error while printing code-source-line"))))
    (write-string string output-stream-designator)))

(defun print-stack (base &key (stream *standard-output*) count source-positions)
  "Write a representation of the stack beginning at BASE to STREAM.
If COUNT is provided and not NIL, at most COUNT frames are printed.
If SOURCE-POSITIONS is true, a description of the source position of each frame's call will be printed."
  (map-indexed-stack
   (lambda (frame i)
     (format stream "~&~4d: " i)
     (prin1-frame-call frame stream)
     (when source-positions
       (let ((fsp (frame-source-position frame)))
         (when fsp
           (fresh-line stream)
           (write-string "    |---> " stream)
           (princ-code-source-line fsp stream)))))
   base
   :count count)
  (fresh-line stream)
  (values))

(defun print-backtrace (&key (stream *standard-output*) count source-positions
                          (delimited t))
  "Write a current backtrace to STREAM.
If COUNT is provided and not NIL, at most COUNT frames are printed.
If SOURCE-POSITIONS is true, a description of the source position of each frame's call will be printed.
Other keyword arguments are passed to WITH-STACK."
  (with-stack (stack :delimited delimited)
    (print-stack stack :stream stream :count count
                 :source-positions source-positions)))
